from typing import List, Dict, Any, Optional, Callable
import re

# ---------------------------------------------------------------------------
# Type Predicates

def is_string(s):
    return isinstance(s, str)

def is_integer(x):
    return isinstance(x, int)

def is_positive_int(i):
    return is_integer(i) and i >= 0

def is_function(f):
    return callable(f)

def is_object(o):
    return isinstance(o, object)

def is_array(x):
    return isinstance(x, list)

# ---------------------------------------------------------------------------
# Language Helpers
# NOTE: wrapping these functions make it easier to port between languages

# returns the length of a String
def str_len(s):
    return len(s)

# returns the length of an Array
def array_size(a):
    return len(a)

# returns the last item in an Array
# returns None if the Array has no items
def array_last(a):
    s = array_size(a)
    if s == 0:
        return None
    else:
        return a[dec(s)]

def drop_last(arr):
    return arr[:dec(array_size(arr))]

# given an array of objects, returns a new array of the values at obj[key]
def array_pluck(arr, key):
    arr2 = []
    size = array_size(arr)
    idx = 0
    while idx < size:
        itm = arr[idx]
        arr2.append(itm[key])
        idx = inc(idx)
    return arr2

def array_reverse(arr):
    return list(reversed(arr))

def str_concat(s1, s2):
    return '' + str(s1) + str(s2)

def str_concat3(s1, s2, s3):
    return '' + str(s1) + str(s2) + str(s3)

def inc(n):
    return n + 1

def dec(n):
    return n - 1

runtime_has_object_keys = is_function(dict.keys)

# runs aFn(key, value) on every key/value pair inside of obj
def object_for_each(obj, aFn):
    if runtime_has_object_keys:
        keys = list(obj.keys())
        num_keys = array_size(keys)
        idx = 0
        while idx < num_keys:
            key = keys[idx]
            aFn(key, obj[key])
            idx = inc(idx)
    else:
        for key in obj:
            if obj.get(key):
                aFn(key, obj[key])

def delete_obj_key(obj, key):
    if key in obj:
        del obj[key]
    return obj

def always_true():
    return True

# ---------------------------------------------------------------------------
# Stack Operations

def stack_peek(arr, idx_from_back):
    max_idx = dec(array_size(arr))
    if idx_from_back > max_idx:
        return None
    return arr[max_idx - idx_from_back]

def stack_pop(s):
    return s.pop()

def stack_push(s, itm):
    s.append(itm)
    return None

# ---------------------------------------------------------------------------
# String Utils

# returns the character at position n inside of String s (0-indexed)
def char_at(s, n):
    return s[n]

# Returns the substring of s beginning at start inclusive, and ending
# at end (defaults to length of string), exclusive.
def substr(s, start, end=None):
    len_s = str_len(s)
    if not is_positive_int(end):
        end = len_s
    if end > len_s:
        end = len_s
    # TODO: throw here if end < start?
    return s[start:end]

def repeat_string(text, n):
    return text * n

# does String needle exist inside of String s?
def str_includes(s, needle):
    return needle in s

def to_upper_case(s):
    return s.upper()

def str_join(arr, s):
    return s.join(arr)

def rtrim(s):
    return s.rstrip()

def str_trim(s):
    return s.strip()

def str_starts_with(s, start_str):
    return s.startswith(start_str)

def str_ends_with(s, end_str):
    return s.endswith(end_str)

def is_non_blank_string(s):
    return is_string(s) and s != ''

def str_replace(s, find, replace):
    return s.replace(find, replace)

def crlf_to_lf(txt):
    return txt.replace('\r\n', '\n')

def str_split(string, ch):
    return string.split(ch)

# ---------------------------------------------------------------------------
# id generator

id_counter = 0

def create_id():
    global id_counter
    id_counter = inc(id_counter)
    return id_counter

# ---------------------------------------------------------------------------
# Node Types

# creates and returns an AST Node Object:
# - start: start position in String (inclusive)
# - end: end position in String (exclusive)
# - children: array of child Nodes
# - name: name of the Node
# - text: raw text of the Node (only for terminal nodes like Regex or Strings)
def Node(opts):
    return {
        'children': opts.get('children'),
        'end': opts.get('end'),
        'id': create_id(),
        'name': opts.get('name'),
        'start': opts.get('start'),
        'text': opts.get('text')
    }

def Named(opts):
    def parse(txt, pos):
        parser = get_parser(opts['parser'])
        node = parser.parse(txt, pos)

        if not node:
            return None
        elif node and not is_string(node.get('name')):
            node['name'] = opts['name']
            return node
        else:
            return Node({
                'start': node['start'],
                'end': node['end'],
                'children': [node],
                'name': opts['name']
            })

    return {'parse': parse}

# ---------------------------------------------------------------------------
# Terminal Parsers

# Terminal parser that matches any single character.
def AnyChar(opts):
    def parse(txt, pos):
        if pos < str_len(txt):
            return Node({
                'start': pos,
                'end': inc(pos),
                'name': opts['name'],
                'text': char_at(txt, pos)
            })
        else:
            return None

    return {
        'name': opts['name'],
        'parse': parse
    }

# Terminal parser that matches one character.
def Char(opts):
    def parse(txt, pos):
        if pos < str_len(txt) and char_at(txt, pos) == opts['char']:
            return Node({
                'start': pos,
                'end': inc(pos),
                'name': opts['name'],
                'text': opts['char']
            })
        else:
            return None

    return {
        'isTerminal': True,
        'char': opts['char'],
        'name': opts['name'],
        'parse': parse
    }

# Terminal parser that matches any single character, except one.
def NotChar(opts):
    def parse(txt, pos):
        if pos < str_len(txt):
            char_at_this_pos = char_at(txt, pos)
            if char_at_this_pos != opts['char']:
                return Node({
                    'start': pos,
                    'end': inc(pos),
                    'name': opts['name'],
                    'text': char_at_this_pos
                })

    return {
        'isTerminal': True,
        'char': opts['char'],
        'name': opts['name'],
        'parse': parse
    }

# Terminal parser that matches a String
def String(opts):
    def parse(txt, pos):
        len_str = str_len(opts['str'])
        if pos + len_str <= str_len(txt):
            str_to_compare = substr(txt, pos, pos + len_str)
            if opts['str'] == str_to_compare:
                return Node({
                    'start': pos,
                    'end': pos + len_str,
                    'name': opts['name'],
                    'text': opts['str']
                })

    return {
        'name': opts['name'],
        'parse': parse
    }

# TODO: extract the regex operations here to make it easier to port
import re

def Regex(opts):
    def parse(txt, pos):
        # NOTE: this might be a perf issue; investigate later
        txt2 = substr(txt, pos)
        result = re.match(opts['regex'], txt2)

        # HACK HACK HACK:
        # make sure the match was the beginning of the String
        # this can break in subtle ways: think of a better solution here
        if result and result.group(0) != '':
            matched_txt = result.group(0)
            if not txt2.startswith(matched_txt):
                return None

        matched_str = None
        if result and is_integer(opts.get('groupIdx')) and is_string(result.group(inc(opts['groupIdx']))):
            matched_str = result.group(inc(opts['groupIdx']))
        elif result and is_string(result.group(0)):
            matched_str = result.group(0)

        if is_string(matched_str):
            return Node({
                'start': pos,
                'end': pos + str_len(matched_str),
                'name': opts['name'],
                'text': matched_str
            })

        return None

    return {
        'name': opts['name'],
        'pattern_str': opts['regex'],
        'parse': parse
    }

# ---------------------------------------------------------------------------
# Sequence Parsers

# parser that matches a linear sequence of other parsers
def Seq(opts):
    def parse(txt, pos):
        children = []
        end = pos

        j = 0
        num_parsers = array_size(opts['parsers'])
        while j < num_parsers:
            parser = opts['parsers'][j]

            possible_node = parser.parse(txt, end)
            if possible_node:
                append_children(children, possible_node)
                end = possible_node['end']
            else:
                # else this is not a valid sequence: early return
                return None
            j = inc(j)

        return Node({'start': pos, 'end': end, 'children': children, 'name': opts['name']})

    return {
        'isTerminal': False,
        'name': opts['name'],
        'parse': parse
    }

# matches the first matching of several parsers
def Choice(opts):
    def parse(txt, pos):
        i = 0
        num_parsers = array_size(opts['parsers'])
        while i < num_parsers:
            parser = get_parser(opts['parsers'][i])
            possible_node = parser.parse(txt, pos)

            if possible_node:
                return possible_node

            i = inc(i)
        return None

    return {'parse': parse}

# matches child parser zero or more times
def Repeat(opts: Dict[str, Any]):
    def parse(txt: str, pos: int):
        opts['parser'] = get_parser(opts['parser'])

        min_matches = 0
        if isinstance(opts.get('minMatches'), int) and opts['minMatches'] >= 0:
            min_matches = opts['minMatches']

        children = []
        end = pos

        look_for_the_next_node = True
        while look_for_the_next_node:
            node = opts['parser'].parse(txt, end)
            if node:
                append_children(children, node)
                end = node['end']
            else:
                look_for_the_next_node = False

        name2 = None
        if isinstance(opts.get('name'), str) and end > pos:
            name2 = opts['name']

        if len(children) >= min_matches:
            return Node(
                start=pos,
                end=end,
                children=children,
                name=name2
            )
        else:
            return None

    return {'parse': parse}

def Optional(parser):
    def parse(txt: str, pos: int):
        node = parser.parse(txt, pos)
        if node and isinstance(node.get('text'), str) and node['text'] != '':
            return node
        else:
            return Node(start=pos, end=pos)

    return {'parse': parse}

def append_children(children_arr: List, node: Dict):
    if isinstance(node.get('name'), str) and node['name'] != '':
        children_arr.append(node)
    elif isinstance(node.get('children'), list):
        for child in node['children']:
            if child:
                append_children(children_arr, child)

def get_parser(p):
    if isinstance(p, str) and p in parsers:
        return parsers[p]
    if isinstance(p, dict) and callable(p.get('parse')):
        return p
    print(f'Unable to getParser: {p}')
    return None

parsers = {}

parsers['string'] = Seq(
    name='string',
    parsers=[
        Regex(regex=r'#?"', name='.open'),
        Optional(Regex(regex=r'([^"\\]+|\\.)+', name='.body')),
        Optional(Char(char='"', name='.close'))
    ]
)

whitespace_commons = r' ,\n\r\t\f'
whitespace_unicodes = r'\u000B\u001C\u001D\u001E\u001F\u2028\u2029\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2008\u2009\u200a\u205f\u3000'
whitespace_chars = whitespace_commons + whitespace_unicodes

token_head_chars = r'()\[\]{}\"@~^;`#\''
token_tail_chars = r'()\[\]{}\"@^;`'

token_re_str = f'[^{token_head_chars}{whitespace_chars}][^{token_tail_chars}{whitespace_chars}]*'
char_re_str = r'\\[()\[\]{}\"@^;`, ]'

parsers['token'] = Regex(name='token', regex=re.compile(f'(##)?({char_re_str}|{token_re_str})'))

parsers['_ws'] = Regex(name='whitespace', regex=re.compile(f'[{whitespace_chars}]+'))

parsers['comment'] = Regex(name='comment', regex=r';[^\n]*')

parsers['discard'] = Seq(
    name='discard',
    parsers=[
        String(name='marker', str='#_'),
        Repeat(parser='_gap'),
        Named(name='.body', parser='_form')
    ]
)

parsers['braces'] = Seq(
    name='braces',
    parsers=[
        Choice(
            parsers=[
                Char(name='.open', char='{'),
                String(name='.open', str='#{'),
                String(name='.open', str='#::{'),
                Regex(name='.open', regex=r'#:{1,2}[a-zA-Z][a-zA-Z0-9.-_]*{')
            ]
        ),
        Repeat(
            name='.body',
            parser=Choice(parsers=['_gap', '_form', NotChar(name='error', char='}')])
        ),
        Optional(Char(name='.close', char='}'))
    ]
)

parsers['brackets'] = Seq(
    name='brackets',
    parsers=[
        Char(name='.open', char='['),
        Repeat(
            name='.body',
            parser=Choice(parsers=['_gap', '_form', NotChar(name='error', char=']')])
        ),
        Optional(Char(name='.close', char=']'))
    ]
)

parsers['parens'] = Seq(
    name='parens',
    parsers=[
        Regex(name='.open', regex=r'(#\?@|#\?|#=|#)?\('),
        Repeat(
            name='.body',
            parser=Choice(parsers=['_gap', '_form', NotChar(char=')', name='error')])
        ),
        Optional(Char(name='.close', char=')'))
    ]
)

parsers['_gap'] = Choice(parsers=['_ws', 'comment', 'discard'])

parsers['meta'] = Seq(
    name='meta',
    parsers=[
        Repeat(
            min_matches=1,
            parser=Seq(
                parsers=[
                    Regex(name='.marker', regex=r'#?\^'),
                    Repeat(parser='_gap'),
                    Named(name='.meta', parser='_form'),
                    Repeat(parser='_gap')
                ]
            )
        ),
        Named(name='.body', parser='_form')
    ]
)

parsers['wrap'] = Seq(
    name='wrap',
    parsers=[
        Regex(name='.marker', regex=r'(@|\'|`|~@|~|#\')'),
        Repeat(parser='_gap'),
        Named(name='.body', parser='_form')
    ]
)

parsers['tagged'] = Seq(
    name='tagged',
    parsers=[
        Char(char='#'),
        Repeat(parser='_gap'),
        Named(name='.tag', parser='token'),
        Repeat(parser='_gap'),
        Named(name='.body', parser='_form')
    ]
)

parsers['_form'] = Choice(parsers=['token', 'string', 'parens', 'brackets', 'braces', 'wrap', 'meta', 'tagged'])

parsers['source'] = Repeat(
    name='source',
    parser=Choice(parsers=['_gap', '_form', AnyChar(name='error')])
)

def node_contains_text(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] != ''

def is_node_with_non_blank_text(node: Dict) -> bool:
    return node_contains_text(node) and node['text'][0] != ' '

def is_ns_node(node: Dict) -> bool:
    return node['name'] == 'token' and node['text'] == 'ns'

def is_require_node(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] in (':require', 'require')

def is_require_macros_keyword(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':require-macros'

def is_refer_clojure_node(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] in (':refer-clojure', 'refer-clojure')

def is_exclude_keyword(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':exclude'

def is_only_keyword(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':only'

def is_rename_keyword(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':rename'

def is_as_keyword(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':as'

def is_as_alias_keyword(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':as-alias'

def is_refer_keyword(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':refer'

def is_refer_macros_keyword(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':refer-macros'

def is_include_macros_node(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':include-macros'

def is_boolean_node(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] in ('true', 'false')

def is_all_node(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':all'

def is_keyword_node(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'].startswith(':')

def is_import_node(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] in (':import', 'import')

def is_newline_node(n: Dict) -> bool:
    return n['name'] == 'whitespace' and isinstance(n.get('text'), str) and '\n' in n['text']

def is_whitespace_node(n: Dict) -> bool:
    return n['name'] == 'whitespace' or is_newline_node(n)

def is_comma_node(n: Dict) -> bool:
    return n['name'] == 'whitespace' and ',' in n['text']

def is_paren_opener(n: Dict) -> bool:
    return n and n['name'] == '.open' and n['text'] in ('(', '[', '{', '#{', '#(', '#?(', '#?@(')

def is_paren_closer(n: Dict) -> bool:
    return n and n['name'] == '.close' and n['text'] in (')', ']', '}')

def is_token_node(n: Dict) -> bool:
    return n['name'] == 'token'

def is_tag_node(n: Dict) -> bool:
    return n['name'] == '.tag'

def is_string_node(n: Dict) -> bool:
    return n['name'] == 'string'

def is_comment_node(n: Dict) -> bool:
    return n['name'] == 'comment'

def is_reader_comment_node(n: Dict) -> bool:
    return n['name'] == 'discard'

def is_discard_node(n: Dict) -> bool:
    return n['name'] == 'marker' and n['text'] == '#_'

def is_standard_clj_ignore_keyword(n: Dict) -> bool:
    return n['name'] == 'token' and n['text'] == ':standard-clj/ignore'

def is_standard_clj_ignore_file_keyword(n: Dict) -> bool:
    return n['name'] == 'token' and n['text'] == ':standard-clj/ignore-file'

def node_contains_text_and_not_whitespace(n: Dict) -> bool:
    return node_contains_text(n) and not is_whitespace_node(n)

def is_one_space_opener(opener: Dict) -> bool:
    return opener['text'] in ('{', '[')

def is_anon_fn_opener(opener: Dict) -> bool:
    return opener['text'] == '#('

def is_reader_conditional_opener(opener: Dict) -> bool:
    return opener['text'] in ('#?(', '#?@(')

def is_opening_brace_node(n: Dict) -> bool:
    return (n['name'] == 'braces' and isinstance(n.get('children'), list) and
            len(n['children']) == 3 and n['children'][2]['name'] == '.close' and
            n['children'][2]['text'] == '}')

def comment_needs_space_before(line_txt: str, node_txt: str) -> bool:
    return (node_txt.startswith(';') and line_txt != '' and
            not line_txt.endswith(' ') and not line_txt.endswith('(') and
            not line_txt.endswith('[') and not line_txt.endswith('{'))

def comment_needs_space_inside(comment_txt: str) -> bool:
    return not re.match(r'^;+ ', comment_txt) and not re.match(r'^;+$', comment_txt)

def is_gen_class_node(node: Dict) -> bool:
    return node and isinstance(node.get('text'), str) and node['text'] == ':gen-class'

def is_gen_class_keyword(node: Dict) -> bool:
    gen_class_keywords = (':name', ':extends', ':implements', ':init', ':constructors',
                          ':post-init', ':methods', ':main', ':factory', ':state',
                          ':exposes', ':exposes-methods', ':prefix', ':impl-ns', ':load-impl-ns')
    return node and isinstance(node.get('text'), str) and node['text'] in gen_class_keywords

gen_class_keys = ['name', 'extends', 'implements', 'init', 'constructors', 'post-init',
                  'methods', 'main', 'factory', 'state', 'exposes', 'exposes-methods',
                  'prefix', 'impl-ns', 'load-impl-ns']

def is_gen_class_name_key(key_txt: str) -> bool:
    return key_txt in ('name', 'extends', 'init', 'post-init', 'factory', 'state', 'impl-ns')

def is_gen_class_boolean_key(key_txt: str) -> bool:
    return key_txt in ('main', 'load-impl-ns')

def recurse_all_children(node: Dict, f: Callable):
    f(node)
    if node.get('children'):
        for child_node in node['children']:
            recurse_all_children(child_node, f)
    return None

def get_text_from_root_node(root_node: Dict) -> str:
    s = []
    def collect_text(n):
        if isinstance(n.get('text'), str) and n['text'] != '':
            s.append(n['text'])
    recurse_all_children(root_node, collect_text)
    return ''.join(s)

def get_last_child_node_with_text(root_node: Dict) -> Optional[Dict]:
    last_node = [None]
    def update_last_node(n):
        if isinstance(n.get('text'), str) and n['text'] != '':
            last_node[0] = n
    recurse_all_children(root_node, update_last_node)
    return last_node[0]

def flatten_tree(tree: Dict) -> List[Dict]:
    nodes = []
    recurse_all_children(tree, nodes.append)
    return nodes

def find_next_node_with_text(all_nodes: List[Dict], idx: int) -> Optional[Dict]:
    for node in all_nodes[idx:]:
        if isinstance(node.get('text'), str) and node['text'] != '':
            return node
    return None

def find_next_non_whitespace_node(all_nodes: List[Dict], idx: int) -> Optional[Dict]:
    for node in all_nodes[idx:]:
        if not is_whitespace_node(node):
            return node
    return None

def find_next_non_whitespace_node(all_nodes, idx):
    max_idx = len(all_nodes)

    while idx < max_idx:
        node = all_nodes[idx]
        if not is_whitespace_node(node):
            return node
        idx += 1

    return None

def find_prev_node_with_text(all_nodes, start_idx, starting_node_id):
    keep_searching = True
    idx = start_idx
    before_starting_node = False

    while keep_searching:
        node = all_nodes[idx]

        if not before_starting_node:
            if node.id == starting_node_id:
                before_starting_node = True
        else:
            if node_contains_text(node):
                return node

        idx -= 1
        if idx == 0:
            keep_searching = False

    return None

def find_next_node_with_predicate_after_specific_node(all_nodes, start_idx, pred_fn, specific_node_id):
    max_idx = len(all_nodes)
    keep_searching = True
    idx = start_idx
    after_specific_node = False

    while keep_searching:
        node = all_nodes[idx]

        if not after_specific_node:
            if node.id == specific_node_id:
                after_specific_node = True
        else:
            if pred_fn(node):
                return node

        idx += 1
        if idx >= max_idx:
            keep_searching = False

    return None

def find_prev_node_with_predicate(all_nodes, start_idx, pred_fn):
    idx = start_idx
    while idx >= 0:
        node = all_nodes[idx]

        if pred_fn(node):
            return node

        idx -= 1

    return None

def are_forward_nodes_already_slurped(nodes, idx):
    nodes_size = len(nodes)
    result = True
    keep_searching = True

    while keep_searching:
        node = nodes[idx] if idx < nodes_size else None

        if not node:
            keep_searching = False
        elif is_newline_node(node):
            keep_searching = False
        elif not isinstance(node.text, str):
            keep_searching = True
        elif node._was_slurped_up or is_whitespace_node(node):
            keep_searching = True
        else:
            keep_searching = False
            result = False

        idx += 1

        if idx >= nodes_size:
            keep_searching = False

    return result

def find_forward_closing_parens(nodes, idx):
    closers = []
    nodes_size = len(nodes)

    keep_searching = True
    while keep_searching:
        node = nodes[idx] if idx < nodes_size else None

        if not node:
            keep_searching = False
        elif is_whitespace_node(node) or is_paren_closer(node) or is_comment_node(node):
            closers.append(node)
            keep_searching = True
        else:
            keep_searching = False

        idx += 1

        if idx >= nodes_size:
            keep_searching = False

    return closers

def num_spaces_after_newline(newline_node):
    x = newline_node.text.split('\n')
    last_x = x[-1]
    return len(last_x)

def record_original_col_indexes(nodes, idx):
    initial_spaces = 0
    if is_newline_node(nodes[idx]):
        initial_spaces = num_spaces_after_newline(nodes[idx])
        idx += 1

    col_idx = initial_spaces
    num_nodes = len(nodes)
    keep_searching = True
    while keep_searching:
        node = nodes[idx] if idx < num_nodes else None

        if not node:
            keep_searching = False
        elif is_newline_node(node):
            keep_searching = False
        else:
            node_txt = node.text
            if isinstance(node_txt, str) and node_txt != '':
                node_txt_length = len(node_txt)
                node._orig_col_idx = col_idx
                col_idx += node_txt_length

        idx += 1
        if idx > num_nodes:
            keep_searching = False

    return nodes

def remove_leading_whitespace(txt):
    return re.sub(r'^[, ]*\n+ *', '', txt).rstrip()

def txt_has_commas_after_newline(s):
    return bool(re.search(r'\n.*,.*$', s))

def has_commas_after_newline(node):
    return is_whitespace_node(node) and txt_has_commas_after_newline(node.text)

def is_next_line_a_comment_line(nodes, idx):
    n1 = nodes[idx] if idx < len(nodes) else None
    n2 = nodes[idx + 1] if idx + 1 < len(nodes) else None

    if n1 and n2:
        return is_comment_node(n1) and is_newline_node(n2)
    elif n1 and not n2:
        return is_comment_node(n1)
    else:
        return False

def num_spaces_for_indentation(wrapping_opener):
    if not wrapping_opener:
        return 0
    else:
        next_node_after_opener = wrapping_opener._next_with_text
        opener_text_length = len(wrapping_opener.text)
        opener_col_idx = wrapping_opener._printed_col_idx

        directly_underneath_opener = opener_col_idx + opener_text_length

        if is_reader_conditional_opener(wrapping_opener):
            return directly_underneath_opener
        elif next_node_after_opener and is_paren_opener(next_node_after_opener):
            return opener_col_idx + 1
        elif is_one_space_opener(wrapping_opener):
            return opener_col_idx + 1
        elif is_anon_fn_opener(wrapping_opener):
            return opener_col_idx + 3
        else:
            return opener_col_idx + 2

def compare_symbols_then_platform(itm_a, itm_b):
    if itm_a['symbol'] > itm_b['symbol']:
        return 1
    elif itm_a['symbol'] < itm_b['symbol']:
        return -1
    elif itm_a['symbol'] == itm_b['symbol']:
        if itm_a.get('platform', '') > itm_b.get('platform', ''):
            return 1
        elif itm_a.get('platform', '') < itm_b.get('platform', ''):
            return -1
    return 0

def compare_from_symbol(itm_a, itm_b):
    if itm_a['fromSymbol'] > itm_b['fromSymbol']:
        return 1
    elif itm_a['fromSymbol'] < itm_b['fromSymbol']:
        return -1
    else:
        return 0

def compare_imports(import_a, import_b):
    if import_a['package'] > import_b['package']:
        return 1
    elif import_a['package'] < import_b['package']:
        return -1
    else:
        return 0

def looks_like_a_java_classname(s):
    return s[0].isupper()

def parse_java_package_with_class(s):
    chunks = s.split('.')
    last_itm = chunks[-1]

    if looks_like_a_java_classname(last_itm):
        package_chunks = chunks[:-1]
        package_name = '.'.join(package_chunks)

        return {
            'package': package_name,
            'className': last_itm
        }
    else:
        return {
            'package': s,
            'className': None
        }

def find_next_token_inside_require_form(nodes, idx):
    result = None
    num_nodes = len(nodes)

    keep_searching = True
    while keep_searching:
        node = nodes[idx]

        if is_paren_closer(node):
            keep_searching = False
            result = None
        elif is_token_node(node) and node.text != '':
            keep_searching = False
            result = node

        idx += 1

        if idx >= num_nodes:
            keep_searching = False

    return result

def sort_ns_result(result, prefix_list_comments):
    # Sort :refer-clojure :exclude symbols
    if result.get('referClojure') and isinstance(result['referClojure'].get('exclude'), list):
        result['referClojure']['exclude'].sort(key=functools.cmp_to_key(compare_symbols_then_platform))

    # Sort :refer-clojure :only symbols
    if result.get('referClojure') and isinstance(result['referClojure'].get('only'), list):
        result['referClojure']['only'].sort(key=functools.cmp_to_key(compare_symbols_then_platform))

    # Sort :refer-clojure :rename symbols
    if result.get('referClojure') and isinstance(result['referClojure'].get('rename'), list):
        result['referClojure']['rename'].sort(key=functools.cmp_to_key(compare_from_symbol))

    # Sort :require-macros symbols
    if isinstance(result.get('requireMacros'), list):
        result['requireMacros'].sort(key=functools.cmp_to_key(compare_symbols_then_platform))

        # Sort :refer symbols
        for rm in result['requireMacros']:
            if isinstance(rm.get('refer'), list):
                rm['refer'].sort(key=functools.cmp_to_key(compare_symbols_then_platform))

    # Sort the requires symbols
    if isinstance(result.get('requires'), list):
        result['requires'].sort(key=functools.cmp_to_key(compare_symbols_then_platform))

        for req in result['requires']:
            # Attach prefix list comments to the first require with the same id (if possible)
            if req.get('prefixListId'):
                if prefix_list_comments.get(req['prefixListId']):
                    if prefix_list_comments[req['prefixListId']].get('commentsAbove'):
                        req['commentsAbove'] = prefix_list_comments[req['prefixListId']]['commentsAbove']
                    if prefix_list_comments[req['prefixListId']].get('commentAfter'):
                        req['commentAfter'] = prefix_list_comments[req['prefixListId']]['commentAfter']
                    del prefix_list_comments[req['prefixListId']]

            # Delete prefixListIds from the result
            req.pop('prefixListId', None)

            # Sort :require :refer symbols
            if isinstance(req.get('refer'), list):
                req['refer'].sort(key=functools.cmp_to_key(compare_symbols_then_platform))

            # Sort :require :exclude symbols
            if isinstance(req.get('exclude'), list):
                req['exclude'].sort(key=functools.cmp_to_key(compare_symbols_then_platform))

            # Sort :require :rename symbols
            if isinstance(req.get('rename'), list):
                req['rename'].sort(key=functools.cmp_to_key(compare_from_symbol))

    # Convert and sort the imports
    if result.get('importsObj'):
        result['imports'] = []

        for package_name, obj in result['importsObj'].items():
            sorted_classes = sorted(obj['classes'])
            import_obj = {
                'package': package_name,
                'classes': sorted_classes
            }

            if obj.get('commentsAbove'):
                import_obj['commentsAbove'] = obj['commentsAbove']
            if obj.get('commentAfter'):
                import_obj['commentAfter'] = obj['commentAfter']
            if obj.get('platform'):
                import_obj['platform'] = obj['platform']

            result['imports'].append(import_obj)

        del result['importsObj']

        result['imports'].sort(key=functools.cmp_to_key(compare_imports))

    # Merge nsMetadata keys
    if isinstance(result.get('nsMetadata'), list):
        num_metadata_itms = len(result['nsMetadata'])
        if num_metadata_itms > 1:
            metadata_obj = {}
            metadata_keys = []
            for metadata_itm in result['nsMetadata']:
                metadata_obj[metadata_itm['key']] = metadata_itm['value']
                metadata_keys.append(metadata_itm['key'])

            new_ns_metadata = []
            for key in reversed(metadata_keys):
                if key in metadata_obj:
                    metadata_itm2 = {
                        'key': key,
                        'value': metadata_obj[key]
                    }
                    del metadata_obj[key]
                    new_ns_metadata.append(metadata_itm2)

            result['nsMetadata'] = list(reversed(new_ns_metadata))

    return result

# Search for a #_ :standard-clj/ignore-file
# stopping when we reach the first (ns) form
def look_for_ignore_file(nodes_arr):
    keep_searching = True
    num_nodes = len(nodes_arr)
    idx = 0

    while keep_searching:
        node = nodes_arr[idx]

        if is_discard_node(node):
            next1 = find_next_node_with_predicate_after_specific_node(nodes_arr, idx, node_contains_text_and_not_whitespace, node.id)
            if is_standard_clj_ignore_file_keyword(next1):
                return True
            elif next1.text == '{':
                next2 = find_next_node_with_predicate_after_specific_node(nodes_arr, idx, node_contains_text_and_not_whitespace, next1.id)
                if is_standard_clj_ignore_file_keyword(next2):
                    next3 = find_next_node_with_predicate_after_specific_node(nodes_arr, idx, node_contains_text_and_not_whitespace, next2.id)
                    if next3.name == 'token' and next3.text == 'true':
                        return True
        elif is_ns_node(node):
            return False

        idx += 1

        if idx >= num_nodes:
            keep_searching = False

    return False

def look_for_ignore_file(nodes_arr):
    keep_searching = True
    num_nodes = len(nodes_arr)
    idx = 0

    while keep_searching:
        node = nodes_arr[idx]

        if is_discard_node(node):
            next1 = find_next_node_with_predicate_after_specific_node(nodes_arr, idx, node_contains_text_and_not_whitespace, node.id)
            if is_standard_clj_ignore_file_keyword(next1):
                return True
            elif next1.text == '{':
                next2 = find_next_node_with_predicate_after_specific_node(nodes_arr, idx, node_contains_text_and_not_whitespace, next1.id)
                if is_standard_clj_ignore_file_keyword(next2):
                    next3 = find_next_node_with_predicate_after_specific_node(nodes_arr, idx, node_contains_text_and_not_whitespace, next2.id)
                    if next3.name == 'token' and next3.text == 'true':
                        return True
        elif is_ns_node(node):
            return False

        idx += 1

        if idx >= num_nodes:
            keep_searching = False

    return False

# Extracts namespace information from a flat array of Nodes.
# Returns a data structure of the ns form that can be used to "print from scratch"
def parse_ns(nodes_arr):
    idx = 0
    num_nodes = len(nodes_arr)
    result = {
        "ns_symbol": None
    }

    continue_parsing_ns_form = True
    ns_form_ends_line_idx = -1
    paren_nesting_depth = 0
    line_no = 0
    paren_stack = []
    inside_ns_form = False
    inside_refer_clojure_form = False
    refer_clojure_line_no = -1
    inside_require_form = False
    require_form_line_no = -1
    inside_import_form = False
    import_form_line_no = -1
    next_text_node_is_ns_symbol = False
    inside_import_package_list = False
    collect_refer_clojure_exclude_symbols = False
    collect_refer_clojure_only_symbols = False
    collect_refer_clojure_rename_symbols = False
    collect_require_exclude_symbols = False
    require_exclude_symbol_paren_depth = -1
    renames_tmp = []
    import_package_list_first_token = None
    ns_node_idx = -1
    ns_symbol_idx = -1
    beyond_ns_metadata = False
    inside_ns_metadata_hash_map = False
    inside_ns_metadata_shorthand = False
    next_token_node_is_metadata_true_key = False
    next_text_node_is_metadata_key = False
    metadata_value_node_id = -1
    tmp_metadata_key = ''
    refer_clojure_node_idx = -1
    require_node_idx = -1
    refer_idx = -1
    refer_paren_nesting_depth = -1
    import_node_idx = -1
    import_node_paren_nesting_depth = -1
    active_require_idx = -1
    require_symbol_idx = -1
    next_token_is_as_symbol = False
    single_line_comments = []
    active_import_package_name = None
    prev_node_is_newline = False
    line_of_last_comment_recording = -1
    inside_prefix_list = False
    prefix_list_prefix = None
    prefix_list_line_no = -1
    prefix_list_comments = {}
    current_prefix_list_id = None
    inside_reader_conditional = False
    current_reader_conditional_platform = None
    reader_conditional_paren_nesting_depth = -1
    inside_require_list = False
    require_list_paren_nesting_depth = -1
    refer_macros_idx = -1
    refer_macros_paren_nesting_depth = -1
    inside_include_macros = False
    active_require_macros_idx = -1
    inside_require_macros_form = False
    require_macros_node_idx = -1
    require_macros_line_no = -1
    require_macros_paren_nesting_depth = -1
    require_macros_refer_node_idx = -1
    require_macros_as_node_idx = -1
    require_macros_rename_idx = -1
    gen_class_node_idx = -1
    inside_gen_class = False
    gen_class_line_no = -1
    gen_class_toggle = 0
    gen_class_key_str = None
    gen_class_value_line_no = -1
    inside_reader_comment = False
    id_of_last_node_inside_reader_comment = -1
    require_rename_idx = -1
    skip_nodes_until_we_reach_this_id = -1

    while continue_parsing_ns_form:
        node = nodes_arr[idx]
        current_node_is_newline = is_newline_node(node)

        if paren_nesting_depth == 1 and is_ns_node(node):
            inside_ns_form = True
            next_text_node_is_ns_symbol = True
            ns_node_idx = idx
        elif inside_ns_form and is_refer_clojure_node(node):
            inside_refer_clojure_form = True
            refer_clojure_line_no = line_no
            refer_clojure_node_idx = idx
            beyond_ns_metadata = True
        elif inside_ns_form and is_require_node(node):
            inside_require_form = True
            require_form_line_no = line_no
            require_node_idx = idx
            beyond_ns_metadata = True
        elif inside_ns_form and is_import_node(node):
            inside_import_form = True
            import_form_line_no = line_no
            import_node_idx = idx
            import_node_paren_nesting_depth = paren_nesting_depth
            beyond_ns_metadata = True
        elif inside_ns_form and is_require_macros_keyword(node):
            inside_require_macros_form = True
            require_macros_node_idx = idx
            require_macros_line_no = line_no
            require_macros_paren_nesting_depth = paren_nesting_depth
            beyond_ns_metadata = True
        elif inside_ns_form and is_gen_class_node(node):
            inside_gen_class = True
            gen_class_node_idx = idx
            beyond_ns_metadata = True

        if is_paren_opener(node):
            paren_nesting_depth += 1
            paren_stack.append(node)

            if inside_ns_form and is_reader_conditional_opener(node):
                inside_reader_conditional = True
                current_reader_conditional_platform = None
                reader_conditional_paren_nesting_depth = paren_nesting_depth
            elif inside_require_form:
                inside_require_list = True
                require_list_paren_nesting_depth = paren_nesting_depth
            elif inside_import_form and paren_nesting_depth > import_node_paren_nesting_depth:
                inside_import_package_list = True
        elif is_paren_closer(node):
            paren_nesting_depth -= 1
            paren_stack.pop()

            if inside_import_package_list:
                inside_import_package_list = False
                import_package_list_first_token = None
            elif inside_require_form and paren_nesting_depth <= 1:
                inside_require_form = False
            elif inside_require_list and paren_nesting_depth < require_list_paren_nesting_depth:
                inside_require_list = False
                require_list_paren_nesting_depth = -1
                require_rename_idx = -1
            elif inside_refer_clojure_form and paren_nesting_depth <= 1:
                inside_refer_clojure_form = False
                refer_clojure_node_idx = -1
            elif inside_ns_form and paren_nesting_depth == 0:
                inside_ns_form = False
                ns_form_ends_line_idx = line_no

            if paren_nesting_depth < 3:
                collect_refer_clojure_exclude_symbols = False
                collect_refer_clojure_only_symbols = False
                collect_refer_clojure_rename_symbols = False
            if refer_idx > 0 and paren_nesting_depth < refer_paren_nesting_depth:
                refer_idx = -1
                refer_paren_nesting_depth = -1
            if inside_require_form and require_symbol_idx > 0:
                require_symbol_idx = -1
            if inside_require_form and inside_prefix_list:
                inside_prefix_list = False
                prefix_list_prefix = None
            if inside_reader_conditional and paren_nesting_depth == reader_conditional_paren_nesting_depth - 1:
                inside_reader_conditional = False
                current_reader_conditional_platform = None
                reader_conditional_paren_nesting_depth = -1
            if idx > refer_macros_idx and paren_nesting_depth <= refer_macros_paren_nesting_depth:
                refer_macros_idx = -1
                refer_macros_paren_nesting_depth = -1
            if inside_import_form and paren_nesting_depth < import_node_paren_nesting_depth:
                inside_import_form = False
                import_node_idx = -1
                import_node_paren_nesting_depth = -1
            if inside_require_macros_form and paren_nesting_depth < require_macros_paren_nesting_depth:
                inside_require_macros_form = False
                require_macros_paren_nesting_depth = -1
                require_macros_node_idx = -1
                require_macros_as_node_idx = -1
            if collect_require_exclude_symbols and paren_nesting_depth < require_exclude_symbol_paren_depth:
                collect_require_exclude_symbols = False
                require_exclude_symbol_paren_depth = -1

            require_macros_refer_node_idx = -1
            require_macros_rename_idx = -1

        is_token_node2 = is_token_node(node)
        is_text_node = node_contains_text(node)
        is_comment_node2 = is_comment_node(node)
        is_reader_comment_node2 = is_reader_comment_node(node)

        if is_reader_comment_node2:
            inside_reader_comment = True
            last_node_of_reader_comment = get_last_child_node_with_text(node)
            id_of_last_node_inside_reader_comment = last_node_of_reader_comment.id

        if skip_nodes_until_we_reach_this_id > 0:
            if node.id == skip_nodes_until_we_reach_this_id:
                skip_nodes_until_we_reach_this_id = -1

        # collect ns metadata shorthand
        elif inside_ns_metadata_shorthand:
            if node.name == '.marker' and node.text == '^':
                next_token_node_is_metadata_true_key = True
            elif next_token_node_is_metadata_true_key and is_token_node2:
                if 'ns_metadata' not in result:
                    result['ns_metadata'] = []

                metadata_obj = {}
                metadata_obj['key'] = node.text
                metadata_obj['value'] = 'true'

                result['ns_metadata'].append(metadata_obj)

                next_token_node_is_metadata_true_key = False
                inside_ns_metadata_shorthand = False

        # collect ns metadata inside a hash map literal
        elif inside_ns_metadata_hash_map:
            if next_text_node_is_metadata_key and node.name == '.close' and node.text == '}':
                inside_ns_metadata_hash_map = False
            elif not next_text_node_is_metadata_key and node.name == '.open' and node.text == '{':
                next_text_node_is_metadata_key = True
            elif next_text_node_is_metadata_key and is_token_node2:
                if 'ns_metadata' not in result:
                    result['ns_metadata'] = []

                tmp_metadata_key = node.text
                next_text_node_is_metadata_key = False

                # the next node should be a whitespace node, then collect the value for this key
                next_non_whitespace_node = find_next_non_whitespace_node(nodes_arr, idx + 1)
                metadata_value_node_id = next_non_whitespace_node.id
            elif node.id == metadata_value_node_id:
                metadata_obj = {}
                metadata_obj['key'] = tmp_metadata_key
                metadata_obj['value'] = get_text_from_root_node(node)

                result['ns_metadata'].append(metadata_obj)

                tmp_metadata_key = ''
                next_text_node_is_metadata_key = True
                metadata_value_node_id = -1

                # skip any forward nodes that we have just collected as text
                if isinstance(node.children, list):
                    last_child_node = node.children[-1]
                    skip_nodes_until_we_reach_this_id = last_child_node.id

        # collect ns metadata before we hit the nsSymbol
        elif not inside_ns_metadata_hash_map and not inside_ns_metadata_shorthand and inside_ns_form and ns_symbol_idx < 0 and node.name == 'meta':
            marker_node = find_next_node_with_text(nodes_arr, idx + 1)

            # NOTE: this should always be true
            if marker_node.text == '^':
                node_after_marker = find_next_node_with_text(nodes_arr, idx + 2)

                if node_after_marker and node_after_marker.text == '{':
                    inside_ns_metadata_hash_map = True
                elif node_after_marker and is_token_node(node_after_marker):
                    inside_ns_metadata_shorthand = True

        # collect metadata hash map after the ns symbol
        elif inside_ns_form and idx > ns_node_idx and paren_nesting_depth >= 1 and not beyond_ns_metadata and not inside_reader_comment and not inside_ns_metadata_shorthand and not inside_ns_metadata_hash_map and node.name == '.open' and node.text == '{':
            inside_ns_metadata_hash_map = True
            next_text_node_is_metadata_key = True

        # collect the ns symbol
        elif idx > ns_node_idx and next_text_node_is_ns_symbol and is_token_node2 and is_text_node:
            result['ns_symbol'] = node.text
            ns_symbol_idx = idx
            next_text_node_is_ns_symbol = False

        # collect reader conditional platform keyword
        elif inside_reader_conditional and paren_nesting_depth == reader_conditional_paren_nesting_depth and is_keyword_node(node):
            current_reader_conditional_platform = node.text

        # collect single-line comments
        elif inside_ns_form and idx > ns_node_idx and prev_node_is_newline and is_comment_node2:
            single_line_comments.append(node.text)

        # collect reader macro comment line(s)
        elif inside_ns_form and idx > ns_node_idx and prev_node_is_newline and is_reader_comment_node2:
            single_line_comments.append(get_text_from_root_node(node))

        # collect comments at the end of a line
        elif idx > ns_node_idx and not prev_node_is_newline and (is_comment_node2 or is_reader_comment_node2):
            comment_at_end_of_line = None
            if is_comment_node2:
                comment_at_end_of_line = node.text
            else:
                comment_at_end_of_line = get_text_from_root_node(node)

            if prefix_list_line_no == line_no:
                if current_prefix_list_id not in prefix_list_comments:
                    prefix_list_comments[current_prefix_list_id] = {}
                prefix_list_comments[current_prefix_list_id]['comment_after'] = comment_at_end_of_line
                line_of_last_comment_recording = line_no
            elif require_form_line_no == line_no and active_require_idx < 0:
                result['require_comment_after'] = comment_at_end_of_line
                line_of_last_comment_recording = line_no
            elif require_form_line_no == line_no and active_require_idx >= 0:
                result['requires'][active_require_idx]['comment_after'] = comment_at_end_of_line
                line_of_last_comment_recording = line_no
            elif line_no == refer_clojure_line_no and 'refer_clojure' in result:
                result['refer_clojure_comment_after'] = comment_at_end_of_line
                line_of_last_comment_recording = line_no
            elif import_form_line_no == line_no and 'imports_obj' not in result:
                result['import_comment_after'] = comment_at_end_of_line
                line_of_last_comment_recording = line_no
            elif import_form_line_no == line_no:
                result['imports_obj'][active_import_package_name]['comment_after'] = comment_at_end_of_line
                line_of_last_comment_recording = line_no
            elif require_macros_line_no == line_no:
                result['require_macros'][active_require_macros_idx]['comment_after'] = comment_at_end_of_line
                line_of_last_comment_recording = line_no
            elif gen_class_line_no == line_no:
                result['gen_class']['comment_after'] = comment_at_end_of_line
                line_of_last_comment_recording = line_no
            elif gen_class_value_line_no == line_no:
                result['gen_class'][gen_class_key_str]['comment_after'] = comment_at_end_of_line
                line_of_last_comment_recording = line_no

            if not inside_ns_form and line_no == line_of_last_comment_recording:
                result['comment_outside_ns_form'] = comment_at_end_of_line

        # discard nodes that are inside a reader comment
        elif inside_reader_comment:
            if node.id == id_of_last_node_inside_reader_comment:
                inside_reader_comment = False
                id_of_last_node_inside_reader_comment = -1

        # attach comments to the :require form
        elif inside_require_form and idx == require_node_idx and len(single_line_comments) > 0:
            result['require_comments_above'] = single_line_comments
            single_line_comments = []

        # attach comments to the :import form
        elif inside_import_form and idx == import_node_idx and len(single_line_comments) > 0:
            result['import_comments_above'] = single_line_comments
            single_line_comments = []

        # attach comments to the :refer-clojure form
        elif inside_refer_clojure_form and idx == refer_clojure_node_idx and len(single_line_comments) > 0:
            result['refer_clojure_comments_above'] = single_line_comments
            single_line_comments = []

        # collect the docstring
        elif inside_ns_form and idx > ns_node_idx and paren_nesting_depth == 1 and not beyond_ns_metadata and not inside_ns_metadata_shorthand and not inside_ns_metadata_hash_map and is_string_node(node):
            # NOTE: this should always be true, but I like being defensive
            if len(node.children) == 3 and node.children[1].name == '.body':
                result['docstring'] = node.children[1].text

        # collect :refer-clojure :exclude
        elif inside_refer_clojure_form and idx > refer_clojure_node_idx and is_exclude_keyword(node):
            if 'refer_clojure' not in result:
                result['refer_clojure'] = {}
            if 'exclude' not in result['refer_clojure'] or not isinstance(result['refer_clojure']['exclude'], list):
                result['refer_clojure']['exclude'] = []
            collect_refer_clojure_exclude_symbols = True

        # collect :refer-clojure :exclude symbols
        elif idx > refer_clojure_node_idx + 1 and collect_refer_clojure_exclude_symbols and paren_nesting_depth >= 3 and is_token_node2 and is_text_node and 'refer_clojure' in result and isinstance(result['refer_clojure']['exclude'], list):
            symbol_obj = {}
            symbol_obj['symbol'] = node.text

            if inside_reader_conditional and current_reader_conditional_platform:
                symbol_obj['platform'] = current_reader_conditional_platform

            result['refer_clojure']['exclude'].append(symbol_obj)

        # collect :refer-clojure :only
        elif inside_refer_clojure_form and idx > refer_clojure_node_idx and is_only_keyword(node):
            if 'refer_clojure' not in result:
                result['refer_clojure'] = {}
            result['refer_clojure']['only'] = []
            collect_refer_clojure_only_symbols = True

        # collect :refer-clojure :only symbols
        elif idx > refer_clojure_node_idx + 1 and collect_refer_clojure_only_symbols and paren_nesting_depth >= 3 and is_token_node2 and is_text_node and 'refer_clojure' in result and isinstance(result['refer_clojure']['only'], list):
            symbol_obj = {
                'symbol': node.text
            }

            # add reader conditional platform if necessary
            if inside_reader_conditional and current_reader_conditional_platform:
                symbol_obj['platform'] = current_reader_conditional_platform

            result['refer_clojure']['only'].append(symbol_obj)

        # collect :refer-clojure :rename
        elif inside_refer_clojure_form and idx > refer_clojure_node_idx and is_rename_keyword(node):
            if 'refer_clojure' not in result:
                result['refer_clojure'] = {}
            result['refer_clojure']['rename'] = []
            collect_refer_clojure_rename_symbols = True

        # collect :refer-clojure :rename symbols
        elif idx > refer_clojure_node_idx + 1 and collect_refer_clojure_rename_symbols and paren_nesting_depth >= 3 and is_token_node2 and is_text_node and 'refer_clojure' in result and isinstance(result['refer_clojure']['rename'], list):
            renames_tmp.append(node.text)

            if len(renames_tmp) == 2:
                itm = {}
                itm['from_symbol'] = renames_tmp[0]
                itm['to_symbol'] = renames_tmp[1]

                if inside_reader_conditional and current_reader_conditional_platform:
                    itm['platform'] = current_reader_conditional_platform

                result['refer_clojure']['rename'].append(itm)

                renames_tmp = []

        # is this :require :as ?
        elif idx > require_node_idx and inside_require_form and is_token_node2 and is_as_keyword(node):
            next_token_is_as_symbol = True

        # collect the require :as symbol
        elif idx > require_node_idx and inside_require_form and next_token_is_as_symbol and is_token_node2 and is_text_node:
            next_token_is_as_symbol = False
            result['requires'][active_require_idx]['as'] = node.text

        # collect :require-macros :refer symbols
        elif inside_require_macros_form and require_macros_refer_node_idx != -1 and idx > require_macros_refer_node_idx and is_token_node2 and is_text_node:
            if 'refer' not in result['require_macros'][active_require_macros_idx] or not isinstance(result['require_macros'][active_require_macros_idx]['refer'], list):
                result['require_macros'][active_require_macros_idx]['refer'] = []

            refer_obj = {}
            refer_obj['symbol'] = node.text

            if inside_reader_conditional and current_reader_conditional_platform:
                refer_obj['platform'] = current_reader_conditional_platform

            result['require_macros'][active_require_macros_idx]['refer'].append(refer_obj)

        # collect :require-macros :as symbol
        elif inside_require_macros_form and require_macros_as_node_idx != -1 and idx > require_macros_as_node_idx and is_token_node2 and is_text_node:
            result['require_macros'][active_require_macros_idx]['as'] = node.text
            require_macros_as_node_idx = -1

        # collect :require-macros :rename
        elif inside_require_macros_form and require_macros_rename_idx != -1 and idx > require_macros_rename_idx and is_token_node2 and is_text_node:
            if 'rename' not in result['require_macros'][active_require_macros_idx] or not isinstance(result['require_macros'][active_require_macros_idx]['rename'], list):
                result['require_macros'][active_require_macros_idx]['rename'] = []

            renames_tmp.append(node.text)

            if len(renames_tmp) == 2:
                itm = {}
                itm['from_symbol'] = renames_tmp[0]
                itm['to_symbol'] = renames_tmp[1]
                if inside_reader_conditional and current_reader_conditional_platform:
                    itm['platform'] = current_reader_conditional_platform
                result['require_macros'][active_require_macros_idx]['rename'].append(itm)
                renames_tmp = []

        # :require-macros :refer
        elif inside_require_macros_form and idx > require_macros_node_idx and is_refer_keyword(node):
            require_macros_refer_node_idx = idx

        # :require-macros :as
        elif inside_require_macros_form and idx > require_macros_node_idx and is_as_keyword(node):
            require_macros_as_node_idx = idx

        # :require-macros :rename
        elif inside_require_macros_form and idx > require_macros_node_idx and is_rename_keyword(node):
            require_macros_rename_idx = idx
            renames_tmp = []

        # collect :require-macros symbol
        elif inside_require_macros_form and idx > require_macros_node_idx and is_token_node2 and is_text_node:
            if 'require_macros' not in result:
                result['require_macros'] = []

                # add comments_above to the :require-macros form if possible
                if len(single_line_comments) > 0:
                    result['require_macros_comments_above'] = single_line_comments
                    single_line_comments = []

            req_obj = {
                'symbol': node.text
            }

            # store the comments above this line
            if len(single_line_comments) > 0:
                req_obj['comments_above'] = single_line_comments
                single_line_comments = []

            # add reader conditional platform
            if inside_reader_conditional and current_reader_conditional_platform:
                req_obj['platform'] = current_reader_conditional_platform

            result['require_macros'].append(req_obj)
            active_require_macros_idx += 1
            require_macros_line_no = line_no

        # is this :include-macros ?
        elif idx > require_node_idx and inside_require_form and is_token_node2 and is_include_macros_node(node):
            inside_include_macros = True

        # collect :include-macros boolean
        elif inside_include_macros and is_token_node2 and is_boolean_node(node):
            if node.text == 'true':
                result['requires'][active_require_idx]['include_macros'] = True
            else:
                result['requires'][active_require_idx]['include_macros'] = False

            inside_include_macros = False

        # is this :refer-macros ?
        elif idx > require_node_idx and inside_require_form and is_token_node2 and is_refer_macros_keyword(node):
            refer_macros_idx = idx
            refer_macros_paren_nesting_depth = paren_nesting_depth

        # collect :refer-macros symbols
        elif idx > refer_macros_idx and inside_require_form and paren_nesting_depth == refer_macros_paren_nesting_depth + 1 and is_token_node2 and is_text_node:
            if 'refer_macros' not in result['requires'][active_require_idx] or not isinstance(result['requires'][active_require_idx]['refer_macros'], list):
                result['requires'][active_require_idx]['refer_macros'] = []
            result['requires'][active_require_idx]['refer_macros'].append(node.text)

        # is this :require :refer ?
        elif idx > require_node_idx and inside_require_form and is_token_node2 and is_refer_keyword(node):
            refer_idx = idx
            refer_paren_nesting_depth = paren_nesting_depth

        # collect :require :exclude symbols
        elif idx > require_node_idx and inside_require_form and is_token_node2 and collect_require_exclude_symbols and paren_nesting_depth > require_exclude_symbol_paren_depth:
            symbol_obj = {
                'symbol': node.text
            }
            result['requires'][active_require_idx]['exclude'].append(symbol_obj)

        # is this :require :exclude ?
        elif idx > require_node_idx and inside_require_form and is_token_node2 and is_exclude_keyword(node):
            result['requires'][active_require_idx]['exclude'] = []
            collect_require_exclude_symbols = True
            require_exclude_symbol_paren_depth = paren_nesting_depth

        # :require :as-alias
        elif idx > require_node_idx and inside_require_form and is_token_node2 and is_as_alias_keyword(node):
            next_symbol = find_next_token_inside_require_form(nodes_arr, idx + 1)
            result['requires'][active_require_idx]['as_alias'] = next_symbol.text

        # collect :refer :all
        elif idx > refer_idx and inside_require_form and is_token_node2 and is_all_node(node):
            result['requires'][active_require_idx]['refer'] = 'all'

        # collect :require :refer symbols
        elif idx > refer_idx and inside_require_form and paren_nesting_depth == refer_paren_nesting_depth + 1 and is_token_node2 and is_text_node:
            if 'refer' not in result['requires'][active_require_idx] or not isinstance(result['requires'][active_require_idx]['refer'], list):
                result['requires'][active_require_idx]['refer'] = []
            refer_obj = {
                'symbol': node.text
            }
            result['requires'][active_require_idx]['refer'].append(refer_obj)

        # collect :require symbol not inside of a list / vector
        elif inside_require_form and not inside_require_list and idx > require_node_idx and is_token_node2 and is_text_node and require_symbol_idx == -1 and not is_keyword_node(node):
            if 'requires' not in result or not isinstance(result['requires'], list):
                result['requires'] = []

            require_obj = {
                'symbol': node.text
            }
            result['requires'].append(require_obj)
            active_require_idx += 1
            require_form_line_no = line_no

            # attach comments from the lines above this require
            if len(single_line_comments) > 0:
                result['requires'][active_require_idx]['comments_above'] = single_line_comments
                single_line_comments = []

            # add platform if we are inside a Reader Conditional
            if inside_reader_conditional and current_reader_conditional_platform:
                result['requires'][active_require_idx]['platform'] = current_reader_conditional_platform

        # collect symbols inside of a prefix list
        elif inside_prefix_list and is_token_node2 and is_text_node:
            if not isinstance(result.get('requires'), list):
                result['requires'] = []

            namespace = str_concat3(prefix_list_prefix, '.', node.text)

            require_obj = {
                'prefixListId': current_prefix_list_id,
                'symbol': namespace
            }
            stack_push(result['requires'], require_obj)
            active_require_idx = inc(active_require_idx)
            require_symbol_idx = idx
            require_form_line_no = line_no
            inside_prefix_list = True

        # collect :require renames
        elif inside_require_form and inside_require_list and require_rename_idx > 0 and idx > require_rename_idx and is_token_node2 and is_text_node:
            stack_push(renames_tmp, node.text)

            if array_size(renames_tmp) == 2:
                itm = {}
                itm['fromSymbol'] = renames_tmp[0]
                itm['toSymbol'] = renames_tmp[1]

                if inside_reader_conditional and current_reader_conditional_platform:
                    itm['platform'] = current_reader_conditional_platform

                if not isinstance(result['requires'][active_require_idx].get('rename'), list):
                    result['requires'][active_require_idx]['rename'] = []

                stack_push(result['requires'][active_require_idx]['rename'], itm)

                renames_tmp = []

        # collect :require symbol inside of a list / vector
        elif inside_require_form and inside_require_list and idx > require_node_idx and is_token_node2 and is_text_node and require_symbol_idx == -1 and not is_keyword_node(node):
            if not isinstance(result.get('requires'), list):
                result['requires'] = []

            # five possibilities for a :require import:
            # - require symbol not inside of list / vector
            # - require symbol inside a list / vector, followed by nothing
            # - require symbol for a prefix list (need to examine the following symbols in order to know this)
            # - require symbol followed by :as
            # - require symbol followed by :refer

            next_token_inside_require_form = find_next_token_inside_require_form(nodes_arr, inc(idx))
            is_prefix_list = next_token_inside_require_form and not is_keyword_node(next_token_inside_require_form)

            if is_prefix_list:
                prefix_list_id = create_id()
                inside_prefix_list = True
                prefix_list_line_no = line_no
                prefix_list_prefix = node.text
                current_prefix_list_id = prefix_list_id

                # store the comments above this line
                # we will attach them to the first ns imported by this prefix list later
                if array_size(single_line_comments) > 0:
                    itm = {
                        'commentsAbove': single_line_comments
                    }
                    prefix_list_comments[prefix_list_id] = itm
                    single_line_comments = []
            else:
                require_obj = {
                    'symbol': node.text
                }
                stack_push(result['requires'], require_obj)
                active_require_idx = inc(active_require_idx)
                require_symbol_idx = idx
                require_form_line_no = line_no
                inside_prefix_list = False
                prefix_list_line_no = -1

                # attach comments from the lines above this require
                if array_size(single_line_comments) > 0:
                    result['requires'][active_require_idx]['commentsAbove'] = single_line_comments
                    single_line_comments = []

                # add platform if we are inside a Reader Conditional
                if inside_reader_conditional and current_reader_conditional_platform:
                    result['requires'][active_require_idx]['platform'] = current_reader_conditional_platform

        # :rename inside require
        elif inside_require_form and inside_require_list and idx > require_node_idx and is_rename_keyword(node):
            require_rename_idx = idx
            renames_tmp = []

        # collect require Strings in ClojureScript
        elif inside_require_form and inside_require_list and idx > require_node_idx and is_string_node(node):
            if not isinstance(result.get('requires'), list):
                result['requires'] = []

            require_obj = {}
            stack_push(result['requires'], require_obj)
            active_require_idx = inc(active_require_idx)
            require_form_line_no = line_no

            # attach comments from the lines above this require
            if array_size(single_line_comments) > 0:
                result['requires'][active_require_idx]['commentsAbove'] = single_line_comments
                single_line_comments = []

            # add platform if we are inside a Reader Conditional
            if inside_reader_conditional and current_reader_conditional_platform:
                result['requires'][active_require_idx]['platform'] = current_reader_conditional_platform

            # NOTE: this should always be true, but I like being defensive
            if array_size(node.children) == 3 and node.children[1].name == '.body':
                result['requires'][active_require_idx]['symbol'] = str_concat3('"', node.children[1].text, '"')
                result['requires'][active_require_idx]['symbolIsString'] = True
            else:
                # FIXME: throw here?
                pass

        # collect :import packages not inside of a list or vector
        elif inside_import_form and idx > import_node_idx and not inside_import_package_list and is_token_node2 and is_text_node:
            if 'importsObj' not in result:
                result['importsObj'] = {}

            package_parsed = parse_java_package_with_class(node.text)
            package_name = package_parsed['package']
            class_name = package_parsed['className']

            if package_name not in result['importsObj']:
                result['importsObj'][package_name] = {
                    'classes': []
                }

            stack_push(result['importsObj'][package_name]['classes'], class_name)
            active_import_package_name = package_name
            import_form_line_no = line_no

            if array_size(single_line_comments) > 0:
                result['importsObj'][package_name]['commentsAbove'] = single_line_comments
                single_line_comments = []

            # add platform if we are inside a Reader Conditional
            if inside_reader_conditional and current_reader_conditional_platform:
                result['importsObj'][package_name]['platform'] = current_reader_conditional_platform

        # collect :import classes inside of a list or vector
        elif inside_import_package_list and is_token_node2 and is_text_node:
            if not import_package_list_first_token:
                package_name = node.text
                import_package_list_first_token = package_name
                active_import_package_name = package_name
                import_form_line_no = line_no

                if 'importsObj' not in result:
                    result['importsObj'] = {}

                if package_name not in result['importsObj']:
                    result['importsObj'][package_name] = {
                        'classes': []
                    }

                if array_size(single_line_comments) > 0:
                    result['importsObj'][package_name]['commentsAbove'] = single_line_comments
                    single_line_comments = []

                # add platform if we are inside a Reader Conditional
                if inside_reader_conditional and current_reader_conditional_platform:
                    result['importsObj'][package_name]['platform'] = current_reader_conditional_platform
            else:
                stack_push(result['importsObj'][import_package_list_first_token]['classes'], node.text)

        # we are on the :gen-class node
        elif inside_gen_class and idx == gen_class_node_idx:
            result['genClass'] = {}
            result['genClass']['isEmpty'] = True

            # add platform if we are inside a Reader Conditional
            if inside_reader_conditional and current_reader_conditional_platform:
                result['genClass']['platform'] = current_reader_conditional_platform

            # add commentsAbove
            if array_size(single_line_comments) > 0:
                result['genClass']['commentsAbove'] = single_line_comments
                single_line_comments = []

            gen_class_line_no = line_no

        # :gen-class key like :main, :name, :state, :init, etc
        elif inside_gen_class and idx > gen_class_node_idx and is_text_node and gen_class_toggle == 0 and is_gen_class_keyword(node):
            result['genClass']['isEmpty'] = False

            gen_class_key_str = substr(node.text, 1)
            result['genClass'][gen_class_key_str] = {}

            # add commentsAbove if possible
            if array_size(single_line_comments) > 0:
                result['genClass'][gen_class_key_str]['commentsAbove'] = single_line_comments
                single_line_comments = []

            # gen_class_toggle = 0 means we are looking for a key
            # gen_class_toggle = 1 means we are looking for a value
            gen_class_toggle = 1

        # :gen-class :prefix value
        elif inside_gen_class and idx > gen_class_node_idx and gen_class_toggle == 1 and gen_class_key_str == 'prefix' and is_string_node(node):
            # NOTE: this should always be true, but I like being defensive
            if array_size(node.children) == 3 and node.children[1].name == '.body':
                result['genClass']['prefix']['value'] = str_concat3('"', node.children[1].text, '"')
            gen_class_toggle = 0
            gen_class_value_line_no = line_no

        # other :gen-class values
        elif inside_gen_class and idx > gen_class_node_idx and is_text_node and is_token_node2 and gen_class_toggle == 1:
            # :name, :extends, :init, :post-init, :factory, :state, :impl-ns
            if is_gen_class_name_key(gen_class_key_str):
                result['genClass'][gen_class_key_str]['value'] = node.text
                gen_class_toggle = 0
                gen_class_value_line_no = line_no
            # :main, :load-impl-ns
            elif is_gen_class_boolean_key(gen_class_key_str):
                if node.text == 'true':
                    result['genClass'][gen_class_key_str]['value'] = True
                    gen_class_toggle = 0
                    gen_class_value_line_no = line_no
                elif node.text == 'false':
                    result['genClass'][gen_class_key_str]['value'] = False
                    gen_class_toggle = 0
                    gen_class_value_line_no = line_no
                else:
                    # FIXME: throw here? this is almost certainly an error in the source
                    pass
            # FIXME: we need to handle :implements, :constructors, :methods, :exposes, :exposes-methods, here

        # increment the lineNo for the next node if we are on a newline node
        # NOTE: this lineNo variable does not account for newlines inside of multi-line strings
        # but we can ignore that for the purposes of ns parsing here
        if current_node_is_newline:
            line_no = inc(line_no)
        prev_node_is_newline = current_node_is_newline

        # increment to look at the next node
        idx = inc(idx)

        # exit if we are at the end of the nodes
        if idx >= num_nodes:
            continue_parsing_ns_form = False
        # exit if we have finished parsing the ns form
        elif ns_node_idx > 0 and not inside_ns_form and line_no >= inc(inc(ns_form_ends_line_idx)):
            continue_parsing_ns_form = False

        # end main ns parsing node loop

        return sort_ns_result(result, prefix_list_comments)




def print_comments_above(out_txt, comments_above, indentation_str):
    if isinstance(comments_above, list):
        num_comment_lines = len(comments_above)
        idx = 0
        while idx < num_comment_lines:
            comment_line = indentation_str + comments_above[idx]
            out_txt = out_txt + comment_line + '\n'
            idx = idx + 1
    return out_txt

# returns a sorted array of platform strings found on items in arr
def get_platforms_from_array(arr):
    has_default = False
    platforms = {}
    num_items = len(arr)
    idx = 0
    while idx < num_items:
        itm = arr[idx]
        if 'platform' in itm:
            if itm['platform'] == ':default':
                has_default = True
            else:
                platforms[itm['platform']] = None
        idx = idx + 1

    platforms_arr = []
    for platform_str in platforms:
        platforms_arr.append(platform_str)

    platforms_arr.sort()  # FIXME: abstract to language neutral

    if has_default:
        platforms_arr.append(':default')

    return platforms_arr

# returns true if there are only one require per platform
# this lets us use the standard reader conditional #?( instead of
# the splicing reader conditional #?@(
def only_one_require_per_platform(reqs):
    platform_counts = {}
    num_reqs = len(reqs)
    idx = 0
    keep_searching = True
    result = True
    while keep_searching:
        if idx < len(reqs) and 'platform' in reqs[idx] and isinstance(reqs[idx]['platform'], str):
            platform = reqs[idx]['platform']
            if platform != '':
                if platform in platform_counts:
                    keep_searching = False
                    result = False
                else:
                    platform_counts[platform] = 1

        idx = idx + 1
        if idx > num_reqs:
            keep_searching = False

    return result

# Returns an array of Objects filtered on the .platform key
def filter_on_platform(arr, platform):
    filtered_reqs = []
    idx = 0
    num_reqs = len(arr)
    while idx < num_reqs:
        itm = arr[idx]
        if platform is False and 'platform' not in itm:
            filtered_reqs.append(itm)
        elif isinstance(itm.get('platform'), str) and itm['platform'] == platform:
            filtered_reqs.append(arr[idx])

        idx = idx + 1
    return filtered_reqs

def format_require_line(req, initial_indentation):
    out_txt = ''

    out_txt = print_comments_above(out_txt, req.get('commentsAbove'), initial_indentation)

    out_txt = out_txt + initial_indentation
    out_txt = out_txt + '[' + req['symbol']

    if isinstance(req.get('as'), str) and req['as'] != '':
        out_txt = out_txt + ' :as ' + req['as']

    if isinstance(req.get('asAlias'), str) and req['asAlias'] != '':
        out_txt = out_txt + ' :as-alias ' + req['asAlias']

    # NOTE: this will not work if the individual :refer symbols are wrapped in a reader conditional
    if isinstance(req.get('refer'), list) and len(req['refer']) > 0:
        out_txt = out_txt + ' :refer ['
        refer_symbols = [item['symbol'] for item in req['refer']]
        out_txt = out_txt + ' '.join(refer_symbols)
        out_txt = out_txt + ']'
    elif req.get('refer') == 'all':
        out_txt = out_txt + ' :refer :all'

    # NOTE: this will not work if the individual :exclude symbols are wrapped in a reader conditional
    if isinstance(req.get('exclude'), list) and len(req['exclude']) > 0:
        out_txt = out_txt + ' :exclude ['
        exclude_symbols = [item['symbol'] for item in req['exclude']]
        out_txt = out_txt + ' '.join(exclude_symbols)
        out_txt = out_txt + ']'

    if req.get('includeMacros') is True:
        out_txt = out_txt + ' :include-macros true'
    elif req.get('includeMacros') is False:
        out_txt = out_txt + ' :include-macros false'

    if isinstance(req.get('referMacros'), list) and len(req['referMacros']) > 0:
        out_txt = out_txt + ' :refer-macros ['
        out_txt = out_txt + ' '.join(req['referMacros'])
        out_txt = out_txt + ']'

    if isinstance(req.get('rename'), list) and len(req['rename']) > 0:
        out_txt = out_txt + ' :rename {'
        out_txt = out_txt + format_renames_list(req['rename'])
        out_txt = out_txt + '}'

    out_txt = out_txt + ']'

    return out_txt

# returns an array of the available :refer-clojure keys
# valid options are: :exclude, :only, :rename
def get_refer_clojure_keys(refer_clojure):
    keys = []
    if refer_clojure:
        if 'exclude' in refer_clojure:
            keys.append(':exclude')
        if 'only' in refer_clojure:
            keys.append(':only')
        if 'rename' in refer_clojure:
            keys.append(':rename')
    return keys

def format_keyword_followed_by_list_of_symbols(kwd, symbols):
    s = kwd + ' ['
    s = s + ' '.join(symbols)
    s = s + ']'

    return s

def format_renames_list(items):
    s = ''
    num_items = len(items)
    idx = 0
    while idx < num_items:
        s = s + items[idx]['fromSymbol']
        s = s + ' '
        s = s + items[idx]['toSymbol']
        if idx + 1 < num_items:
            s = s + ', '
        idx = idx + 1
    return s

def format_refer_clojure_single_keyword(kwd, symbols_arr):
    platforms = get_platforms_from_array(symbols_arr)
    num_platforms = len(platforms)
    symbols_for_all_platforms = [item['symbol'] for item in filter_on_platform(symbols_arr, False)]
    num_symbols_for_all_platforms = len(symbols_for_all_platforms)

    # there are no reader conditionals: print all of the symbols
    if num_platforms == 0:
        s = '\n  (:refer-clojure '
        s = s + format_keyword_followed_by_list_of_symbols(kwd, symbols_for_all_platforms)
        s = s + ')'
        return s

    # all symbols are for a single platform: wrap the entire (:refer-clojure) in a single reader conditional
    elif num_platforms == 1 and num_symbols_for_all_platforms == 0:
        symbols2 = [item['symbol'] for item in symbols_arr]
        s = '\n  #?(' + platforms[0] + '\n'
        s = s + '     (:refer-clojure '
        s = s + format_keyword_followed_by_list_of_symbols(kwd, symbols2)
        s = s + '))'
        return s

    # all symbols are for specific platforms, ie: every symbol is wrapped in a reader conditional
    elif num_platforms > 1 and num_symbols_for_all_platforms == 0:
        s = '\n  (:refer-clojure\n'
        s = s + '    ' + kwd
        s = s + ' #?@('

        platform_idx = 0
        while platform_idx < num_platforms:
            platform = platforms[platform_idx]
            symbols_for_platform = [item['symbol'] for item in filter_on_platform(symbols_arr, platform)]

            s = s + format_keyword_followed_by_list_of_symbols(platform, symbols_for_platform)

            if platform_idx + 1 != num_platforms:
                if kwd == ':exclude':
                    s = s + '\n' + ' ' * 17
                elif kwd == ':only':
                    s = s + '\n' + ' ' * 14
                else:
                    # FIXME: throw error here?
                    pass

            platform_idx = platform_idx + 1

        s = s + '))'

        # FIXME: add commentAfter here?

        return s

    # we have a mix of symbols for all platforms and some for specific platforms
    else:
        s = '\n  (:refer-clojure\n'
        s = s + '    ' + kwd
        s = s + ' ['
        s = s + ' '.join(symbols_for_all_platforms)

        if kwd == ':exclude':
            s = s + '\n' + ' ' * 14
        elif kwd == ':only':
            s = s + '\n' + ' ' * 11
        else:
            # FIXME: throw error here?
            pass

        s = s + '#?@('

        platform_idx = 0
        while platform_idx < num_platforms:
            platform = platforms[platform_idx]
            symbols_for_platform = [item['symbol'] for item in filter_on_platform(symbols_arr, platform)]

            s = s + format_keyword_followed_by_list_of_symbols(platform, symbols_for_platform)
            if platform_idx + 1 != num_platforms:
                if kwd == ':exclude':
                    s = s + '\n' + ' ' * 18
                elif kwd == ':only':
                    s = s + '\n' + ' ' * 15

            platform_idx = platform_idx + 1

        s = s + ')])'

        # FIXME: add commentAfter here?

        return s

def format_refer_clojure(refer_clojure):
    keys = get_refer_clojure_keys(refer_clojure)
    num_keys = len(keys)

    # there are no :refer-clojure items, we are done
    if num_keys == 0:
        return ''
    # there is only :exclude
    elif num_keys == 1 and keys[0] == ':exclude':
        return format_refer_clojure_single_keyword(':exclude', refer_clojure['exclude'])

    # there is only :only
    elif num_keys == 1 and keys[0] == ':only':
        return format_refer_clojure_single_keyword(':only', refer_clojure['only'])

    # there is only :rename
    elif num_keys == 1 and keys[0] == ':rename':
        platforms = get_platforms_from_array(refer_clojure['rename'])
        num_platforms = len(platforms)
        non_platform_specific_renames = filter_on_platform(refer_clojure['rename'], False)
        num_non_platform_specific_renames = len(non_platform_specific_renames)
        all_renames_for_same_platform = num_non_platform_specific_renames == 0 and len(platforms) > 0

        if num_platforms == 0:
            s = '\n  (:refer-clojure :rename {'
            s = s + format_renames_list(refer_clojure['rename'])
            s = s + '})'
            return s
        elif num_platforms == 1 and all_renames_for_same_platform:
            s = '\n  #?(' + platforms[0] + '\n'
            s = s + '     (:refer-clojure :rename {'
            s = s + format_renames_list(refer_clojure['rename'])
            s = s + '}))'
            return s
        else:
            s = '\n  (:refer-clojure\n    :rename {'
            s = s + format_renames_list(non_platform_specific_renames)
            s = s + '\n             #?@('

            platform_idx = 0
            while platform_idx < num_platforms:
                platform_str = platforms[platform_idx]
                platform_renames = filter_on_platform(refer_clojure['rename'], platform_str)

                if platform_idx == 0:
                    s = s + platform_str + ' ['
                else:
                    s = s + '\n                 '
                    s = s + platform_str + ' ['
                s = s + format_renames_list(platform_renames)
                s = s + ']'

                platform_idx = platform_idx + 1

            s = s + ')})'

            return s

    # there are multiple keys, put each one on it's own line
    else:
        s = '\n  (:refer-clojure'

        if 'exclude' in refer_clojure and len(refer_clojure['exclude']) > 0:
            exclude_symbols = [item['symbol'] for item in refer_clojure['exclude']]
            s = s + '\n    '
            s = s + format_keyword_followed_by_list_of_symbols(':exclude', exclude_symbols)

        if 'only' in refer_clojure and len(refer_clojure['only']) > 0:
            only_symbols = [item['symbol'] for item in refer_clojure['only']]
            s = s + '\n    '
            s = s + format_keyword_followed_by_list_of_symbols(':only', only_symbols)

        if 'rename' in refer_clojure and len(refer_clojure['rename']) > 0:
            s = s + '\n    :rename {'
            s = s + format_renames_list(refer_clojure['rename'])
            s = s + '}'

        s = s + ')'
        return s

        # FIXME - I need to create some tests cases for this
        # return 'FIXME: handle reader conditionals for multiple :refer-clojure keys'
def format_ns(ns):
    out_txt = str_concat('(ns ', ns['ns_symbol'])

    num_require_macros = 0
    if isinstance(ns.get('require_macros'), list):
        num_require_macros = array_size(ns['require_macros'])

    num_requires = 0
    if isinstance(ns.get('requires'), list):
        num_requires = array_size(ns['requires'])

    num_imports = 0
    if isinstance(ns.get('imports'), list):
        num_imports = array_size(ns['imports'])

    has_gen_class = bool(ns.get('gen_class'))
    imports_is_last_main_form = num_imports > 0 and not has_gen_class
    require_is_last_main_form = num_requires > 0 and not imports_is_last_main_form and not has_gen_class
    require_macros_is_last_main_form = num_require_macros > 0 and num_requires == 0 and num_imports == 0 and not has_gen_class
    trailing_parens_are_printed = False

    if isinstance(ns.get('docstring'), str):
        out_txt = str_concat(out_txt, '\n  "')
        out_txt = str_concat(out_txt, ns['docstring'])
        out_txt = str_concat(out_txt, '"')

    if isinstance(ns.get('ns_metadata'), list):
        num_metadata_itms = array_size(ns['ns_metadata'])
        if num_metadata_itms > 0:
            metadata_itms_idx = 0

            out_txt = str_concat(out_txt, '\n  {')
            while metadata_itms_idx < num_metadata_itms:
                metadata_itm = ns['ns_metadata'][metadata_itms_idx]
                out_txt = str_concat3(out_txt, metadata_itm['key'], ' ')
                out_txt = str_concat(out_txt, metadata_itm['value'])

                metadata_itms_idx = inc(metadata_itms_idx)

                if metadata_itms_idx != num_metadata_itms:
                    out_txt = str_concat(out_txt, '\n   ')

            out_txt = str_concat(out_txt, '}')

    # FIXME - we need reader conditionals for :refer-clojure here
    # FIXME - comments for :refer-clojure
    if ns.get('refer_clojure'):
        out_txt = str_concat(out_txt, format_refer_clojure(ns['refer_clojure']))

    if num_require_macros > 0:
        cljs_platform_require_macros = filter_on_platform(ns['require_macros'], ':cljs')
        wrap_require_macros_with_reader_conditional = array_size(cljs_platform_require_macros) == num_require_macros
        rm_last_line_comment_after = None

        rm_indentation = '   '
        if wrap_require_macros_with_reader_conditional:
            out_txt = str_concat(out_txt, '\n')
            out_txt = str_concat(out_txt, '  #?(:cljs\n')
            out_txt = print_comments_above(out_txt, ns.get('require_macros_comments_above'), '     ')
            out_txt = str_concat(out_txt, '     (:require-macros\n')

            rm_indentation = '      '
        else:
            out_txt = str_concat(out_txt, '\n')
            out_txt = print_comments_above(out_txt, ns.get('require_macros_comments_above'), '  ')
            out_txt = str_concat(out_txt, '  (:require-macros\n')

        rm_idx = 0
        while rm_idx < num_require_macros:
            rm = ns['require_macros'][rm_idx]
            is_last_require_macro_line = inc(rm_idx) == num_require_macros
            out_txt = str_concat(out_txt, format_require_line(rm, rm_indentation))

            if is_non_blank_string(rm.get('comment_after')):
                if is_last_require_macro_line:
                    rm_last_line_comment_after = rm['comment_after']
                else:
                    out_txt = str_concat3(out_txt, ' ', rm['comment_after'])

            if not is_last_require_macro_line:
                out_txt = str_concat(out_txt, '\n')

            rm_idx = inc(rm_idx)

        if not require_macros_is_last_main_form and not wrap_require_macros_with_reader_conditional:
            out_txt = str_concat(out_txt, ')')
        elif not require_macros_is_last_main_form and wrap_require_macros_with_reader_conditional:
            out_txt = str_concat(out_txt, '))')
        elif require_macros_is_last_main_form and not wrap_require_macros_with_reader_conditional:
            out_txt = str_concat(out_txt, '))')
            trailing_parens_are_printed = True
        elif require_macros_is_last_main_form and wrap_require_macros_with_reader_conditional:
            out_txt = str_concat(out_txt, ')))')
            trailing_parens_are_printed = True

        if is_non_blank_string(rm_last_line_comment_after):
            out_txt = str_concat3(out_txt, ' ', rm_last_line_comment_after)

    if num_requires > 0:
        close_require_paren_trail = ')'
        last_require_has_comment = False
        last_require_comment = None
        req_platforms = get_platforms_from_array(ns['requires'])
        num_platforms = array_size(req_platforms)

        all_requires_under_one_platform = False
        if num_platforms == 1:
            one_platform_requires = filter_on_platform(ns['requires'], req_platforms[0])
            if num_requires == array_size(one_platform_requires):
                all_requires_under_one_platform = True

        require_line_indentation = '   '
        if all_requires_under_one_platform:
            out_txt = str_concat(out_txt, '\n  #?(')
            out_txt = str_concat(out_txt, req_platforms[0])

            if isinstance(ns.get('require_comments_above'), list) and array_size(ns['require_comments_above']) > 0:
                out_txt = str_concat(out_txt, '\n     ')
                out_txt = str_concat(out_txt, str_join(ns['require_comments_above'], '\n     '))

            out_txt = str_concat(out_txt, '\n     (:require')
            if isinstance(ns.get('require_comment_after'), str) and ns['require_comment_after'] != '':
                out_txt = str_concat3(out_txt, ' ', ns['require_comment_after'])
            out_txt = str_concat(out_txt, '\n')

            require_line_indentation = '      '
        else:
            if isinstance(ns.get('require_comments_above'), list) and array_size(ns['require_comments_above']) > 0:
                out_txt = str_concat(out_txt, '\n  ')
                out_txt = str_concat(out_txt, str_join(ns['require_comments_above'], '\n  '))
            out_txt = str_concat(out_txt, '\n  (:require\n')

        requires_idx = 0
        while requires_idx < num_requires:
            req = ns['requires'][requires_idx]
            # NOTE: I am not sure this works correctly with reader conditionals
            is_last_require1 = inc(requires_idx) == num_requires

            if not req.get('platform') or all_requires_under_one_platform:
                out_txt = str_concat(out_txt, format_require_line(req, require_line_indentation))

                if req.get('comment_after') and not is_last_require1:
                    out_txt = str_concat(out_txt, ' ')
                    out_txt = str_concat(out_txt, req['comment_after'])
                    out_txt = str_concat(out_txt, '\n')
                elif is_last_require1 and req.get('comment_after') and require_is_last_main_form and not all_requires_under_one_platform:
                    close_require_paren_trail = str_concat(')) ', req['comment_after'])
                    trailing_parens_are_printed = True
                elif is_last_require1 and req.get('comment_after') and all_requires_under_one_platform:
                    last_require_comment = req['comment_after']
                    last_require_has_comment = True
                elif is_last_require1 and req.get('comment_after'):
                    close_require_paren_trail = str_concat(') ', req['comment_after'])
                elif is_last_require1 and not req.get('comment_after'):
                    close_require_paren_trail = ')'
                else:
                    out_txt = str_concat(out_txt, '\n')

            requires_idx = inc(requires_idx)

        platform_idx = 0

        require_block_has_reader_conditionals = num_platforms > 0
        use_standard_reader_conditional = only_one_require_per_platform(ns['requires'])

        if not all_requires_under_one_platform:
            # use standard reader conditional #?(
            if use_standard_reader_conditional:
                while platform_idx < num_platforms:
                    platform = req_platforms[platform_idx]

                    if platform_idx == 0:
                        out_txt = str_trim(out_txt)
                        out_txt = str_concat3(out_txt, '\n   #?(', platform)
                        out_txt = str_concat(out_txt, ' ')
                    else:
                        out_txt = str_concat(out_txt, '\n      ')
                        out_txt = str_concat3(out_txt, platform, ' ')

                    # only look at requires for this platform
                    platform_requires = filter_on_platform(ns['requires'], platform)
                    req = platform_requires[0]
                    out_txt = str_concat(out_txt, format_require_line(req, ''))

                    # FIXME: need to add comments_before and comments_after here

                    platform_idx = inc(platform_idx)
            # use splicing reader conditional #?@(
            else:
                while platform_idx < num_platforms:
                    platform = req_platforms[platform_idx]
                    is_last_platform = inc(platform_idx) == num_platforms

                    if platform_idx == 0:
                        out_txt = str_trim(out_txt)
                        out_txt = str_concat(out_txt, '\n   #?@(')
                        out_txt = str_concat3(out_txt, platform, '\n       [')
                    else:
                        out_txt = str_concat(out_txt, '\n\n       ')
                        out_txt = str_concat3(out_txt, platform, '\n       [')

                    # only look at requires for this platform
                    platform_requires = filter_on_platform(ns['requires'], platform)
                    num_filtered_reqs = array_size(platform_requires)
                    printed_first_req_line = False
                    print_platform_closing_bracket = True
                    req_idx2 = 0
                    while req_idx2 < num_filtered_reqs:
                        req = platform_requires[req_idx2]
                        is_last_require_for_this_platform = inc(req_idx2) == num_filtered_reqs

                        if printed_first_req_line:
                            out_txt = str_concat(out_txt, format_require_line(req, '        '))
                        else:
                            printed_first_req_line = True
                            out_txt = str_concat(out_txt, format_require_line(req, ''))

                        if req.get('comment_after') and not is_last_require_for_this_platform:
                            out_txt = str_concat(out_txt, ' ')
                            out_txt = str_concat(out_txt, req['comment_after'])
                            out_txt = str_concat(out_txt, '\n')
                        elif req.get('comment_after') and is_last_require_for_this_platform and not is_last_platform:
                            out_txt = str_concat3(out_txt, '] ', req['comment_after'])
                            print_platform_closing_bracket = False
                        elif req.get('comment_after') and is_last_require_for_this_platform and (is_last_platform or require_is_last_main_form):
                            last_require_has_comment = True
                            last_require_comment = req['comment_after']
                        elif is_last_require_for_this_platform and req.get('comment_after'):
                            close_require_paren_trail = str_concat(') ', req['comment_after'])
                        elif is_last_require_for_this_platform and not req.get('comment_after'):
                            close_require_paren_trail = ']'
                        else:
                            out_txt = str_concat(out_txt, '\n')

                        req_idx2 = inc(req_idx2)

                    if print_platform_closing_bracket:
                        out_txt = str_concat(out_txt, ']')

                    platform_idx = inc(platform_idx)

        # closeRequireParenTrail can be one of six options:
        # - )             <-- no reader conditional, no comment on the last item, not the last main form
        # - ) <comment>   <-- no reader conditional, comment on the last itm, not the last main form
        # - ))            <-- reader conditional, no comment on the last itm, not the last main form
        # - )) <comment>  <-- reader conditional, comment on last itm, not the last main form
        # - )))           <-- reader conditional, no comment on last itm, :require is last main form
        # - ))) <comment> <-- reader conditional, comment on last itm, :require is last main form
        if not require_block_has_reader_conditionals and not last_require_has_comment and not require_is_last_main_form:
            close_require_paren_trail = ')'
        elif not require_block_has_reader_conditionals and last_require_has_comment and not require_is_last_main_form:
            close_require_paren_trail = str_concat(') ', last_require_comment)
        elif require_block_has_reader_conditionals and not last_require_has_comment and not require_is_last_main_form:
            close_require_paren_trail = '))'
        elif require_block_has_reader_conditionals and last_require_has_comment and not require_is_last_main_form:
            close_require_paren_trail = str_concat(')) ', last_require_comment)
        elif require_block_has_reader_conditionals and not last_require_has_comment and require_is_last_main_form:
            close_require_paren_trail = ')))'
            trailing_parens_are_printed = True
        elif require_block_has_reader_conditionals and last_require_has_comment and require_is_last_main_form:
            close_require_paren_trail = str_concat('))) ', last_require_comment)
            trailing_parens_are_printed = True

        out_txt = str_trim(out_txt)
        out_txt = str_concat(out_txt, close_require_paren_trail)
    # end :require printing

    if num_imports > 0:
        # collect imports that are platform-specific (or not)
        non_platform_specific_imports = filter_on_platform(ns['imports'], False)
        num_non_platform_specific_imports = array_size(non_platform_specific_imports)
        import_platforms = get_platforms_from_array(ns['imports'])
        num_import_platforms = array_size(import_platforms)

        last_import_line_comment_after = None
        is_import_keyword_printed = False

        imports_idx = 0
        while imports_idx < num_non_platform_specific_imports:
            if not is_import_keyword_printed:
                out_txt = str_concat(out_txt, '\n  (:import\n')
                is_import_keyword_printed = True

            imp = non_platform_specific_imports[imports_idx]
            is_last_import = inc(imports_idx) == num_non_platform_specific_imports

            out_txt = str_concat3(out_txt, '   (', imp['package'])

            num_classes = array_size(imp['classes'])
            class_name_idx = 0
            while class_name_idx < num_classes:
                class_name = imp['classes'][class_name_idx]
                out_txt = str_concat3(out_txt, ' ', class_name)

                class_name_idx = inc(class_name_idx)

            out_txt = str_concat(out_txt, ')')

            if is_non_blank_string(imp.get('comment_after')):
                out_txt = str_concat3(out_txt, ' ', imp['comment_after'])

            if not is_last_import:
                out_txt = str_concat(out_txt, '\n')

            imports_idx = inc(imports_idx)

        platform_idx = 0
        is_first_platform = True
        import_section_has_reader_conditionals = num_import_platforms > 0
        place_reader_conditional_outside_of_import = num_import_platforms == 1 and num_non_platform_specific_imports == 0

        while platform_idx < num_import_platforms:
            platform_str = import_platforms[platform_idx]

            if place_reader_conditional_outside_of_import:
                out_txt = str_concat(out_txt, '\n  #?(')
                out_txt = str_concat(out_txt, platform_str)
                out_txt = str_concat(out_txt, '\n')
                out_txt = str_concat(out_txt, '     (:import\n')
                out_txt = str_concat(out_txt, '      ')
                is_import_keyword_printed = True
            elif is_first_platform:
                if not is_import_keyword_printed:
                    out_txt = str_concat(out_txt, '\n  (:import')
                    is_import_keyword_printed = True
                out_txt = str_concat3(out_txt, '\n   #?@(', platform_str)
                out_txt = str_concat(out_txt, '\n       [')
                is_first_platform = False
            else:
                out_txt = str_concat3(out_txt, '\n\n       ', platform_str)
                out_txt = str_concat(out_txt, '\n       [')

            imports_for_this_platform = filter_on_platform(ns['imports'], platform_str)
            idx2 = 0
            num_imports2 = array_size(imports_for_this_platform)
            while idx2 < num_imports2:
                imp = imports_for_this_platform[idx2]
                is_last_import2 = inc(idx2) == num_imports2

                out_txt = str_concat(out_txt, '(')
                out_txt = str_concat(out_txt, imp['package'])
                out_txt = str_concat(out_txt, ' ')
                out_txt = str_concat(out_txt, str_join(imp['classes'], ' '))
                out_txt = str_concat(out_txt, ')')

                if is_last_import2:
                    if not place_reader_conditional_outside_of_import:
                        out_txt = str_concat(out_txt, ']')
                    if is_non_blank_string(imp.get('comment_after')):
                        last_import_line_comment_after = imp['comment_after']
                else:
                    if is_non_blank_string(imp.get('comment_after')):
                        out_txt = str_concat3(out_txt, ' ', imp['comment_after'])
                    if place_reader_conditional_outside_of_import:
                        out_txt = str_concat(out_txt, '\n      ')
                    else:
                        out_txt = str_concat(out_txt, '\n        ')

                idx2 = inc(idx2)

            platform_idx = inc(platform_idx)

        close_import_paren_trail = ')'
        if imports_is_last_main_form and import_section_has_reader_conditionals:
            close_import_paren_trail = ')))'
            trailing_parens_are_printed = True
        elif imports_is_last_main_form and not import_section_has_reader_conditionals:
            close_import_paren_trail = '))'
            trailing_parens_are_printed = True

        out_txt = str_concat(out_txt, close_import_paren_trail)

        if is_non_blank_string(last_import_line_comment_after):
            out_txt = str_concat3(out_txt, ' ', last_import_line_comment_after)
    # end :import section

    if has_gen_class:
        gen_class_indentation_level = 2
        out_txt = str_concat(out_txt, '\n')

        is_gen_class_behind_reader_conditional = ns['gen_class'].get('platform') == ':clj'

        if is_gen_class_behind_reader_conditional:
            out_txt = str_concat(out_txt, '  #?(:clj\n')
            gen_class_indentation_level = 5

        indentation_str = repeat_string(' ', gen_class_indentation_level)
        out_txt = print_comments_above(out_txt, ns['gen_class'].get('comments_above'), indentation_str)

        out_txt = str_concat(out_txt, indentation_str)
        out_txt = str_concat(out_txt, '(:gen-class')

        comment_after_gen_class = None

        if ns['gen_class'].get('is_empty'):
            if is_non_blank_string(ns['gen_class'].get('comment_after')):
                comment_after_gen_class = ns['gen_class']['comment_after']
        else:
            if is_non_blank_string(ns['gen_class'].get('comment_after')):
                out_txt = str_concat3(out_txt, ' ', ns['gen_class']['comment_after'])

            gen_class_value_indentation_level = inc(gen_class_indentation_level)
            indentation_str2 = repeat_string(' ', gen_class_value_indentation_level)

            # print the :gen-class keys in the order in which they appear in the clojure.core.genclass documentation
            # https://github.com/clojure/clojure/blob/clojure-1.11.1/src/clj/clojure/genclass.clj#L507
            idx3 = 0
            num_gen_class_keys = array_size(gen_class_keys)
            while idx3 < num_gen_class_keys:
                gen_class_key = gen_class_keys[idx3]
                gen_class_value = ns['gen_class'].get(gen_class_key)

                if gen_class_value:
                    # print the comment from the previous line if necessary
                    if is_non_blank_string(comment_after_gen_class):
                        out_txt = str_concat3(out_txt, ' ', comment_after_gen_class)
                        comment_after_gen_class = None

                    out_txt = str_concat(out_txt, '\n')

                    out_txt = print_comments_above(out_txt, gen_class_value.get('comments_above'), indentation_str2)

                    out_txt = str_concat(out_txt, indentation_str2)
                    out_txt = str_concat3(out_txt, ':', gen_class_key)
                    out_txt = str_concat3(out_txt, ' ', gen_class_value['value'])

                    if is_non_blank_string(gen_class_value.get('comment_after')):
                        comment_after_gen_class = gen_class_value['comment_after']

                idx3 = inc(idx3)

        if not is_gen_class_behind_reader_conditional and not comment_after_gen_class:
            out_txt = str_concat(out_txt, '))')
            trailing_parens_are_printed = True
        elif is_gen_class_behind_reader_conditional and not comment_after_gen_class:
            out_txt = str_concat(out_txt, ')))')
            trailing_parens_are_printed = True
        elif not is_gen_class_behind_reader_conditional and is_non_blank_string(comment_after_gen_class):
            out_txt = str_concat3(out_txt, ')) ', comment_after_gen_class)
            trailing_parens_are_printed = True
        elif is_gen_class_behind_reader_conditional and is_non_blank_string(comment_after_gen_class):
            out_txt = str_concat3(out_txt, '))) ', comment_after_gen_class)
            trailing_parens_are_printed = True
    # end :gen-class section

    if not trailing_parens_are_printed:
        out_txt = str_concat(out_txt, ')')

    return out_txt


# Continuation of the format() function, with the input text parsed into nodes
# and ns form parsed.

def format_nodes(nodes_arr, parsed_ns):
    num_nodes = len(nodes_arr)

    paren_nesting_depth = 0
    idx = 0
    out_txt = ''
    output_txt_contains_chars = False
    line_txt = ''
    line_idx = 0
    inside_ns_form = False
    line_idx_of_closing_ns_form = -1
    ns_start_string_idx = -1
    ns_end_string_idx = -1
    tagged_node_idx = -1
    ignore_nodes_start_id = -1
    ignore_nodes_end_id = -1
    inside_the_ignore_zone = False

    paren_stack = []
    nodes_we_have_printed_on_this_line = []

    col_idx = 0
    while idx < num_nodes:
        node = nodes_arr[idx]

        if ignore_nodes_start_id > 0 and node['id'] == ignore_nodes_start_id:
            inside_the_ignore_zone = True

            # dump the current line_txt when we start the ignore zone
            out_txt = out_txt + line_txt
            line_txt = ''

        if inside_the_ignore_zone:
            if isinstance(node['text'], str) and node['text'] != '':
                out_txt = out_txt + node['text']

            if node['id'] == ignore_nodes_end_id:
                ignore_nodes_start_id = -1
                ignore_nodes_end_id = -1
                inside_the_ignore_zone = False
        else:
            # record original column indexes for the first line
            if idx == 0:
                nodes_arr = record_original_col_indexes(nodes_arr, idx)

            if ns_start_string_idx == -1 and paren_nesting_depth == 1 and is_ns_node(node):
                inside_ns_form = True
                ns_start_string_idx = len(out_txt + line_txt)

            next_text_node = find_next_node_with_text(nodes_arr, idx + 1)
            is_last_node = idx + 1 >= num_nodes

            current_node_is_whitespace = is_whitespace_node(node)
            current_node_is_newline = is_newline_node(node)
            current_node_is_tag = is_tag_node(node)
            skip_printing_this_node = False

            if is_standard_clj_ignore_keyword(node) and idx > 1:
                prev_node1 = find_prev_node_with_text(nodes_arr, idx, node['id'])
                prev_node2 = None
                if prev_node1:
                    prev_node2 = find_prev_node_with_text(nodes_arr, idx, prev_node1['id'])

                is_discard_map = prev_node1['name'] == '.open' and prev_node1['text'] == '{' and prev_node2 and is_discard_node(prev_node2)

                if is_discard_node(prev_node1) or (is_whitespace_node(prev_node1) and is_discard_node(prev_node2)):
                    # look forward to find the next node with text
                    next_ignore_node = find_next_non_whitespace_node(nodes_arr, idx + 1)

                    # if parens or brackets or something with children, then find the closing node id
                    if isinstance(next_ignore_node.get('children'), list) and len(next_ignore_node['children']) > 0:
                        closing_node = next_ignore_node['children'][-1]
                        ignore_nodes_start_id = next_ignore_node['id']
                        ignore_nodes_end_id = closing_node['id']

                    # if a node without children, then just don't format it
                    else:
                        next_immediate_node = nodes_arr[idx + 1]
                        ignore_nodes_start_id = next_immediate_node['id']
                        ignore_nodes_end_id = next_ignore_node['id']
                elif is_discard_map:
                    # find the opening { and closing } for this form
                    opening_brace_node = find_prev_node_with_predicate(nodes_arr, idx, is_opening_brace_node)
                    closing_brace_node_id = opening_brace_node['children'][2]['id']

                    start_ignore_node = find_next_node_with_predicate_after_specific_node(nodes_arr, idx, always_true, closing_brace_node_id)
                    first_node_inside_ignore_zone = find_next_node_with_predicate_after_specific_node(nodes_arr, idx, always_true, start_ignore_node['id'])

                    # if parens or brackets or something with children, then find the closing node id
                    if isinstance(first_node_inside_ignore_zone.get('children'), list) and len(first_node_inside_ignore_zone['children']) > 0:
                        closing_node = first_node_inside_ignore_zone['children'][-1]
                        ignore_nodes_start_id = start_ignore_node['id']
                        ignore_nodes_end_id = closing_node['id']

                    # if a node without children, then just don't format it
                    else:
                        ignore_nodes_start_id = start_ignore_node['id']
                        ignore_nodes_end_id = first_node_inside_ignore_zone['id']

            if is_paren_opener(node):
                # we potentially need to add this opener node to the current opening_line_nodes
                # before we push into the next paren_stack
                top_of_the_paren_stack = stack_peek(paren_stack, 0)
                if top_of_the_paren_stack:
                    on_opening_line_of_paren_stack = line_idx == top_of_the_paren_stack['_paren_opener_line_idx']
                    if on_opening_line_of_paren_stack:
                        node['_col_idx'] = col_idx
                        node['_line_idx'] = line_idx
                        top_of_the_paren_stack['_opening_line_nodes'].append(node)

                paren_nesting_depth += 1

                # attach some extra information to this node and push it onto the paren_stack
                paren_stack_node = node.copy()
                paren_stack_node['_col_idx'] = col_idx
                paren_stack_node['_next_with_text'] = next_text_node
                paren_stack_node['_paren_opener_line_idx'] = line_idx
                # an array of nodes on the first line of this paren_stack
                # used to determine if Rule 3 indentation applies
                paren_stack_node['_opening_line_nodes'] = []
                paren_stack_node['_rule3_active'] = False
                paren_stack_node['_rule3_num_spaces'] = 0
                paren_stack_node['_rule3_search_complete'] = False

                paren_stack.append(paren_stack_node)

                # remove whitespace after an opener (remove-surrounding-whitespace?)
                if is_whitespace_node(next_text_node):
                    # FIXME: skip this via index instead of modifying the tree like this
                    next_text_node['text'] = ''
            elif is_paren_closer(node):
                # NOTE: this code is duplicated when we look forward to close paren_trails
                paren_nesting_depth -= 1
                paren_stack.pop()

                # flag the end of the ns form
                if inside_ns_form and paren_nesting_depth == 0:
                    inside_ns_form = False
                    ns_end_string_idx = len(out_txt + line_txt)
                    line_idx_of_closing_ns_form = line_idx

            # flag the index of a tagged literal node so we can mark the next one if necessary
            if node['name'] == '.tag':
                tagged_node_idx = idx

            # add nodes to the top of the paren_stack if we are on the opening line
            top_of_the_paren_stack = stack_peek(paren_stack, 0)
            if top_of_the_paren_stack and node_contains_text(node):
                on_opening_line_of_paren_stack = line_idx == top_of_the_paren_stack['_paren_opener_line_idx']
                if on_opening_line_of_paren_stack:
                    if tagged_node_idx == idx - 1:
                        node['_node_is_tag_literal'] = True

                    node['_col_idx'] = col_idx
                    node['_line_idx'] = line_idx
                    top_of_the_paren_stack['_opening_line_nodes'].append(node)

            # remove whitespace before a closer (remove-surrounding-whitespace?)
            if current_node_is_whitespace and not current_node_is_newline and is_paren_closer(next_text_node):
                skip_printing_this_node = True

            # If we are inside of a paren_stack and hit a newline,
            # look forward to see if we can close the current paren_trail.
            # ie: slurp closing parens onto the current line
            paren_stack_size = len(paren_stack)
            if paren_stack_size > 0 and not inside_ns_form:
                is_comment_followed_by_newline = is_comment_node(node) and next_text_node and is_newline_node(next_text_node)
                is_newline = is_newline_node(node)
                has_commas_after_newline2 = has_commas_after_newline(node) or (next_text_node and has_commas_after_newline(next_text_node))

                look_forward_to_slurp_nodes = False
                if has_commas_after_newline2:
                    look_forward_to_slurp_nodes = False
                elif is_comment_followed_by_newline:
                    look_forward_to_slurp_nodes = True
                elif is_newline:
                    look_forward_to_slurp_nodes = True

                if look_forward_to_slurp_nodes:
                    # look forward and grab any closers nodes that may be slurped up
                    paren_trail_closers = find_forward_closing_parens(nodes_arr, idx + 1)

                    # If we have printed a whitespace node just before this, we may need to remove it and then re-print
                    last_node_we_printed = nodes_we_have_printed_on_this_line[-1] if nodes_we_have_printed_on_this_line else None
                    line_txt_has_been_right_trimmed = False
                    if last_node_we_printed and is_whitespace_node(last_node_we_printed):
                        line_txt = line_txt.rstrip()
                        line_txt_has_been_right_trimmed = True

                    paren_trail_closer_idx = 0
                    num_paren_trail_closers = len(paren_trail_closers)

                    while paren_trail_closer_idx < num_paren_trail_closers:
                        paren_trail_closer_node = paren_trail_closers[paren_trail_closer_idx]

                        if is_paren_closer(paren_trail_closer_node):
                            # NOTE: we are adjusting the current line here, but we do not update the nodes_we_have_printed_on_this_line
                            # because we cannot have a Rule 3 alignment to a closer node
                            line_txt += paren_trail_closer_node['text']

                            paren_trail_closer_node['text'] = ''
                            paren_trail_closer_node['_was_slurped_up'] = True

                            paren_nesting_depth -= 1
                            paren_stack.pop()

                        paren_trail_closer_idx += 1

                    # re-print the whitespace node if necessary
                    if line_txt_has_been_right_trimmed:
                        line_txt += last_node_we_printed['text']

            if current_node_is_newline:
                # record the original column indexes for the next line
                nodes_arr = record_original_col_indexes(nodes_arr, idx)

                num_spaces_on_next_line = num_spaces_after_newline(node)

                # Have we already slurped up everything on the next line?
                all_next_line_nodes_were_slurped_up = are_forward_nodes_already_slurped(nodes_arr, idx + 1)

                next_line_contains_only_one_comment = is_next_line_a_comment_line(nodes_arr, idx + 1)
                next_line_comment_col_idx = -1
                if next_line_contains_only_one_comment:
                    next_line_comment_col_idx = num_spaces_on_next_line

                is_double_newline = '\n\n' in node['text']
                newline_str = '\n'
                if is_double_newline:
                    newline_str = '\n\n'

                # print the current line and calculate the next line's indentation level
                if output_txt_contains_chars:
                    top_of_the_paren_stack = stack_peek(paren_stack, 0)

                    # Check for Rule 3:
                    # Are we inside of a paren_stack that crosses into the next line?
                    # And have not already done a "Rule 3" check for this paren_stack?
                    if top_of_the_paren_stack and not top_of_the_paren_stack['_rule3_search_complete']:
                        search_for_alignment_node = True
                        # NOTE: we can start this index at 1 because we will always want to skip at least the first node
                        opening_line_node_idx = 1

                        # we must be past the first whitespace node in order to look for Rule 3 alignment nodes
                        past_first_whitespace_node = False

                        num_opening_line_nodes = len(top_of_the_paren_stack['_opening_line_nodes'])
                        if num_opening_line_nodes > 2:
                            while search_for_alignment_node:
                                opening_line_node = top_of_the_paren_stack['_opening_line_nodes'][opening_line_node_idx]
                                if opening_line_node:
                                    # Is the first node on this new line vertically aligned with any of the nodes
                                    # on the line above that are in the same paren stack?
                                    if past_first_whitespace_node and is_node_with_non_blank_text(opening_line_node) and opening_line_node['_orig_col_idx'] == num_spaces_on_next_line:
                                        # Rule 3 is activated 👍
                                        top_of_the_paren_stack['_rule3_active'] = True

                                        # NOTE: we use the original _col_idx of this node in order to determine Rule 3 alignment,
                                        # but we use the _printed_col_idx of this node to determine the number of leading spaces
                                        top_of_the_paren_stack['_rule3_num_spaces'] = opening_line_node['_printed_col_idx']

                                        # edge case: align tagged literals to the # char
                                        if opening_line_node.get('_node_is_tag_literal'):
                                            top_of_the_paren_stack['_rule3_num_spaces'] -= 1

                                        # we are done searching at this point
                                        search_for_alignment_node = False
                                    elif not past_first_whitespace_node and is_whitespace_node(opening_line_node):
                                        past_first_whitespace_node = True

                                opening_line_node_idx += 1
                                if opening_line_node_idx >= num_opening_line_nodes:
                                    search_for_alignment_node = False

                        # only check for Rule 3 alignment once per paren_stack
                        top_of_the_paren_stack['_rule3_search_complete'] = True

                    # Do we have a comment line that looks vertically aligned with nodes on the previous line?
                    # NOTE: this is basically "Rule 3" for single line comments
                    col_idx_of_single_line_comment_alignment_node = -1
                    comment_looks_aligned_with_previous_form = False
                    if next_line_contains_only_one_comment:
                        idx2 = 0
                        num_prev_line_nodes = len(nodes_we_have_printed_on_this_line)
                        while idx2 < num_prev_line_nodes:
                            prev_line_node = nodes_we_have_printed_on_this_line[idx2]
                            prev_node2 = None
                            if idx2 > 0:
                                prev_node2 = nodes_we_have_printed_on_this_line[idx2 - 1]

                            is_possible_alignment_node = False
                            if is_node_with_non_blank_text(prev_line_node):
                                if not prev_node2 or (prev_node2 and not is_paren_opener(prev_node2)):
                                    is_possible_alignment_node = True

                            if is_possible_alignment_node and next_line_comment_col_idx == prev_line_node['_orig_col_idx']:
                                col_idx_of_single_line_comment_alignment_node = prev_line_node['_printed_col_idx']
                                comment_looks_aligned_with_previous_form = True
                                idx2 = num_prev_line_nodes  # exit the loop

                            idx2 += 1

                    num_spaces = 0
                    # If we are inside a paren_stack and Rule 3 has been activated, use that first.
                    if top_of_the_paren_stack and top_of_the_paren_stack['_rule3_active']:
                        num_spaces = top_of_the_paren_stack['_rule3_num_spaces']

                    # Comment lines that are vertically aligned with a node from the line above --> align to that node
                    elif next_line_contains_only_one_comment and comment_looks_aligned_with_previous_form:
                        num_spaces = col_idx_of_single_line_comment_alignment_node

                    # Comment lines that are outside of a paren_stack, and have no obvious relation to lines above them:
                    # keep their current indentation
                    elif next_line_contains_only_one_comment and not top_of_the_paren_stack:
                        num_spaces = num_spaces_on_next_line

                    # Else apply regular fixed indentation rules based on the paren_stack depth (ie: Tonsky rules)
                    else:
                        num_spaces = num_spaces_for_indentation(top_of_the_paren_stack)

                    indentation_str = ' ' * num_spaces

                    # If we have slurped up all of the nodes on this line, we can remove it.
                    if all_next_line_nodes_were_slurped_up:
                        newline_str = ''
                        indentation_str = ''

                    if is_comma_node(node):
                        next_line_comma_trail = remove_leading_whitespace(node['text'])
                        trimmed_comma_trail = next_line_comma_trail.rstrip()
                        indentation_str += trimmed_comma_trail

                    # add this line to the out_txt and reset line_txt
                    if line_txt.strip() != '':
                        out_txt += line_txt
                    out_txt += newline_str

                    line_txt = indentation_str
                    nodes_we_have_printed_on_this_line = []

                    # reset the col_idx
                    col_idx = len(indentation_str)

                    # increment the line_idx
                    line_idx += 1
                    if is_double_newline:
                        line_idx += 1

                # we have taken care of printing this node, skip the "normal" printing step
                skip_printing_this_node = True
            # end current_node_is_newline

            if (node_contains_text(node) or current_node_is_tag) and not skip_printing_this_node:
                is_token_followed_by_opener = is_token_node(node) and next_text_node and is_paren_opener(next_text_node)
                is_paren_closer_followed_by_text = is_paren_closer(node) and next_text_node and (is_token_node(next_text_node) or is_paren_opener(next_text_node))
                add_space_after_this_node = is_token_followed_by_opener or is_paren_closer_followed_by_text

                node_txt = node['text']
                if current_node_is_tag:
                    node_txt = '#'
                elif is_comment_node(node):
                    if comment_needs_space_inside(node_txt):
                        node_txt = re.sub(r'^(;+)([^ ])', r'\1 \2', node_txt)
                    if comment_needs_space_before(line_txt, node_txt):
                        node_txt = ' ' + node_txt

                # if there is a whitespace node as the first or last node, do not print it
                if current_node_is_whitespace and (is_last_node or not output_txt_contains_chars):
                    skip_printing_this_node = True

                # do not print a comment node on the last line of the ns form
                # (this is handled by the ns_format function)
                elif is_comment_node(node) and parsed_ns['comment_outside_ns_form'] == node['text'] and line_idx == line_idx_of_closing_ns_form:
                    skip_printing_this_node = True
                elif current_node_is_whitespace and line_idx == line_idx_of_closing_ns_form:
                    skip_printing_this_node = True
                elif node.get('_skip_printing_this_node') == True:
                    skip_printing_this_node = True

                # add the text of this node to the current line
                if not skip_printing_this_node:
                    line_length_before_printing_node = len(line_txt)
                    line_txt += node_txt

                    if line_txt != '':
                        output_txt_contains_chars = True

                    # add the printed col_idx to this node
                    node['_printed_col_idx'] = line_length_before_printing_node
                    node['_printed_line_idx'] = line_idx

                    nodes_we_have_printed_on_this_line.append(node)

                if add_space_after_this_node:
                    line_txt += ' '

                # update the col_idx
                col_idx += len(node_txt)

        # end !inside_the_ignore_zone

        idx += 1
    # end looping through the nodes

    # add the last line to out_txt if necessary
    if line_txt != '':
        out_txt += line_txt

    # replace the ns form with our formatted version
    if ns_start_string_idx > 0:
        head_str = out_txt[:ns_start_string_idx - 1]

        ns_str = None
        try:
            ns_str = format_ns(parsed_ns)
        except Exception as e:
            return {
                'status': 'error',
                'reason': str(e)
            }

        tail_str = ''
        if ns_end_string_idx > 0:
            tail_str = out_txt[ns_end_string_idx + 1:]

        out_txt = head_str + ns_str + tail_str

    # remove any leading or trailing whitespace
    out_txt = out_txt.strip()

    return {
        'status': 'success',
        'out': out_txt
    }

# Parses input_txt (Clojure code) and returns a String of it formatted according
# to Standard Clojure Style.
def format(input_txt):
    # replace any CRLF with LF before we do anything
    input_txt = crlf_to_lf(input_txt)

    # FIXME: wrap this in try/catch and return error code if found
    tree = parse(input_txt)
    nodes_arr = flatten_tree(tree)

    ignore_file = look_for_ignore_file(nodes_arr)

    if ignore_file:
        return {
            'file_was_ignored': True,
            'status': 'success',
            'out': input_txt
        }
    else:
        # parse the ns data structure from the nodes
        parsed_ns = None
        try:
            parsed_ns = parse_ns(nodes_arr)
        except Exception as e:
            return {
                'status': 'error',
                'reason': str(e)
            }

        return format_nodes(nodes_arr, parsed_ns)

def format(input_txt):
    # replace any CRLF with LF before we do anything
    input_txt = crlf_to_lf(input_txt)

    # FIXME: wrap this in try/except and return error code if found
    tree = parse(input_txt)
    nodes_arr = flatten_tree(tree)

    ignore_file = look_for_ignore_file(nodes_arr)

    if ignore_file:
        return {
            'fileWasIgnored': True,
            'status': 'success',
            'out': input_txt
        }
    else:
        # parse the ns data structure from the nodes
        parsed_ns = None
        try:
            parsed_ns = parse_ns(nodes_arr)
        except Exception as e:
            return {
                'status': 'error',
                'reason': str(e)
            }

        return format_nodes(nodes_arr, parsed_ns)

# parses input_txt and returns a tree structure of the code
def parse(input_txt):
    return get_parser('source').parse(input_txt, 0)
