{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Search
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of documents that match the specified search criteria. How you specify the search criteria depends on which query parser you use. Amazon CloudSearch supports four query parsers:
--
--
--     * @simple@ : search all @text@ and @text-array@ fields for the specified string. Search for phrases, individual terms, and prefixes.
--
--     * @structured@ : search specific fields, construct compound queries using Boolean operators, and use advanced features such as term boosting and proximity searching.
--
--     * @lucene@ : specify search criteria using the Apache Lucene query parser syntax.
--
--     * @dismax@ : specify search criteria using the simplified subset of the Apache Lucene query parser syntax defined by the DisMax query parser.
--
-- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html Searching Your Data> in the /Amazon CloudSearch Developer Guide/ .
-- The endpoint for submitting @Search@ requests is domain-specific. You submit search requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service @DescribeDomains@ action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console.
module Network.AWS.CloudSearchDomains.Search
  ( -- * Creating a request
    Search (..),
    mkSearch,

    -- ** Request lenses
    sExpr,
    sCursor,
    sReturn,
    sQueryOptions,
    sFilterQuery,
    sSize,
    sQueryParser,
    sStart,
    sHighlight,
    sStats,
    sQuery,
    sSort,
    sFacet,
    sPartial,

    -- * Destructuring the response
    SearchResponse (..),
    mkSearchResponse,

    -- ** Response lenses
    sfrsStatus,
    sfrsFacets,
    sfrsStats,
    sfrsHits,
    sfrsResponseStatus,
  )
where

import Network.AWS.CloudSearchDomains.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @Search@ request.
--
-- /See:/ 'mkSearch' smart constructor.
data Search = Search'
  { -- | Defines one or more numeric expressions that can be used to sort results or specify search or filter criteria. You can also specify expressions as return fields.
    --
    -- You specify the expressions in JSON using the form @{"EXPRESSIONNAME":"EXPRESSION"}@ . You can define and use multiple expressions in a search request. For example:
    -- @{"expression1":"_score*rating", "expression2":"(1/rank)*year"} @
    -- For information about the variables, operators, and functions you can use in expressions, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html#writing-expressions Writing Expressions> in the /Amazon CloudSearch Developer Guide/ .
    expr :: Lude.Maybe Lude.Text,
    -- | Retrieves a cursor value you can use to page through large result sets. Use the @size@ parameter to control the number of hits to include in each response. You can specify either the @cursor@ or @start@ parameter in a request; they are mutually exclusive. To get the first cursor, set the cursor value to @initial@ . In subsequent requests, specify the cursor value returned in the hits section of the response.
    --
    -- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results> in the /Amazon CloudSearch Developer Guide/ .
    cursor :: Lude.Maybe Lude.Text,
    -- | Specifies the field and expression values to include in the response. Multiple fields or expressions are specified as a comma-separated list. By default, a search response includes all return enabled fields (@_all_fields@ ). To return only the document IDs for the matching documents, specify @_no_fields@ . To retrieve the relevance score calculated for each document, specify @_score@ .
    return :: Lude.Maybe Lude.Text,
    -- | Configures options for the query parser specified in the @queryParser@ parameter. You specify the options in JSON using the following form @{"OPTION1":"VALUE1","OPTION2":VALUE2"..."OPTIONN":"VALUEN"}.@
    --
    -- The options you can configure vary according to which parser you use:
    --
    --     * @defaultOperator@ : The default operator used to combine individual terms in the search string. For example: @defaultOperator: 'or'@ . For the @dismax@ parser, you specify a percentage that represents the percentage of terms in the search string (rounded down) that must match, rather than a default operator. A value of @0%@ is the equivalent to OR, and a value of @100%@ is equivalent to AND. The percentage must be specified as a value in the range 0-100 followed by the percent (%) symbol. For example, @defaultOperator: 50%@ . Valid values: @and@ , @or@ , a percentage in the range 0%-100% (@dismax@ ). Default: @and@ (@simple@ , @structured@ , @lucene@ ) or @100@ (@dismax@ ). Valid for: @simple@ , @structured@ , @lucene@ , and @dismax@ .
    --
    --     * @fields@ : An array of the fields to search when no fields are specified in a search. If no fields are specified in a search and this option is not specified, all text and text-array fields are searched. You can specify a weight for each field to control the relative importance of each field when Amazon CloudSearch calculates relevance scores. To specify a field weight, append a caret (@^@ ) symbol and the weight to the field name. For example, to boost the importance of the @title@ field over the @description@ field you could specify: @"fields":["title^5","description"]@ . Valid values: The name of any configured field and an optional numeric value greater than zero. Default: All @text@ and @text-array@ fields. Valid for: @simple@ , @structured@ , @lucene@ , and @dismax@ .
    --
    --     * @operators@ : An array of the operators or special characters you want to disable for the simple query parser. If you disable the @and@ , @or@ , or @not@ operators, the corresponding operators (@+@ , @|@ , @-@ ) have no special meaning and are dropped from the search string. Similarly, disabling @prefix@ disables the wildcard operator (@*@ ) and disabling @phrase@ disables the ability to search for phrases by enclosing phrases in double quotes. Disabling precedence disables the ability to control order of precedence using parentheses. Disabling @near@ disables the ability to use the ~ operator to perform a sloppy phrase search. Disabling the @fuzzy@ operator disables the ability to use the ~ operator to perform a fuzzy search. @escape@ disables the ability to use a backslash (@\@ ) to escape special characters within the search string. Disabling whitespace is an advanced option that prevents the parser from tokenizing on whitespace, which can be useful for Vietnamese. (It prevents Vietnamese words from being split incorrectly.) For example, you could disable all operators other than the phrase operator to support just simple term and phrase queries: @"operators":["and","not","or", "prefix"]@ . Valid values: @and@ , @escape@ , @fuzzy@ , @near@ , @not@ , @or@ , @phrase@ , @precedence@ , @prefix@ , @whitespace@ . Default: All operators and special characters are enabled. Valid for: @simple@ .
    --
    --     * @phraseFields@ : An array of the @text@ or @text-array@ fields you want to use for phrase searches. When the terms in the search string appear in close proximity within a field, the field scores higher. You can specify a weight for each field to boost that score. The @phraseSlop@ option controls how much the matches can deviate from the search string and still be boosted. To specify a field weight, append a caret (@^@ ) symbol and the weight to the field name. For example, to boost phrase matches in the @title@ field over the @abstract@ field, you could specify: @"phraseFields":["title^3", "plot"]@ Valid values: The name of any @text@ or @text-array@ field and an optional numeric value greater than zero. Default: No fields. If you don't specify any fields with @phraseFields@ , proximity scoring is disabled even if @phraseSlop@ is specified. Valid for: @dismax@ .
    --
    --     * @phraseSlop@ : An integer value that specifies how much matches can deviate from the search phrase and still be boosted according to the weights specified in the @phraseFields@ option; for example, @phraseSlop: 2@ . You must also specify @phraseFields@ to enable proximity scoring. Valid values: positive integers. Default: 0. Valid for: @dismax@ .
    --
    --     * @explicitPhraseSlop@ : An integer value that specifies how much a match can deviate from the search phrase when the phrase is enclosed in double quotes in the search string. (Phrases that exceed this proximity distance are not considered a match.) For example, to specify a slop of three for dismax phrase queries, you would specify @"explicitPhraseSlop":3@ . Valid values: positive integers. Default: 0. Valid for: @dismax@ .
    --
    --     * @tieBreaker@ : When a term in the search string is found in a document's field, a score is calculated for that field based on how common the word is in that field compared to other documents. If the term occurs in multiple fields within a document, by default only the highest scoring field contributes to the document's overall score. You can specify a @tieBreaker@ value to enable the matches in lower-scoring fields to contribute to the document's score. That way, if two documents have the same max field score for a particular term, the score for the document that has matches in more fields will be higher. The formula for calculating the score with a tieBreaker is @(max field score) + (tieBreaker) * (sum of the scores for the rest of the matching fields)@ . Set @tieBreaker@ to 0 to disregard all but the highest scoring field (pure max): @"tieBreaker":0@ . Set to 1 to sum the scores from all fields (pure sum): @"tieBreaker":1@ . Valid values: 0.0 to 1.0. Default: 0.0. Valid for: @dismax@ .
    queryOptions :: Lude.Maybe Lude.Text,
    -- | Specifies a structured query that filters the results of a search without affecting how the results are scored and sorted. You use @filterQuery@ in conjunction with the @query@ parameter to filter the documents that match the constraints specified in the @query@ parameter. Specifying a filter controls only which matching documents are included in the results, it has no effect on how they are scored and sorted. The @filterQuery@ parameter supports the full structured query syntax.
    --
    -- For more information about using filters, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/filtering-results.html Filtering Matching Documents> in the /Amazon CloudSearch Developer Guide/ .
    filterQuery :: Lude.Maybe Lude.Text,
    -- | Specifies the maximum number of search hits to include in the response.
    size :: Lude.Maybe Lude.Integer,
    -- | Specifies which query parser to use to process the request. If @queryParser@ is not specified, Amazon CloudSearch uses the @simple@ query parser.
    --
    -- Amazon CloudSearch supports four query parsers:
    --
    --     * @simple@ : perform simple searches of @text@ and @text-array@ fields. By default, the @simple@ query parser searches all @text@ and @text-array@ fields. You can specify which fields to search by with the @queryOptions@ parameter. If you prefix a search term with a plus sign (+) documents must contain the term to be considered a match. (This is the default, unless you configure the default operator with the @queryOptions@ parameter.) You can use the @-@ (NOT), @|@ (OR), and @*@ (wildcard) operators to exclude particular terms, find results that match any of the specified terms, or search for a prefix. To search for a phrase rather than individual terms, enclose the phrase in double quotes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-text.html Searching for Text> in the /Amazon CloudSearch Developer Guide/ .
    --
    --     * @structured@ : perform advanced searches by combining multiple expressions to define the search criteria. You can also search within particular fields, search for values and ranges of values, and use advanced options such as term boosting, @matchall@ , and @near@ . For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-compound-queries.html Constructing Compound Queries> in the /Amazon CloudSearch Developer Guide/ .
    --
    --     * @lucene@ : search using the Apache Lucene query parser syntax. For more information, see <http://lucene.apache.org/core/4_6_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package_description Apache Lucene Query Parser Syntax> .
    --
    --     * @dismax@ : search using the simplified subset of the Apache Lucene query parser syntax defined by the DisMax query parser. For more information, see <http://wiki.apache.org/solr/DisMaxQParserPlugin#Query_Syntax DisMax Query Parser Syntax> .
    queryParser :: Lude.Maybe QueryParser,
    -- | Specifies the offset of the first search hit you want to return. Note that the result set is zero-based; the first result is at index 0. You can specify either the @start@ or @cursor@ parameter in a request, they are mutually exclusive.
    --
    -- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results> in the /Amazon CloudSearch Developer Guide/ .
    start :: Lude.Maybe Lude.Integer,
    -- | Retrieves highlights for matches in the specified @text@ or @text-array@ fields. Each specified field must be highlight enabled in the domain configuration. The fields and options are specified in JSON using the form @{"FIELD":{"OPTION":VALUE,"OPTION:"STRING"},"FIELD":{"OPTION":VALUE,"OPTION":"STRING"}}@ .
    --
    -- You can specify the following highlight options:
    --
    --     * @format@ : specifies the format of the data in the text field: @text@ or @html@ . When data is returned as HTML, all non-alphanumeric characters are encoded. The default is @html@ .
    --
    --     * @max_phrases@ : specifies the maximum number of occurrences of the search term(s) you want to highlight. By default, the first occurrence is highlighted.
    --
    --     * @pre_tag@ : specifies the string to prepend to an occurrence of a search term. The default for HTML highlights is @&lt;em&gt;@ . The default for text highlights is @*@ .
    --
    --     * @post_tag@ : specifies the string to append to an occurrence of a search term. The default for HTML highlights is @&lt;/em&gt;@ . The default for text highlights is @*@ .
    --
    -- If no highlight options are specified for a field, the returned field text is treated as HTML and the first match is highlighted with emphasis tags: @&lt;em>search-term&lt;/em&gt;@ .
    -- For example, the following request retrieves highlights for the @actors@ and @title@ fields.
    -- @{ "actors": {}, "title": {"format": "text","max_phrases": 2,"pre_tag": "__","post_tag": "__ "} }@
    highlight :: Lude.Maybe Lude.Text,
    -- | Specifies one or more fields for which to get statistics information. Each specified field must be facet-enabled in the domain configuration. The fields are specified in JSON using the form:
    --
    -- @{"FIELD-A":{},"FIELD-B":{}}@ There are currently no options supported for statistics.
    stats :: Lude.Maybe Lude.Text,
    -- | Specifies the search criteria for the request. How you specify the search criteria depends on the query parser used for the request and the parser options specified in the @queryOptions@ parameter. By default, the @simple@ query parser is used to process requests. To use the @structured@ , @lucene@ , or @dismax@ query parser, you must also specify the @queryParser@ parameter.
    --
    -- For more information about specifying search criteria, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html Searching Your Data> in the /Amazon CloudSearch Developer Guide/ .
    query :: Lude.Text,
    -- | Specifies the fields or custom expressions to use to sort the search results. Multiple fields or expressions are specified as a comma-separated list. You must specify the sort direction (@asc@ or @desc@ ) for each field; for example, @year desc,title asc@ . To use a field to sort results, the field must be sort-enabled in the domain configuration. Array type fields cannot be used for sorting. If no @sort@ parameter is specified, results are sorted by their default relevance scores in descending order: @_score desc@ . You can also sort by document ID (@_id asc@ ) and version (@_version desc@ ).
    --
    -- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/sorting-results.html Sorting Results> in the /Amazon CloudSearch Developer Guide/ .
    sort :: Lude.Maybe Lude.Text,
    -- | Specifies one or more fields for which to get facet information, and options that control how the facet information is returned. Each specified field must be facet-enabled in the domain configuration. The fields and options are specified in JSON using the form @{"FIELD":{"OPTION":VALUE,"OPTION:"STRING"},"FIELD":{"OPTION":VALUE,"OPTION":"STRING"}}@ .
    --
    -- You can specify the following faceting options:
    --
    --     * @buckets@ specifies an array of the facet values or ranges to count. Ranges are specified using the same syntax that you use to search for a range of values. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-ranges.html Searching for a Range of Values> in the /Amazon CloudSearch Developer Guide/ . Buckets are returned in the order they are specified in the request. The @sort@ and @size@ options are not valid if you specify @buckets@ .
    --
    --
    --     * @size@ specifies the maximum number of facets to include in the results. By default, Amazon CloudSearch returns counts for the top 10. The @size@ parameter is only valid when you specify the @sort@ option; it cannot be used in conjunction with @buckets@ .
    --
    --
    --     * @sort@ specifies how you want to sort the facets in the results: @bucket@ or @count@ . Specify @bucket@ to sort alphabetically or numerically by facet value (in ascending order). Specify @count@ to sort by the facet counts computed for each facet value (in descending order). To retrieve facet counts for particular values or ranges of values, use the @buckets@ option instead of @sort@ .
    --
    --
    -- If no facet options are specified, facet counts are computed for all field values, the facets are sorted by facet count, and the top 10 facets are returned in the results.
    -- To count particular buckets of values, use the @buckets@ option. For example, the following request uses the @buckets@ option to calculate and return facet counts by decade.
    -- @{"year":{"buckets":["[1970,1979]","[1980,1989]","[1990,1999]","[2000,2009]","[2010,}"]}} @
    -- To sort facets by facet count, use the @count@ option. For example, the following request sets the @sort@ option to @count@ to sort the facet values by facet count, with the facet values that have the most matching documents listed first. Setting the @size@ option to 3 returns only the top three facet values.
    -- @{"year":{"sort":"count","size":3}} @
    -- To sort the facets by value, use the @bucket@ option. For example, the following request sets the @sort@ option to @bucket@ to sort the facet values numerically by year, with earliest year listed first.
    -- @{"year":{"sort":"bucket"}} @
    -- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/faceting.html Getting and Using Facet Information> in the /Amazon CloudSearch Developer Guide/ .
    facet :: Lude.Maybe Lude.Text,
    -- | Enables partial results to be returned if one or more index partitions are unavailable. When your search index is partitioned across multiple search instances, by default Amazon CloudSearch only returns results if every partition can be queried. This means that the failure of a single search instance can result in 5xx (internal server) errors. When you enable partial results, Amazon CloudSearch returns whatever results are available and includes the percentage of documents searched in the search results (percent-searched). This enables you to more gracefully degrade your users' search experience. For example, rather than displaying no results, you could display the partial results and a message indicating that the results might be incomplete due to a temporary system outage.
    partial :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Search' with the minimum fields required to make a request.
--
-- * 'expr' - Defines one or more numeric expressions that can be used to sort results or specify search or filter criteria. You can also specify expressions as return fields.
--
-- You specify the expressions in JSON using the form @{"EXPRESSIONNAME":"EXPRESSION"}@ . You can define and use multiple expressions in a search request. For example:
-- @{"expression1":"_score*rating", "expression2":"(1/rank)*year"} @
-- For information about the variables, operators, and functions you can use in expressions, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html#writing-expressions Writing Expressions> in the /Amazon CloudSearch Developer Guide/ .
-- * 'cursor' - Retrieves a cursor value you can use to page through large result sets. Use the @size@ parameter to control the number of hits to include in each response. You can specify either the @cursor@ or @start@ parameter in a request; they are mutually exclusive. To get the first cursor, set the cursor value to @initial@ . In subsequent requests, specify the cursor value returned in the hits section of the response.
--
-- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results> in the /Amazon CloudSearch Developer Guide/ .
-- * 'return' - Specifies the field and expression values to include in the response. Multiple fields or expressions are specified as a comma-separated list. By default, a search response includes all return enabled fields (@_all_fields@ ). To return only the document IDs for the matching documents, specify @_no_fields@ . To retrieve the relevance score calculated for each document, specify @_score@ .
-- * 'queryOptions' - Configures options for the query parser specified in the @queryParser@ parameter. You specify the options in JSON using the following form @{"OPTION1":"VALUE1","OPTION2":VALUE2"..."OPTIONN":"VALUEN"}.@
--
-- The options you can configure vary according to which parser you use:
--
--     * @defaultOperator@ : The default operator used to combine individual terms in the search string. For example: @defaultOperator: 'or'@ . For the @dismax@ parser, you specify a percentage that represents the percentage of terms in the search string (rounded down) that must match, rather than a default operator. A value of @0%@ is the equivalent to OR, and a value of @100%@ is equivalent to AND. The percentage must be specified as a value in the range 0-100 followed by the percent (%) symbol. For example, @defaultOperator: 50%@ . Valid values: @and@ , @or@ , a percentage in the range 0%-100% (@dismax@ ). Default: @and@ (@simple@ , @structured@ , @lucene@ ) or @100@ (@dismax@ ). Valid for: @simple@ , @structured@ , @lucene@ , and @dismax@ .
--
--     * @fields@ : An array of the fields to search when no fields are specified in a search. If no fields are specified in a search and this option is not specified, all text and text-array fields are searched. You can specify a weight for each field to control the relative importance of each field when Amazon CloudSearch calculates relevance scores. To specify a field weight, append a caret (@^@ ) symbol and the weight to the field name. For example, to boost the importance of the @title@ field over the @description@ field you could specify: @"fields":["title^5","description"]@ . Valid values: The name of any configured field and an optional numeric value greater than zero. Default: All @text@ and @text-array@ fields. Valid for: @simple@ , @structured@ , @lucene@ , and @dismax@ .
--
--     * @operators@ : An array of the operators or special characters you want to disable for the simple query parser. If you disable the @and@ , @or@ , or @not@ operators, the corresponding operators (@+@ , @|@ , @-@ ) have no special meaning and are dropped from the search string. Similarly, disabling @prefix@ disables the wildcard operator (@*@ ) and disabling @phrase@ disables the ability to search for phrases by enclosing phrases in double quotes. Disabling precedence disables the ability to control order of precedence using parentheses. Disabling @near@ disables the ability to use the ~ operator to perform a sloppy phrase search. Disabling the @fuzzy@ operator disables the ability to use the ~ operator to perform a fuzzy search. @escape@ disables the ability to use a backslash (@\@ ) to escape special characters within the search string. Disabling whitespace is an advanced option that prevents the parser from tokenizing on whitespace, which can be useful for Vietnamese. (It prevents Vietnamese words from being split incorrectly.) For example, you could disable all operators other than the phrase operator to support just simple term and phrase queries: @"operators":["and","not","or", "prefix"]@ . Valid values: @and@ , @escape@ , @fuzzy@ , @near@ , @not@ , @or@ , @phrase@ , @precedence@ , @prefix@ , @whitespace@ . Default: All operators and special characters are enabled. Valid for: @simple@ .
--
--     * @phraseFields@ : An array of the @text@ or @text-array@ fields you want to use for phrase searches. When the terms in the search string appear in close proximity within a field, the field scores higher. You can specify a weight for each field to boost that score. The @phraseSlop@ option controls how much the matches can deviate from the search string and still be boosted. To specify a field weight, append a caret (@^@ ) symbol and the weight to the field name. For example, to boost phrase matches in the @title@ field over the @abstract@ field, you could specify: @"phraseFields":["title^3", "plot"]@ Valid values: The name of any @text@ or @text-array@ field and an optional numeric value greater than zero. Default: No fields. If you don't specify any fields with @phraseFields@ , proximity scoring is disabled even if @phraseSlop@ is specified. Valid for: @dismax@ .
--
--     * @phraseSlop@ : An integer value that specifies how much matches can deviate from the search phrase and still be boosted according to the weights specified in the @phraseFields@ option; for example, @phraseSlop: 2@ . You must also specify @phraseFields@ to enable proximity scoring. Valid values: positive integers. Default: 0. Valid for: @dismax@ .
--
--     * @explicitPhraseSlop@ : An integer value that specifies how much a match can deviate from the search phrase when the phrase is enclosed in double quotes in the search string. (Phrases that exceed this proximity distance are not considered a match.) For example, to specify a slop of three for dismax phrase queries, you would specify @"explicitPhraseSlop":3@ . Valid values: positive integers. Default: 0. Valid for: @dismax@ .
--
--     * @tieBreaker@ : When a term in the search string is found in a document's field, a score is calculated for that field based on how common the word is in that field compared to other documents. If the term occurs in multiple fields within a document, by default only the highest scoring field contributes to the document's overall score. You can specify a @tieBreaker@ value to enable the matches in lower-scoring fields to contribute to the document's score. That way, if two documents have the same max field score for a particular term, the score for the document that has matches in more fields will be higher. The formula for calculating the score with a tieBreaker is @(max field score) + (tieBreaker) * (sum of the scores for the rest of the matching fields)@ . Set @tieBreaker@ to 0 to disregard all but the highest scoring field (pure max): @"tieBreaker":0@ . Set to 1 to sum the scores from all fields (pure sum): @"tieBreaker":1@ . Valid values: 0.0 to 1.0. Default: 0.0. Valid for: @dismax@ .
--
-- * 'filterQuery' - Specifies a structured query that filters the results of a search without affecting how the results are scored and sorted. You use @filterQuery@ in conjunction with the @query@ parameter to filter the documents that match the constraints specified in the @query@ parameter. Specifying a filter controls only which matching documents are included in the results, it has no effect on how they are scored and sorted. The @filterQuery@ parameter supports the full structured query syntax.
--
-- For more information about using filters, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/filtering-results.html Filtering Matching Documents> in the /Amazon CloudSearch Developer Guide/ .
-- * 'size' - Specifies the maximum number of search hits to include in the response.
-- * 'queryParser' - Specifies which query parser to use to process the request. If @queryParser@ is not specified, Amazon CloudSearch uses the @simple@ query parser.
--
-- Amazon CloudSearch supports four query parsers:
--
--     * @simple@ : perform simple searches of @text@ and @text-array@ fields. By default, the @simple@ query parser searches all @text@ and @text-array@ fields. You can specify which fields to search by with the @queryOptions@ parameter. If you prefix a search term with a plus sign (+) documents must contain the term to be considered a match. (This is the default, unless you configure the default operator with the @queryOptions@ parameter.) You can use the @-@ (NOT), @|@ (OR), and @*@ (wildcard) operators to exclude particular terms, find results that match any of the specified terms, or search for a prefix. To search for a phrase rather than individual terms, enclose the phrase in double quotes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-text.html Searching for Text> in the /Amazon CloudSearch Developer Guide/ .
--
--     * @structured@ : perform advanced searches by combining multiple expressions to define the search criteria. You can also search within particular fields, search for values and ranges of values, and use advanced options such as term boosting, @matchall@ , and @near@ . For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-compound-queries.html Constructing Compound Queries> in the /Amazon CloudSearch Developer Guide/ .
--
--     * @lucene@ : search using the Apache Lucene query parser syntax. For more information, see <http://lucene.apache.org/core/4_6_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package_description Apache Lucene Query Parser Syntax> .
--
--     * @dismax@ : search using the simplified subset of the Apache Lucene query parser syntax defined by the DisMax query parser. For more information, see <http://wiki.apache.org/solr/DisMaxQParserPlugin#Query_Syntax DisMax Query Parser Syntax> .
--
-- * 'start' - Specifies the offset of the first search hit you want to return. Note that the result set is zero-based; the first result is at index 0. You can specify either the @start@ or @cursor@ parameter in a request, they are mutually exclusive.
--
-- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results> in the /Amazon CloudSearch Developer Guide/ .
-- * 'highlight' - Retrieves highlights for matches in the specified @text@ or @text-array@ fields. Each specified field must be highlight enabled in the domain configuration. The fields and options are specified in JSON using the form @{"FIELD":{"OPTION":VALUE,"OPTION:"STRING"},"FIELD":{"OPTION":VALUE,"OPTION":"STRING"}}@ .
--
-- You can specify the following highlight options:
--
--     * @format@ : specifies the format of the data in the text field: @text@ or @html@ . When data is returned as HTML, all non-alphanumeric characters are encoded. The default is @html@ .
--
--     * @max_phrases@ : specifies the maximum number of occurrences of the search term(s) you want to highlight. By default, the first occurrence is highlighted.
--
--     * @pre_tag@ : specifies the string to prepend to an occurrence of a search term. The default for HTML highlights is @&lt;em&gt;@ . The default for text highlights is @*@ .
--
--     * @post_tag@ : specifies the string to append to an occurrence of a search term. The default for HTML highlights is @&lt;/em&gt;@ . The default for text highlights is @*@ .
--
-- If no highlight options are specified for a field, the returned field text is treated as HTML and the first match is highlighted with emphasis tags: @&lt;em>search-term&lt;/em&gt;@ .
-- For example, the following request retrieves highlights for the @actors@ and @title@ fields.
-- @{ "actors": {}, "title": {"format": "text","max_phrases": 2,"pre_tag": "__","post_tag": "__ "} }@
-- * 'stats' - Specifies one or more fields for which to get statistics information. Each specified field must be facet-enabled in the domain configuration. The fields are specified in JSON using the form:
--
-- @{"FIELD-A":{},"FIELD-B":{}}@ There are currently no options supported for statistics.
-- * 'query' - Specifies the search criteria for the request. How you specify the search criteria depends on the query parser used for the request and the parser options specified in the @queryOptions@ parameter. By default, the @simple@ query parser is used to process requests. To use the @structured@ , @lucene@ , or @dismax@ query parser, you must also specify the @queryParser@ parameter.
--
-- For more information about specifying search criteria, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html Searching Your Data> in the /Amazon CloudSearch Developer Guide/ .
-- * 'sort' - Specifies the fields or custom expressions to use to sort the search results. Multiple fields or expressions are specified as a comma-separated list. You must specify the sort direction (@asc@ or @desc@ ) for each field; for example, @year desc,title asc@ . To use a field to sort results, the field must be sort-enabled in the domain configuration. Array type fields cannot be used for sorting. If no @sort@ parameter is specified, results are sorted by their default relevance scores in descending order: @_score desc@ . You can also sort by document ID (@_id asc@ ) and version (@_version desc@ ).
--
-- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/sorting-results.html Sorting Results> in the /Amazon CloudSearch Developer Guide/ .
-- * 'facet' - Specifies one or more fields for which to get facet information, and options that control how the facet information is returned. Each specified field must be facet-enabled in the domain configuration. The fields and options are specified in JSON using the form @{"FIELD":{"OPTION":VALUE,"OPTION:"STRING"},"FIELD":{"OPTION":VALUE,"OPTION":"STRING"}}@ .
--
-- You can specify the following faceting options:
--
--     * @buckets@ specifies an array of the facet values or ranges to count. Ranges are specified using the same syntax that you use to search for a range of values. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-ranges.html Searching for a Range of Values> in the /Amazon CloudSearch Developer Guide/ . Buckets are returned in the order they are specified in the request. The @sort@ and @size@ options are not valid if you specify @buckets@ .
--
--
--     * @size@ specifies the maximum number of facets to include in the results. By default, Amazon CloudSearch returns counts for the top 10. The @size@ parameter is only valid when you specify the @sort@ option; it cannot be used in conjunction with @buckets@ .
--
--
--     * @sort@ specifies how you want to sort the facets in the results: @bucket@ or @count@ . Specify @bucket@ to sort alphabetically or numerically by facet value (in ascending order). Specify @count@ to sort by the facet counts computed for each facet value (in descending order). To retrieve facet counts for particular values or ranges of values, use the @buckets@ option instead of @sort@ .
--
--
-- If no facet options are specified, facet counts are computed for all field values, the facets are sorted by facet count, and the top 10 facets are returned in the results.
-- To count particular buckets of values, use the @buckets@ option. For example, the following request uses the @buckets@ option to calculate and return facet counts by decade.
-- @{"year":{"buckets":["[1970,1979]","[1980,1989]","[1990,1999]","[2000,2009]","[2010,}"]}} @
-- To sort facets by facet count, use the @count@ option. For example, the following request sets the @sort@ option to @count@ to sort the facet values by facet count, with the facet values that have the most matching documents listed first. Setting the @size@ option to 3 returns only the top three facet values.
-- @{"year":{"sort":"count","size":3}} @
-- To sort the facets by value, use the @bucket@ option. For example, the following request sets the @sort@ option to @bucket@ to sort the facet values numerically by year, with earliest year listed first.
-- @{"year":{"sort":"bucket"}} @
-- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/faceting.html Getting and Using Facet Information> in the /Amazon CloudSearch Developer Guide/ .
-- * 'partial' - Enables partial results to be returned if one or more index partitions are unavailable. When your search index is partitioned across multiple search instances, by default Amazon CloudSearch only returns results if every partition can be queried. This means that the failure of a single search instance can result in 5xx (internal server) errors. When you enable partial results, Amazon CloudSearch returns whatever results are available and includes the percentage of documents searched in the search results (percent-searched). This enables you to more gracefully degrade your users' search experience. For example, rather than displaying no results, you could display the partial results and a message indicating that the results might be incomplete due to a temporary system outage.
mkSearch ::
  -- | 'query'
  Lude.Text ->
  Search
mkSearch pQuery_ =
  Search'
    { expr = Lude.Nothing,
      cursor = Lude.Nothing,
      return = Lude.Nothing,
      queryOptions = Lude.Nothing,
      filterQuery = Lude.Nothing,
      size = Lude.Nothing,
      queryParser = Lude.Nothing,
      start = Lude.Nothing,
      highlight = Lude.Nothing,
      stats = Lude.Nothing,
      query = pQuery_,
      sort = Lude.Nothing,
      facet = Lude.Nothing,
      partial = Lude.Nothing
    }

-- | Defines one or more numeric expressions that can be used to sort results or specify search or filter criteria. You can also specify expressions as return fields.
--
-- You specify the expressions in JSON using the form @{"EXPRESSIONNAME":"EXPRESSION"}@ . You can define and use multiple expressions in a search request. For example:
-- @{"expression1":"_score*rating", "expression2":"(1/rank)*year"} @
-- For information about the variables, operators, and functions you can use in expressions, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html#writing-expressions Writing Expressions> in the /Amazon CloudSearch Developer Guide/ .
--
-- /Note:/ Consider using 'expr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sExpr :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sExpr = Lens.lens (expr :: Search -> Lude.Maybe Lude.Text) (\s a -> s {expr = a} :: Search)
{-# DEPRECATED sExpr "Use generic-lens or generic-optics with 'expr' instead." #-}

-- | Retrieves a cursor value you can use to page through large result sets. Use the @size@ parameter to control the number of hits to include in each response. You can specify either the @cursor@ or @start@ parameter in a request; they are mutually exclusive. To get the first cursor, set the cursor value to @initial@ . In subsequent requests, specify the cursor value returned in the hits section of the response.
--
-- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results> in the /Amazon CloudSearch Developer Guide/ .
--
-- /Note:/ Consider using 'cursor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCursor :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sCursor = Lens.lens (cursor :: Search -> Lude.Maybe Lude.Text) (\s a -> s {cursor = a} :: Search)
{-# DEPRECATED sCursor "Use generic-lens or generic-optics with 'cursor' instead." #-}

-- | Specifies the field and expression values to include in the response. Multiple fields or expressions are specified as a comma-separated list. By default, a search response includes all return enabled fields (@_all_fields@ ). To return only the document IDs for the matching documents, specify @_no_fields@ . To retrieve the relevance score calculated for each document, specify @_score@ .
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReturn :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sReturn = Lens.lens (return :: Search -> Lude.Maybe Lude.Text) (\s a -> s {return = a} :: Search)
{-# DEPRECATED sReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | Configures options for the query parser specified in the @queryParser@ parameter. You specify the options in JSON using the following form @{"OPTION1":"VALUE1","OPTION2":VALUE2"..."OPTIONN":"VALUEN"}.@
--
-- The options you can configure vary according to which parser you use:
--
--     * @defaultOperator@ : The default operator used to combine individual terms in the search string. For example: @defaultOperator: 'or'@ . For the @dismax@ parser, you specify a percentage that represents the percentage of terms in the search string (rounded down) that must match, rather than a default operator. A value of @0%@ is the equivalent to OR, and a value of @100%@ is equivalent to AND. The percentage must be specified as a value in the range 0-100 followed by the percent (%) symbol. For example, @defaultOperator: 50%@ . Valid values: @and@ , @or@ , a percentage in the range 0%-100% (@dismax@ ). Default: @and@ (@simple@ , @structured@ , @lucene@ ) or @100@ (@dismax@ ). Valid for: @simple@ , @structured@ , @lucene@ , and @dismax@ .
--
--     * @fields@ : An array of the fields to search when no fields are specified in a search. If no fields are specified in a search and this option is not specified, all text and text-array fields are searched. You can specify a weight for each field to control the relative importance of each field when Amazon CloudSearch calculates relevance scores. To specify a field weight, append a caret (@^@ ) symbol and the weight to the field name. For example, to boost the importance of the @title@ field over the @description@ field you could specify: @"fields":["title^5","description"]@ . Valid values: The name of any configured field and an optional numeric value greater than zero. Default: All @text@ and @text-array@ fields. Valid for: @simple@ , @structured@ , @lucene@ , and @dismax@ .
--
--     * @operators@ : An array of the operators or special characters you want to disable for the simple query parser. If you disable the @and@ , @or@ , or @not@ operators, the corresponding operators (@+@ , @|@ , @-@ ) have no special meaning and are dropped from the search string. Similarly, disabling @prefix@ disables the wildcard operator (@*@ ) and disabling @phrase@ disables the ability to search for phrases by enclosing phrases in double quotes. Disabling precedence disables the ability to control order of precedence using parentheses. Disabling @near@ disables the ability to use the ~ operator to perform a sloppy phrase search. Disabling the @fuzzy@ operator disables the ability to use the ~ operator to perform a fuzzy search. @escape@ disables the ability to use a backslash (@\@ ) to escape special characters within the search string. Disabling whitespace is an advanced option that prevents the parser from tokenizing on whitespace, which can be useful for Vietnamese. (It prevents Vietnamese words from being split incorrectly.) For example, you could disable all operators other than the phrase operator to support just simple term and phrase queries: @"operators":["and","not","or", "prefix"]@ . Valid values: @and@ , @escape@ , @fuzzy@ , @near@ , @not@ , @or@ , @phrase@ , @precedence@ , @prefix@ , @whitespace@ . Default: All operators and special characters are enabled. Valid for: @simple@ .
--
--     * @phraseFields@ : An array of the @text@ or @text-array@ fields you want to use for phrase searches. When the terms in the search string appear in close proximity within a field, the field scores higher. You can specify a weight for each field to boost that score. The @phraseSlop@ option controls how much the matches can deviate from the search string and still be boosted. To specify a field weight, append a caret (@^@ ) symbol and the weight to the field name. For example, to boost phrase matches in the @title@ field over the @abstract@ field, you could specify: @"phraseFields":["title^3", "plot"]@ Valid values: The name of any @text@ or @text-array@ field and an optional numeric value greater than zero. Default: No fields. If you don't specify any fields with @phraseFields@ , proximity scoring is disabled even if @phraseSlop@ is specified. Valid for: @dismax@ .
--
--     * @phraseSlop@ : An integer value that specifies how much matches can deviate from the search phrase and still be boosted according to the weights specified in the @phraseFields@ option; for example, @phraseSlop: 2@ . You must also specify @phraseFields@ to enable proximity scoring. Valid values: positive integers. Default: 0. Valid for: @dismax@ .
--
--     * @explicitPhraseSlop@ : An integer value that specifies how much a match can deviate from the search phrase when the phrase is enclosed in double quotes in the search string. (Phrases that exceed this proximity distance are not considered a match.) For example, to specify a slop of three for dismax phrase queries, you would specify @"explicitPhraseSlop":3@ . Valid values: positive integers. Default: 0. Valid for: @dismax@ .
--
--     * @tieBreaker@ : When a term in the search string is found in a document's field, a score is calculated for that field based on how common the word is in that field compared to other documents. If the term occurs in multiple fields within a document, by default only the highest scoring field contributes to the document's overall score. You can specify a @tieBreaker@ value to enable the matches in lower-scoring fields to contribute to the document's score. That way, if two documents have the same max field score for a particular term, the score for the document that has matches in more fields will be higher. The formula for calculating the score with a tieBreaker is @(max field score) + (tieBreaker) * (sum of the scores for the rest of the matching fields)@ . Set @tieBreaker@ to 0 to disregard all but the highest scoring field (pure max): @"tieBreaker":0@ . Set to 1 to sum the scores from all fields (pure sum): @"tieBreaker":1@ . Valid values: 0.0 to 1.0. Default: 0.0. Valid for: @dismax@ .
--
--
-- /Note:/ Consider using 'queryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sQueryOptions :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sQueryOptions = Lens.lens (queryOptions :: Search -> Lude.Maybe Lude.Text) (\s a -> s {queryOptions = a} :: Search)
{-# DEPRECATED sQueryOptions "Use generic-lens or generic-optics with 'queryOptions' instead." #-}

-- | Specifies a structured query that filters the results of a search without affecting how the results are scored and sorted. You use @filterQuery@ in conjunction with the @query@ parameter to filter the documents that match the constraints specified in the @query@ parameter. Specifying a filter controls only which matching documents are included in the results, it has no effect on how they are scored and sorted. The @filterQuery@ parameter supports the full structured query syntax.
--
-- For more information about using filters, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/filtering-results.html Filtering Matching Documents> in the /Amazon CloudSearch Developer Guide/ .
--
-- /Note:/ Consider using 'filterQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFilterQuery :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sFilterQuery = Lens.lens (filterQuery :: Search -> Lude.Maybe Lude.Text) (\s a -> s {filterQuery = a} :: Search)
{-# DEPRECATED sFilterQuery "Use generic-lens or generic-optics with 'filterQuery' instead." #-}

-- | Specifies the maximum number of search hits to include in the response.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSize :: Lens.Lens' Search (Lude.Maybe Lude.Integer)
sSize = Lens.lens (size :: Search -> Lude.Maybe Lude.Integer) (\s a -> s {size = a} :: Search)
{-# DEPRECATED sSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | Specifies which query parser to use to process the request. If @queryParser@ is not specified, Amazon CloudSearch uses the @simple@ query parser.
--
-- Amazon CloudSearch supports four query parsers:
--
--     * @simple@ : perform simple searches of @text@ and @text-array@ fields. By default, the @simple@ query parser searches all @text@ and @text-array@ fields. You can specify which fields to search by with the @queryOptions@ parameter. If you prefix a search term with a plus sign (+) documents must contain the term to be considered a match. (This is the default, unless you configure the default operator with the @queryOptions@ parameter.) You can use the @-@ (NOT), @|@ (OR), and @*@ (wildcard) operators to exclude particular terms, find results that match any of the specified terms, or search for a prefix. To search for a phrase rather than individual terms, enclose the phrase in double quotes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-text.html Searching for Text> in the /Amazon CloudSearch Developer Guide/ .
--
--     * @structured@ : perform advanced searches by combining multiple expressions to define the search criteria. You can also search within particular fields, search for values and ranges of values, and use advanced options such as term boosting, @matchall@ , and @near@ . For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-compound-queries.html Constructing Compound Queries> in the /Amazon CloudSearch Developer Guide/ .
--
--     * @lucene@ : search using the Apache Lucene query parser syntax. For more information, see <http://lucene.apache.org/core/4_6_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package_description Apache Lucene Query Parser Syntax> .
--
--     * @dismax@ : search using the simplified subset of the Apache Lucene query parser syntax defined by the DisMax query parser. For more information, see <http://wiki.apache.org/solr/DisMaxQParserPlugin#Query_Syntax DisMax Query Parser Syntax> .
--
--
-- /Note:/ Consider using 'queryParser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sQueryParser :: Lens.Lens' Search (Lude.Maybe QueryParser)
sQueryParser = Lens.lens (queryParser :: Search -> Lude.Maybe QueryParser) (\s a -> s {queryParser = a} :: Search)
{-# DEPRECATED sQueryParser "Use generic-lens or generic-optics with 'queryParser' instead." #-}

-- | Specifies the offset of the first search hit you want to return. Note that the result set is zero-based; the first result is at index 0. You can specify either the @start@ or @cursor@ parameter in a request, they are mutually exclusive.
--
-- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results> in the /Amazon CloudSearch Developer Guide/ .
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStart :: Lens.Lens' Search (Lude.Maybe Lude.Integer)
sStart = Lens.lens (start :: Search -> Lude.Maybe Lude.Integer) (\s a -> s {start = a} :: Search)
{-# DEPRECATED sStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | Retrieves highlights for matches in the specified @text@ or @text-array@ fields. Each specified field must be highlight enabled in the domain configuration. The fields and options are specified in JSON using the form @{"FIELD":{"OPTION":VALUE,"OPTION:"STRING"},"FIELD":{"OPTION":VALUE,"OPTION":"STRING"}}@ .
--
-- You can specify the following highlight options:
--
--     * @format@ : specifies the format of the data in the text field: @text@ or @html@ . When data is returned as HTML, all non-alphanumeric characters are encoded. The default is @html@ .
--
--     * @max_phrases@ : specifies the maximum number of occurrences of the search term(s) you want to highlight. By default, the first occurrence is highlighted.
--
--     * @pre_tag@ : specifies the string to prepend to an occurrence of a search term. The default for HTML highlights is @&lt;em&gt;@ . The default for text highlights is @*@ .
--
--     * @post_tag@ : specifies the string to append to an occurrence of a search term. The default for HTML highlights is @&lt;/em&gt;@ . The default for text highlights is @*@ .
--
-- If no highlight options are specified for a field, the returned field text is treated as HTML and the first match is highlighted with emphasis tags: @&lt;em>search-term&lt;/em&gt;@ .
-- For example, the following request retrieves highlights for the @actors@ and @title@ fields.
-- @{ "actors": {}, "title": {"format": "text","max_phrases": 2,"pre_tag": "__","post_tag": "__ "} }@
--
-- /Note:/ Consider using 'highlight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHighlight :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sHighlight = Lens.lens (highlight :: Search -> Lude.Maybe Lude.Text) (\s a -> s {highlight = a} :: Search)
{-# DEPRECATED sHighlight "Use generic-lens or generic-optics with 'highlight' instead." #-}

-- | Specifies one or more fields for which to get statistics information. Each specified field must be facet-enabled in the domain configuration. The fields are specified in JSON using the form:
--
-- @{"FIELD-A":{},"FIELD-B":{}}@ There are currently no options supported for statistics.
--
-- /Note:/ Consider using 'stats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStats :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sStats = Lens.lens (stats :: Search -> Lude.Maybe Lude.Text) (\s a -> s {stats = a} :: Search)
{-# DEPRECATED sStats "Use generic-lens or generic-optics with 'stats' instead." #-}

-- | Specifies the search criteria for the request. How you specify the search criteria depends on the query parser used for the request and the parser options specified in the @queryOptions@ parameter. By default, the @simple@ query parser is used to process requests. To use the @structured@ , @lucene@ , or @dismax@ query parser, you must also specify the @queryParser@ parameter.
--
-- For more information about specifying search criteria, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html Searching Your Data> in the /Amazon CloudSearch Developer Guide/ .
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sQuery :: Lens.Lens' Search Lude.Text
sQuery = Lens.lens (query :: Search -> Lude.Text) (\s a -> s {query = a} :: Search)
{-# DEPRECATED sQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | Specifies the fields or custom expressions to use to sort the search results. Multiple fields or expressions are specified as a comma-separated list. You must specify the sort direction (@asc@ or @desc@ ) for each field; for example, @year desc,title asc@ . To use a field to sort results, the field must be sort-enabled in the domain configuration. Array type fields cannot be used for sorting. If no @sort@ parameter is specified, results are sorted by their default relevance scores in descending order: @_score desc@ . You can also sort by document ID (@_id asc@ ) and version (@_version desc@ ).
--
-- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/sorting-results.html Sorting Results> in the /Amazon CloudSearch Developer Guide/ .
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSort :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sSort = Lens.lens (sort :: Search -> Lude.Maybe Lude.Text) (\s a -> s {sort = a} :: Search)
{-# DEPRECATED sSort "Use generic-lens or generic-optics with 'sort' instead." #-}

-- | Specifies one or more fields for which to get facet information, and options that control how the facet information is returned. Each specified field must be facet-enabled in the domain configuration. The fields and options are specified in JSON using the form @{"FIELD":{"OPTION":VALUE,"OPTION:"STRING"},"FIELD":{"OPTION":VALUE,"OPTION":"STRING"}}@ .
--
-- You can specify the following faceting options:
--
--     * @buckets@ specifies an array of the facet values or ranges to count. Ranges are specified using the same syntax that you use to search for a range of values. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-ranges.html Searching for a Range of Values> in the /Amazon CloudSearch Developer Guide/ . Buckets are returned in the order they are specified in the request. The @sort@ and @size@ options are not valid if you specify @buckets@ .
--
--
--     * @size@ specifies the maximum number of facets to include in the results. By default, Amazon CloudSearch returns counts for the top 10. The @size@ parameter is only valid when you specify the @sort@ option; it cannot be used in conjunction with @buckets@ .
--
--
--     * @sort@ specifies how you want to sort the facets in the results: @bucket@ or @count@ . Specify @bucket@ to sort alphabetically or numerically by facet value (in ascending order). Specify @count@ to sort by the facet counts computed for each facet value (in descending order). To retrieve facet counts for particular values or ranges of values, use the @buckets@ option instead of @sort@ .
--
--
-- If no facet options are specified, facet counts are computed for all field values, the facets are sorted by facet count, and the top 10 facets are returned in the results.
-- To count particular buckets of values, use the @buckets@ option. For example, the following request uses the @buckets@ option to calculate and return facet counts by decade.
-- @{"year":{"buckets":["[1970,1979]","[1980,1989]","[1990,1999]","[2000,2009]","[2010,}"]}} @
-- To sort facets by facet count, use the @count@ option. For example, the following request sets the @sort@ option to @count@ to sort the facet values by facet count, with the facet values that have the most matching documents listed first. Setting the @size@ option to 3 returns only the top three facet values.
-- @{"year":{"sort":"count","size":3}} @
-- To sort the facets by value, use the @bucket@ option. For example, the following request sets the @sort@ option to @bucket@ to sort the facet values numerically by year, with earliest year listed first.
-- @{"year":{"sort":"bucket"}} @
-- For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/faceting.html Getting and Using Facet Information> in the /Amazon CloudSearch Developer Guide/ .
--
-- /Note:/ Consider using 'facet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFacet :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sFacet = Lens.lens (facet :: Search -> Lude.Maybe Lude.Text) (\s a -> s {facet = a} :: Search)
{-# DEPRECATED sFacet "Use generic-lens or generic-optics with 'facet' instead." #-}

-- | Enables partial results to be returned if one or more index partitions are unavailable. When your search index is partitioned across multiple search instances, by default Amazon CloudSearch only returns results if every partition can be queried. This means that the failure of a single search instance can result in 5xx (internal server) errors. When you enable partial results, Amazon CloudSearch returns whatever results are available and includes the percentage of documents searched in the search results (percent-searched). This enables you to more gracefully degrade your users' search experience. For example, rather than displaying no results, you could display the partial results and a message indicating that the results might be incomplete due to a temporary system outage.
--
-- /Note:/ Consider using 'partial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPartial :: Lens.Lens' Search (Lude.Maybe Lude.Bool)
sPartial = Lens.lens (partial :: Search -> Lude.Maybe Lude.Bool) (\s a -> s {partial = a} :: Search)
{-# DEPRECATED sPartial "Use generic-lens or generic-optics with 'partial' instead." #-}

instance Lude.AWSRequest Search where
  type Rs Search = SearchResponse
  request = Req.get cloudSearchDomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "facets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "stats" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "hits")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Search where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath Search where
  toPath = Lude.const "/2013-01-01/search"

instance Lude.ToQuery Search where
  toQuery Search' {..} =
    Lude.mconcat
      [ "expr" Lude.=: expr,
        "cursor" Lude.=: cursor,
        "return" Lude.=: return,
        "q.options" Lude.=: queryOptions,
        "fq" Lude.=: filterQuery,
        "size" Lude.=: size,
        "q.parser" Lude.=: queryParser,
        "start" Lude.=: start,
        "highlight" Lude.=: highlight,
        "stats" Lude.=: stats,
        "q" Lude.=: query,
        "sort" Lude.=: sort,
        "facet" Lude.=: facet,
        "partial" Lude.=: partial,
        "format=sdk&pretty=true"
      ]

-- | The result of a @Search@ request. Contains the documents that match the specified search criteria and any requested fields, highlights, and facet information.
--
-- /See:/ 'mkSearchResponse' smart constructor.
data SearchResponse = SearchResponse'
  { -- | The status information returned for the search request.
    status :: Lude.Maybe SearchStatus,
    -- | The requested facet information.
    facets :: Lude.Maybe (Lude.HashMap Lude.Text (BucketInfo)),
    -- | The requested field statistics information.
    stats :: Lude.Maybe (Lude.HashMap Lude.Text (FieldStats)),
    -- | The documents that match the search criteria.
    hits :: Lude.Maybe Hits,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status information returned for the search request.
-- * 'facets' - The requested facet information.
-- * 'stats' - The requested field statistics information.
-- * 'hits' - The documents that match the search criteria.
-- * 'responseStatus' - The response status code.
mkSearchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchResponse
mkSearchResponse pResponseStatus_ =
  SearchResponse'
    { status = Lude.Nothing,
      facets = Lude.Nothing,
      stats = Lude.Nothing,
      hits = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status information returned for the search request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsStatus :: Lens.Lens' SearchResponse (Lude.Maybe SearchStatus)
sfrsStatus = Lens.lens (status :: SearchResponse -> Lude.Maybe SearchStatus) (\s a -> s {status = a} :: SearchResponse)
{-# DEPRECATED sfrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The requested facet information.
--
-- /Note:/ Consider using 'facets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsFacets :: Lens.Lens' SearchResponse (Lude.Maybe (Lude.HashMap Lude.Text (BucketInfo)))
sfrsFacets = Lens.lens (facets :: SearchResponse -> Lude.Maybe (Lude.HashMap Lude.Text (BucketInfo))) (\s a -> s {facets = a} :: SearchResponse)
{-# DEPRECATED sfrsFacets "Use generic-lens or generic-optics with 'facets' instead." #-}

-- | The requested field statistics information.
--
-- /Note:/ Consider using 'stats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsStats :: Lens.Lens' SearchResponse (Lude.Maybe (Lude.HashMap Lude.Text (FieldStats)))
sfrsStats = Lens.lens (stats :: SearchResponse -> Lude.Maybe (Lude.HashMap Lude.Text (FieldStats))) (\s a -> s {stats = a} :: SearchResponse)
{-# DEPRECATED sfrsStats "Use generic-lens or generic-optics with 'stats' instead." #-}

-- | The documents that match the search criteria.
--
-- /Note:/ Consider using 'hits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsHits :: Lens.Lens' SearchResponse (Lude.Maybe Hits)
sfrsHits = Lens.lens (hits :: SearchResponse -> Lude.Maybe Hits) (\s a -> s {hits = a} :: SearchResponse)
{-# DEPRECATED sfrsHits "Use generic-lens or generic-optics with 'hits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsResponseStatus :: Lens.Lens' SearchResponse Lude.Int
sfrsResponseStatus = Lens.lens (responseStatus :: SearchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchResponse)
{-# DEPRECATED sfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
