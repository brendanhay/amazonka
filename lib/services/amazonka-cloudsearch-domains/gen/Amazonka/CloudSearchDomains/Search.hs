{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudSearchDomains.Search
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of documents that match the specified search criteria.
-- How you specify the search criteria depends on which query parser you
-- use. Amazon CloudSearch supports four query parsers:
--
-- -   @simple@: search all @text@ and @text-array@ fields for the
--     specified string. Search for phrases, individual terms, and
--     prefixes.
-- -   @structured@: search specific fields, construct compound queries
--     using Boolean operators, and use advanced features such as term
--     boosting and proximity searching.
-- -   @lucene@: specify search criteria using the Apache Lucene query
--     parser syntax.
-- -   @dismax@: specify search criteria using the simplified subset of the
--     Apache Lucene query parser syntax defined by the DisMax query
--     parser.
--
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html Searching Your Data>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- The endpoint for submitting @Search@ requests is domain-specific. You
-- submit search requests to a domain\'s search endpoint. To get the search
-- endpoint for your domain, use the Amazon CloudSearch configuration
-- service @DescribeDomains@ action. A domain\'s endpoints are also
-- displayed on the domain dashboard in the Amazon CloudSearch console.
module Amazonka.CloudSearchDomains.Search
  ( -- * Creating a Request
    Search (..),
    newSearch,

    -- * Request Lenses
    search_filterQuery,
    search_queryParser,
    search_start,
    search_return,
    search_cursor,
    search_size,
    search_stats,
    search_facet,
    search_sort,
    search_queryOptions,
    search_partial,
    search_highlight,
    search_expr,
    search_query,

    -- * Destructuring the Response
    SearchResponse (..),
    newSearchResponse,

    -- * Response Lenses
    searchResponse_facets,
    searchResponse_stats,
    searchResponse_hits,
    searchResponse_status,
    searchResponse_httpStatus,
  )
where

import Amazonka.CloudSearchDomains.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @Search@ request.
--
-- /See:/ 'newSearch' smart constructor.
data Search = Search'
  { -- | Specifies a structured query that filters the results of a search
    -- without affecting how the results are scored and sorted. You use
    -- @filterQuery@ in conjunction with the @query@ parameter to filter the
    -- documents that match the constraints specified in the @query@ parameter.
    -- Specifying a filter controls only which matching documents are included
    -- in the results, it has no effect on how they are scored and sorted. The
    -- @filterQuery@ parameter supports the full structured query syntax.
    --
    -- For more information about using filters, see
    -- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/filtering-results.html Filtering Matching Documents>
    -- in the /Amazon CloudSearch Developer Guide/.
    filterQuery :: Prelude.Maybe Prelude.Text,
    -- | Specifies which query parser to use to process the request. If
    -- @queryParser@ is not specified, Amazon CloudSearch uses the @simple@
    -- query parser.
    --
    -- Amazon CloudSearch supports four query parsers:
    --
    -- -   @simple@: perform simple searches of @text@ and @text-array@ fields.
    --     By default, the @simple@ query parser searches all @text@ and
    --     @text-array@ fields. You can specify which fields to search by with
    --     the @queryOptions@ parameter. If you prefix a search term with a
    --     plus sign (+) documents must contain the term to be considered a
    --     match. (This is the default, unless you configure the default
    --     operator with the @queryOptions@ parameter.) You can use the @-@
    --     (NOT), @|@ (OR), and @*@ (wildcard) operators to exclude particular
    --     terms, find results that match any of the specified terms, or search
    --     for a prefix. To search for a phrase rather than individual terms,
    --     enclose the phrase in double quotes. For more information, see
    --     <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-text.html Searching for Text>
    --     in the /Amazon CloudSearch Developer Guide/.
    -- -   @structured@: perform advanced searches by combining multiple
    --     expressions to define the search criteria. You can also search
    --     within particular fields, search for values and ranges of values,
    --     and use advanced options such as term boosting, @matchall@, and
    --     @near@. For more information, see
    --     <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-compound-queries.html Constructing Compound Queries>
    --     in the /Amazon CloudSearch Developer Guide/.
    -- -   @lucene@: search using the Apache Lucene query parser syntax. For
    --     more information, see
    --     <http://lucene.apache.org/core/4_6_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package_description Apache Lucene Query Parser Syntax>.
    -- -   @dismax@: search using the simplified subset of the Apache Lucene
    --     query parser syntax defined by the DisMax query parser. For more
    --     information, see
    --     <http://wiki.apache.org/solr/DisMaxQParserPlugin#Query_Syntax DisMax Query Parser Syntax>.
    queryParser :: Prelude.Maybe QueryParser,
    -- | Specifies the offset of the first search hit you want to return. Note
    -- that the result set is zero-based; the first result is at index 0. You
    -- can specify either the @start@ or @cursor@ parameter in a request, they
    -- are mutually exclusive.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results>
    -- in the /Amazon CloudSearch Developer Guide/.
    start :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the field and expression values to include in the response.
    -- Multiple fields or expressions are specified as a comma-separated list.
    -- By default, a search response includes all return enabled fields
    -- (@_all_fields@). To return only the document IDs for the matching
    -- documents, specify @_no_fields@. To retrieve the relevance score
    -- calculated for each document, specify @_score@.
    return' :: Prelude.Maybe Prelude.Text,
    -- | Retrieves a cursor value you can use to page through large result sets.
    -- Use the @size@ parameter to control the number of hits to include in
    -- each response. You can specify either the @cursor@ or @start@ parameter
    -- in a request; they are mutually exclusive. To get the first cursor, set
    -- the cursor value to @initial@. In subsequent requests, specify the
    -- cursor value returned in the hits section of the response.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results>
    -- in the /Amazon CloudSearch Developer Guide/.
    cursor :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of search hits to include in the response.
    size :: Prelude.Maybe Prelude.Integer,
    -- | Specifies one or more fields for which to get statistics information.
    -- Each specified field must be facet-enabled in the domain configuration.
    -- The fields are specified in JSON using the form:
    --
    -- @{\"FIELD-A\":{},\"FIELD-B\":{}}@
    --
    -- There are currently no options supported for statistics.
    stats :: Prelude.Maybe Prelude.Text,
    -- | Specifies one or more fields for which to get facet information, and
    -- options that control how the facet information is returned. Each
    -- specified field must be facet-enabled in the domain configuration. The
    -- fields and options are specified in JSON using the form
    -- @{\"FIELD\":{\"OPTION\":VALUE,\"OPTION:\"STRING\"},\"FIELD\":{\"OPTION\":VALUE,\"OPTION\":\"STRING\"}}@.
    --
    -- You can specify the following faceting options:
    --
    -- -   @buckets@ specifies an array of the facet values or ranges to count.
    --     Ranges are specified using the same syntax that you use to search
    --     for a range of values. For more information, see
    --     <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-ranges.html Searching for a Range of Values>
    --     in the /Amazon CloudSearch Developer Guide/. Buckets are returned in
    --     the order they are specified in the request. The @sort@ and @size@
    --     options are not valid if you specify @buckets@.
    --
    -- -   @size@ specifies the maximum number of facets to include in the
    --     results. By default, Amazon CloudSearch returns counts for the top
    --     10. The @size@ parameter is only valid when you specify the @sort@
    --     option; it cannot be used in conjunction with @buckets@.
    --
    -- -   @sort@ specifies how you want to sort the facets in the results:
    --     @bucket@ or @count@. Specify @bucket@ to sort alphabetically or
    --     numerically by facet value (in ascending order). Specify @count@ to
    --     sort by the facet counts computed for each facet value (in
    --     descending order). To retrieve facet counts for particular values or
    --     ranges of values, use the @buckets@ option instead of @sort@.
    --
    -- If no facet options are specified, facet counts are computed for all
    -- field values, the facets are sorted by facet count, and the top 10
    -- facets are returned in the results.
    --
    -- To count particular buckets of values, use the @buckets@ option. For
    -- example, the following request uses the @buckets@ option to calculate
    -- and return facet counts by decade.
    --
    -- @ {\"year\":{\"buckets\":[\"[1970,1979]\",\"[1980,1989]\",\"[1990,1999]\",\"[2000,2009]\",\"[2010,}\"]}} @
    --
    -- To sort facets by facet count, use the @count@ option. For example, the
    -- following request sets the @sort@ option to @count@ to sort the facet
    -- values by facet count, with the facet values that have the most matching
    -- documents listed first. Setting the @size@ option to 3 returns only the
    -- top three facet values.
    --
    -- @ {\"year\":{\"sort\":\"count\",\"size\":3}} @
    --
    -- To sort the facets by value, use the @bucket@ option. For example, the
    -- following request sets the @sort@ option to @bucket@ to sort the facet
    -- values numerically by year, with earliest year listed first.
    --
    -- @ {\"year\":{\"sort\":\"bucket\"}} @
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/faceting.html Getting and Using Facet Information>
    -- in the /Amazon CloudSearch Developer Guide/.
    facet :: Prelude.Maybe Prelude.Text,
    -- | Specifies the fields or custom expressions to use to sort the search
    -- results. Multiple fields or expressions are specified as a
    -- comma-separated list. You must specify the sort direction (@asc@ or
    -- @desc@) for each field; for example, @year desc,title asc@. To use a
    -- field to sort results, the field must be sort-enabled in the domain
    -- configuration. Array type fields cannot be used for sorting. If no
    -- @sort@ parameter is specified, results are sorted by their default
    -- relevance scores in descending order: @_score desc@. You can also sort
    -- by document ID (@_id asc@) and version (@_version desc@).
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/sorting-results.html Sorting Results>
    -- in the /Amazon CloudSearch Developer Guide/.
    sort :: Prelude.Maybe Prelude.Text,
    -- | Configures options for the query parser specified in the @queryParser@
    -- parameter. You specify the options in JSON using the following form
    -- @{\"OPTION1\":\"VALUE1\",\"OPTION2\":VALUE2\"...\"OPTIONN\":\"VALUEN\"}.@
    --
    -- The options you can configure vary according to which parser you use:
    --
    -- -   @defaultOperator@: The default operator used to combine individual
    --     terms in the search string. For example: @defaultOperator: \'or\'@.
    --     For the @dismax@ parser, you specify a percentage that represents
    --     the percentage of terms in the search string (rounded down) that
    --     must match, rather than a default operator. A value of @0%@ is the
    --     equivalent to OR, and a value of @100%@ is equivalent to AND. The
    --     percentage must be specified as a value in the range 0-100 followed
    --     by the percent (%) symbol. For example, @defaultOperator: 50%@.
    --     Valid values: @and@, @or@, a percentage in the range 0%-100%
    --     (@dismax@). Default: @and@ (@simple@, @structured@, @lucene@) or
    --     @100@ (@dismax@). Valid for: @simple@, @structured@, @lucene@, and
    --     @dismax@.
    -- -   @fields@: An array of the fields to search when no fields are
    --     specified in a search. If no fields are specified in a search and
    --     this option is not specified, all text and text-array fields are
    --     searched. You can specify a weight for each field to control the
    --     relative importance of each field when Amazon CloudSearch calculates
    --     relevance scores. To specify a field weight, append a caret (@^@)
    --     symbol and the weight to the field name. For example, to boost the
    --     importance of the @title@ field over the @description@ field you
    --     could specify: @\"fields\":[\"title^5\",\"description\"]@. Valid
    --     values: The name of any configured field and an optional numeric
    --     value greater than zero. Default: All @text@ and @text-array@
    --     fields. Valid for: @simple@, @structured@, @lucene@, and @dismax@.
    -- -   @operators@: An array of the operators or special characters you
    --     want to disable for the simple query parser. If you disable the
    --     @and@, @or@, or @not@ operators, the corresponding operators (@+@,
    --     @|@, @-@) have no special meaning and are dropped from the search
    --     string. Similarly, disabling @prefix@ disables the wildcard operator
    --     (@*@) and disabling @phrase@ disables the ability to search for
    --     phrases by enclosing phrases in double quotes. Disabling precedence
    --     disables the ability to control order of precedence using
    --     parentheses. Disabling @near@ disables the ability to use the ~
    --     operator to perform a sloppy phrase search. Disabling the @fuzzy@
    --     operator disables the ability to use the ~ operator to perform a
    --     fuzzy search. @escape@ disables the ability to use a backslash
    --     (@\\@) to escape special characters within the search string.
    --     Disabling whitespace is an advanced option that prevents the parser
    --     from tokenizing on whitespace, which can be useful for Vietnamese.
    --     (It prevents Vietnamese words from being split incorrectly.) For
    --     example, you could disable all operators other than the phrase
    --     operator to support just simple term and phrase queries:
    --     @\"operators\":[\"and\",\"not\",\"or\", \"prefix\"]@. Valid values:
    --     @and@, @escape@, @fuzzy@, @near@, @not@, @or@, @phrase@,
    --     @precedence@, @prefix@, @whitespace@. Default: All operators and
    --     special characters are enabled. Valid for: @simple@.
    -- -   @phraseFields@: An array of the @text@ or @text-array@ fields you
    --     want to use for phrase searches. When the terms in the search string
    --     appear in close proximity within a field, the field scores higher.
    --     You can specify a weight for each field to boost that score. The
    --     @phraseSlop@ option controls how much the matches can deviate from
    --     the search string and still be boosted. To specify a field weight,
    --     append a caret (@^@) symbol and the weight to the field name. For
    --     example, to boost phrase matches in the @title@ field over the
    --     @abstract@ field, you could specify:
    --     @\"phraseFields\":[\"title^3\", \"plot\"]@ Valid values: The name of
    --     any @text@ or @text-array@ field and an optional numeric value
    --     greater than zero. Default: No fields. If you don\'t specify any
    --     fields with @phraseFields@, proximity scoring is disabled even if
    --     @phraseSlop@ is specified. Valid for: @dismax@.
    -- -   @phraseSlop@: An integer value that specifies how much matches can
    --     deviate from the search phrase and still be boosted according to the
    --     weights specified in the @phraseFields@ option; for example,
    --     @phraseSlop: 2@. You must also specify @phraseFields@ to enable
    --     proximity scoring. Valid values: positive integers. Default: 0.
    --     Valid for: @dismax@.
    -- -   @explicitPhraseSlop@: An integer value that specifies how much a
    --     match can deviate from the search phrase when the phrase is enclosed
    --     in double quotes in the search string. (Phrases that exceed this
    --     proximity distance are not considered a match.) For example, to
    --     specify a slop of three for dismax phrase queries, you would specify
    --     @\"explicitPhraseSlop\":3@. Valid values: positive integers.
    --     Default: 0. Valid for: @dismax@.
    -- -   @tieBreaker@: When a term in the search string is found in a
    --     document\'s field, a score is calculated for that field based on how
    --     common the word is in that field compared to other documents. If the
    --     term occurs in multiple fields within a document, by default only
    --     the highest scoring field contributes to the document\'s overall
    --     score. You can specify a @tieBreaker@ value to enable the matches in
    --     lower-scoring fields to contribute to the document\'s score. That
    --     way, if two documents have the same max field score for a particular
    --     term, the score for the document that has matches in more fields
    --     will be higher. The formula for calculating the score with a
    --     tieBreaker is
    --     @(max field score) + (tieBreaker) * (sum of the scores for the rest of the matching fields)@.
    --     Set @tieBreaker@ to 0 to disregard all but the highest scoring field
    --     (pure max): @\"tieBreaker\":0@. Set to 1 to sum the scores from all
    --     fields (pure sum): @\"tieBreaker\":1@. Valid values: 0.0 to 1.0.
    --     Default: 0.0. Valid for: @dismax@.
    queryOptions :: Prelude.Maybe Prelude.Text,
    -- | Enables partial results to be returned if one or more index partitions
    -- are unavailable. When your search index is partitioned across multiple
    -- search instances, by default Amazon CloudSearch only returns results if
    -- every partition can be queried. This means that the failure of a single
    -- search instance can result in 5xx (internal server) errors. When you
    -- enable partial results, Amazon CloudSearch returns whatever results are
    -- available and includes the percentage of documents searched in the
    -- search results (percent-searched). This enables you to more gracefully
    -- degrade your users\' search experience. For example, rather than
    -- displaying no results, you could display the partial results and a
    -- message indicating that the results might be incomplete due to a
    -- temporary system outage.
    partial :: Prelude.Maybe Prelude.Bool,
    -- | Retrieves highlights for matches in the specified @text@ or @text-array@
    -- fields. Each specified field must be highlight enabled in the domain
    -- configuration. The fields and options are specified in JSON using the
    -- form
    -- @{\"FIELD\":{\"OPTION\":VALUE,\"OPTION:\"STRING\"},\"FIELD\":{\"OPTION\":VALUE,\"OPTION\":\"STRING\"}}@.
    --
    -- You can specify the following highlight options:
    --
    -- -   @format@: specifies the format of the data in the text field: @text@
    --     or @html@. When data is returned as HTML, all non-alphanumeric
    --     characters are encoded. The default is @html@.
    -- -   @max_phrases@: specifies the maximum number of occurrences of the
    --     search term(s) you want to highlight. By default, the first
    --     occurrence is highlighted.
    -- -   @pre_tag@: specifies the string to prepend to an occurrence of a
    --     search term. The default for HTML highlights is @&lt;em&gt;@. The
    --     default for text highlights is @*@.
    -- -   @post_tag@: specifies the string to append to an occurrence of a
    --     search term. The default for HTML highlights is @&lt;\/em&gt;@. The
    --     default for text highlights is @*@.
    --
    -- If no highlight options are specified for a field, the returned field
    -- text is treated as HTML and the first match is highlighted with emphasis
    -- tags: @&lt;em>search-term&lt;\/em&gt;@.
    --
    -- For example, the following request retrieves highlights for the @actors@
    -- and @title@ fields.
    --
    -- @{ \"actors\": {}, \"title\": {\"format\": \"text\",\"max_phrases\": 2,\"pre_tag\": \"\",\"post_tag\": \"\"} }@
    highlight :: Prelude.Maybe Prelude.Text,
    -- | Defines one or more numeric expressions that can be used to sort results
    -- or specify search or filter criteria. You can also specify expressions
    -- as return fields.
    --
    -- You specify the expressions in JSON using the form
    -- @{\"EXPRESSIONNAME\":\"EXPRESSION\"}@. You can define and use multiple
    -- expressions in a search request. For example:
    --
    -- @ {\"expression1\":\"_score*rating\", \"expression2\":\"(1\/rank)*year\"} @
    --
    -- For information about the variables, operators, and functions you can
    -- use in expressions, see
    -- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html#writing-expressions Writing Expressions>
    -- in the /Amazon CloudSearch Developer Guide/.
    expr :: Prelude.Maybe Prelude.Text,
    -- | Specifies the search criteria for the request. How you specify the
    -- search criteria depends on the query parser used for the request and the
    -- parser options specified in the @queryOptions@ parameter. By default,
    -- the @simple@ query parser is used to process requests. To use the
    -- @structured@, @lucene@, or @dismax@ query parser, you must also specify
    -- the @queryParser@ parameter.
    --
    -- For more information about specifying search criteria, see
    -- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html Searching Your Data>
    -- in the /Amazon CloudSearch Developer Guide/.
    query :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Search' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterQuery', 'search_filterQuery' - Specifies a structured query that filters the results of a search
-- without affecting how the results are scored and sorted. You use
-- @filterQuery@ in conjunction with the @query@ parameter to filter the
-- documents that match the constraints specified in the @query@ parameter.
-- Specifying a filter controls only which matching documents are included
-- in the results, it has no effect on how they are scored and sorted. The
-- @filterQuery@ parameter supports the full structured query syntax.
--
-- For more information about using filters, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/filtering-results.html Filtering Matching Documents>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- 'queryParser', 'search_queryParser' - Specifies which query parser to use to process the request. If
-- @queryParser@ is not specified, Amazon CloudSearch uses the @simple@
-- query parser.
--
-- Amazon CloudSearch supports four query parsers:
--
-- -   @simple@: perform simple searches of @text@ and @text-array@ fields.
--     By default, the @simple@ query parser searches all @text@ and
--     @text-array@ fields. You can specify which fields to search by with
--     the @queryOptions@ parameter. If you prefix a search term with a
--     plus sign (+) documents must contain the term to be considered a
--     match. (This is the default, unless you configure the default
--     operator with the @queryOptions@ parameter.) You can use the @-@
--     (NOT), @|@ (OR), and @*@ (wildcard) operators to exclude particular
--     terms, find results that match any of the specified terms, or search
--     for a prefix. To search for a phrase rather than individual terms,
--     enclose the phrase in double quotes. For more information, see
--     <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-text.html Searching for Text>
--     in the /Amazon CloudSearch Developer Guide/.
-- -   @structured@: perform advanced searches by combining multiple
--     expressions to define the search criteria. You can also search
--     within particular fields, search for values and ranges of values,
--     and use advanced options such as term boosting, @matchall@, and
--     @near@. For more information, see
--     <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-compound-queries.html Constructing Compound Queries>
--     in the /Amazon CloudSearch Developer Guide/.
-- -   @lucene@: search using the Apache Lucene query parser syntax. For
--     more information, see
--     <http://lucene.apache.org/core/4_6_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package_description Apache Lucene Query Parser Syntax>.
-- -   @dismax@: search using the simplified subset of the Apache Lucene
--     query parser syntax defined by the DisMax query parser. For more
--     information, see
--     <http://wiki.apache.org/solr/DisMaxQParserPlugin#Query_Syntax DisMax Query Parser Syntax>.
--
-- 'start', 'search_start' - Specifies the offset of the first search hit you want to return. Note
-- that the result set is zero-based; the first result is at index 0. You
-- can specify either the @start@ or @cursor@ parameter in a request, they
-- are mutually exclusive.
--
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- 'return'', 'search_return' - Specifies the field and expression values to include in the response.
-- Multiple fields or expressions are specified as a comma-separated list.
-- By default, a search response includes all return enabled fields
-- (@_all_fields@). To return only the document IDs for the matching
-- documents, specify @_no_fields@. To retrieve the relevance score
-- calculated for each document, specify @_score@.
--
-- 'cursor', 'search_cursor' - Retrieves a cursor value you can use to page through large result sets.
-- Use the @size@ parameter to control the number of hits to include in
-- each response. You can specify either the @cursor@ or @start@ parameter
-- in a request; they are mutually exclusive. To get the first cursor, set
-- the cursor value to @initial@. In subsequent requests, specify the
-- cursor value returned in the hits section of the response.
--
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- 'size', 'search_size' - Specifies the maximum number of search hits to include in the response.
--
-- 'stats', 'search_stats' - Specifies one or more fields for which to get statistics information.
-- Each specified field must be facet-enabled in the domain configuration.
-- The fields are specified in JSON using the form:
--
-- @{\"FIELD-A\":{},\"FIELD-B\":{}}@
--
-- There are currently no options supported for statistics.
--
-- 'facet', 'search_facet' - Specifies one or more fields for which to get facet information, and
-- options that control how the facet information is returned. Each
-- specified field must be facet-enabled in the domain configuration. The
-- fields and options are specified in JSON using the form
-- @{\"FIELD\":{\"OPTION\":VALUE,\"OPTION:\"STRING\"},\"FIELD\":{\"OPTION\":VALUE,\"OPTION\":\"STRING\"}}@.
--
-- You can specify the following faceting options:
--
-- -   @buckets@ specifies an array of the facet values or ranges to count.
--     Ranges are specified using the same syntax that you use to search
--     for a range of values. For more information, see
--     <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-ranges.html Searching for a Range of Values>
--     in the /Amazon CloudSearch Developer Guide/. Buckets are returned in
--     the order they are specified in the request. The @sort@ and @size@
--     options are not valid if you specify @buckets@.
--
-- -   @size@ specifies the maximum number of facets to include in the
--     results. By default, Amazon CloudSearch returns counts for the top
--     10. The @size@ parameter is only valid when you specify the @sort@
--     option; it cannot be used in conjunction with @buckets@.
--
-- -   @sort@ specifies how you want to sort the facets in the results:
--     @bucket@ or @count@. Specify @bucket@ to sort alphabetically or
--     numerically by facet value (in ascending order). Specify @count@ to
--     sort by the facet counts computed for each facet value (in
--     descending order). To retrieve facet counts for particular values or
--     ranges of values, use the @buckets@ option instead of @sort@.
--
-- If no facet options are specified, facet counts are computed for all
-- field values, the facets are sorted by facet count, and the top 10
-- facets are returned in the results.
--
-- To count particular buckets of values, use the @buckets@ option. For
-- example, the following request uses the @buckets@ option to calculate
-- and return facet counts by decade.
--
-- @ {\"year\":{\"buckets\":[\"[1970,1979]\",\"[1980,1989]\",\"[1990,1999]\",\"[2000,2009]\",\"[2010,}\"]}} @
--
-- To sort facets by facet count, use the @count@ option. For example, the
-- following request sets the @sort@ option to @count@ to sort the facet
-- values by facet count, with the facet values that have the most matching
-- documents listed first. Setting the @size@ option to 3 returns only the
-- top three facet values.
--
-- @ {\"year\":{\"sort\":\"count\",\"size\":3}} @
--
-- To sort the facets by value, use the @bucket@ option. For example, the
-- following request sets the @sort@ option to @bucket@ to sort the facet
-- values numerically by year, with earliest year listed first.
--
-- @ {\"year\":{\"sort\":\"bucket\"}} @
--
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/faceting.html Getting and Using Facet Information>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- 'sort', 'search_sort' - Specifies the fields or custom expressions to use to sort the search
-- results. Multiple fields or expressions are specified as a
-- comma-separated list. You must specify the sort direction (@asc@ or
-- @desc@) for each field; for example, @year desc,title asc@. To use a
-- field to sort results, the field must be sort-enabled in the domain
-- configuration. Array type fields cannot be used for sorting. If no
-- @sort@ parameter is specified, results are sorted by their default
-- relevance scores in descending order: @_score desc@. You can also sort
-- by document ID (@_id asc@) and version (@_version desc@).
--
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/sorting-results.html Sorting Results>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- 'queryOptions', 'search_queryOptions' - Configures options for the query parser specified in the @queryParser@
-- parameter. You specify the options in JSON using the following form
-- @{\"OPTION1\":\"VALUE1\",\"OPTION2\":VALUE2\"...\"OPTIONN\":\"VALUEN\"}.@
--
-- The options you can configure vary according to which parser you use:
--
-- -   @defaultOperator@: The default operator used to combine individual
--     terms in the search string. For example: @defaultOperator: \'or\'@.
--     For the @dismax@ parser, you specify a percentage that represents
--     the percentage of terms in the search string (rounded down) that
--     must match, rather than a default operator. A value of @0%@ is the
--     equivalent to OR, and a value of @100%@ is equivalent to AND. The
--     percentage must be specified as a value in the range 0-100 followed
--     by the percent (%) symbol. For example, @defaultOperator: 50%@.
--     Valid values: @and@, @or@, a percentage in the range 0%-100%
--     (@dismax@). Default: @and@ (@simple@, @structured@, @lucene@) or
--     @100@ (@dismax@). Valid for: @simple@, @structured@, @lucene@, and
--     @dismax@.
-- -   @fields@: An array of the fields to search when no fields are
--     specified in a search. If no fields are specified in a search and
--     this option is not specified, all text and text-array fields are
--     searched. You can specify a weight for each field to control the
--     relative importance of each field when Amazon CloudSearch calculates
--     relevance scores. To specify a field weight, append a caret (@^@)
--     symbol and the weight to the field name. For example, to boost the
--     importance of the @title@ field over the @description@ field you
--     could specify: @\"fields\":[\"title^5\",\"description\"]@. Valid
--     values: The name of any configured field and an optional numeric
--     value greater than zero. Default: All @text@ and @text-array@
--     fields. Valid for: @simple@, @structured@, @lucene@, and @dismax@.
-- -   @operators@: An array of the operators or special characters you
--     want to disable for the simple query parser. If you disable the
--     @and@, @or@, or @not@ operators, the corresponding operators (@+@,
--     @|@, @-@) have no special meaning and are dropped from the search
--     string. Similarly, disabling @prefix@ disables the wildcard operator
--     (@*@) and disabling @phrase@ disables the ability to search for
--     phrases by enclosing phrases in double quotes. Disabling precedence
--     disables the ability to control order of precedence using
--     parentheses. Disabling @near@ disables the ability to use the ~
--     operator to perform a sloppy phrase search. Disabling the @fuzzy@
--     operator disables the ability to use the ~ operator to perform a
--     fuzzy search. @escape@ disables the ability to use a backslash
--     (@\\@) to escape special characters within the search string.
--     Disabling whitespace is an advanced option that prevents the parser
--     from tokenizing on whitespace, which can be useful for Vietnamese.
--     (It prevents Vietnamese words from being split incorrectly.) For
--     example, you could disable all operators other than the phrase
--     operator to support just simple term and phrase queries:
--     @\"operators\":[\"and\",\"not\",\"or\", \"prefix\"]@. Valid values:
--     @and@, @escape@, @fuzzy@, @near@, @not@, @or@, @phrase@,
--     @precedence@, @prefix@, @whitespace@. Default: All operators and
--     special characters are enabled. Valid for: @simple@.
-- -   @phraseFields@: An array of the @text@ or @text-array@ fields you
--     want to use for phrase searches. When the terms in the search string
--     appear in close proximity within a field, the field scores higher.
--     You can specify a weight for each field to boost that score. The
--     @phraseSlop@ option controls how much the matches can deviate from
--     the search string and still be boosted. To specify a field weight,
--     append a caret (@^@) symbol and the weight to the field name. For
--     example, to boost phrase matches in the @title@ field over the
--     @abstract@ field, you could specify:
--     @\"phraseFields\":[\"title^3\", \"plot\"]@ Valid values: The name of
--     any @text@ or @text-array@ field and an optional numeric value
--     greater than zero. Default: No fields. If you don\'t specify any
--     fields with @phraseFields@, proximity scoring is disabled even if
--     @phraseSlop@ is specified. Valid for: @dismax@.
-- -   @phraseSlop@: An integer value that specifies how much matches can
--     deviate from the search phrase and still be boosted according to the
--     weights specified in the @phraseFields@ option; for example,
--     @phraseSlop: 2@. You must also specify @phraseFields@ to enable
--     proximity scoring. Valid values: positive integers. Default: 0.
--     Valid for: @dismax@.
-- -   @explicitPhraseSlop@: An integer value that specifies how much a
--     match can deviate from the search phrase when the phrase is enclosed
--     in double quotes in the search string. (Phrases that exceed this
--     proximity distance are not considered a match.) For example, to
--     specify a slop of three for dismax phrase queries, you would specify
--     @\"explicitPhraseSlop\":3@. Valid values: positive integers.
--     Default: 0. Valid for: @dismax@.
-- -   @tieBreaker@: When a term in the search string is found in a
--     document\'s field, a score is calculated for that field based on how
--     common the word is in that field compared to other documents. If the
--     term occurs in multiple fields within a document, by default only
--     the highest scoring field contributes to the document\'s overall
--     score. You can specify a @tieBreaker@ value to enable the matches in
--     lower-scoring fields to contribute to the document\'s score. That
--     way, if two documents have the same max field score for a particular
--     term, the score for the document that has matches in more fields
--     will be higher. The formula for calculating the score with a
--     tieBreaker is
--     @(max field score) + (tieBreaker) * (sum of the scores for the rest of the matching fields)@.
--     Set @tieBreaker@ to 0 to disregard all but the highest scoring field
--     (pure max): @\"tieBreaker\":0@. Set to 1 to sum the scores from all
--     fields (pure sum): @\"tieBreaker\":1@. Valid values: 0.0 to 1.0.
--     Default: 0.0. Valid for: @dismax@.
--
-- 'partial', 'search_partial' - Enables partial results to be returned if one or more index partitions
-- are unavailable. When your search index is partitioned across multiple
-- search instances, by default Amazon CloudSearch only returns results if
-- every partition can be queried. This means that the failure of a single
-- search instance can result in 5xx (internal server) errors. When you
-- enable partial results, Amazon CloudSearch returns whatever results are
-- available and includes the percentage of documents searched in the
-- search results (percent-searched). This enables you to more gracefully
-- degrade your users\' search experience. For example, rather than
-- displaying no results, you could display the partial results and a
-- message indicating that the results might be incomplete due to a
-- temporary system outage.
--
-- 'highlight', 'search_highlight' - Retrieves highlights for matches in the specified @text@ or @text-array@
-- fields. Each specified field must be highlight enabled in the domain
-- configuration. The fields and options are specified in JSON using the
-- form
-- @{\"FIELD\":{\"OPTION\":VALUE,\"OPTION:\"STRING\"},\"FIELD\":{\"OPTION\":VALUE,\"OPTION\":\"STRING\"}}@.
--
-- You can specify the following highlight options:
--
-- -   @format@: specifies the format of the data in the text field: @text@
--     or @html@. When data is returned as HTML, all non-alphanumeric
--     characters are encoded. The default is @html@.
-- -   @max_phrases@: specifies the maximum number of occurrences of the
--     search term(s) you want to highlight. By default, the first
--     occurrence is highlighted.
-- -   @pre_tag@: specifies the string to prepend to an occurrence of a
--     search term. The default for HTML highlights is @&lt;em&gt;@. The
--     default for text highlights is @*@.
-- -   @post_tag@: specifies the string to append to an occurrence of a
--     search term. The default for HTML highlights is @&lt;\/em&gt;@. The
--     default for text highlights is @*@.
--
-- If no highlight options are specified for a field, the returned field
-- text is treated as HTML and the first match is highlighted with emphasis
-- tags: @&lt;em>search-term&lt;\/em&gt;@.
--
-- For example, the following request retrieves highlights for the @actors@
-- and @title@ fields.
--
-- @{ \"actors\": {}, \"title\": {\"format\": \"text\",\"max_phrases\": 2,\"pre_tag\": \"\",\"post_tag\": \"\"} }@
--
-- 'expr', 'search_expr' - Defines one or more numeric expressions that can be used to sort results
-- or specify search or filter criteria. You can also specify expressions
-- as return fields.
--
-- You specify the expressions in JSON using the form
-- @{\"EXPRESSIONNAME\":\"EXPRESSION\"}@. You can define and use multiple
-- expressions in a search request. For example:
--
-- @ {\"expression1\":\"_score*rating\", \"expression2\":\"(1\/rank)*year\"} @
--
-- For information about the variables, operators, and functions you can
-- use in expressions, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html#writing-expressions Writing Expressions>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- 'query', 'search_query' - Specifies the search criteria for the request. How you specify the
-- search criteria depends on the query parser used for the request and the
-- parser options specified in the @queryOptions@ parameter. By default,
-- the @simple@ query parser is used to process requests. To use the
-- @structured@, @lucene@, or @dismax@ query parser, you must also specify
-- the @queryParser@ parameter.
--
-- For more information about specifying search criteria, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html Searching Your Data>
-- in the /Amazon CloudSearch Developer Guide/.
newSearch ::
  -- | 'query'
  Prelude.Text ->
  Search
newSearch pQuery_ =
  Search'
    { filterQuery = Prelude.Nothing,
      queryParser = Prelude.Nothing,
      start = Prelude.Nothing,
      return' = Prelude.Nothing,
      cursor = Prelude.Nothing,
      size = Prelude.Nothing,
      stats = Prelude.Nothing,
      facet = Prelude.Nothing,
      sort = Prelude.Nothing,
      queryOptions = Prelude.Nothing,
      partial = Prelude.Nothing,
      highlight = Prelude.Nothing,
      expr = Prelude.Nothing,
      query = pQuery_
    }

-- | Specifies a structured query that filters the results of a search
-- without affecting how the results are scored and sorted. You use
-- @filterQuery@ in conjunction with the @query@ parameter to filter the
-- documents that match the constraints specified in the @query@ parameter.
-- Specifying a filter controls only which matching documents are included
-- in the results, it has no effect on how they are scored and sorted. The
-- @filterQuery@ parameter supports the full structured query syntax.
--
-- For more information about using filters, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/filtering-results.html Filtering Matching Documents>
-- in the /Amazon CloudSearch Developer Guide/.
search_filterQuery :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_filterQuery = Lens.lens (\Search' {filterQuery} -> filterQuery) (\s@Search' {} a -> s {filterQuery = a} :: Search)

-- | Specifies which query parser to use to process the request. If
-- @queryParser@ is not specified, Amazon CloudSearch uses the @simple@
-- query parser.
--
-- Amazon CloudSearch supports four query parsers:
--
-- -   @simple@: perform simple searches of @text@ and @text-array@ fields.
--     By default, the @simple@ query parser searches all @text@ and
--     @text-array@ fields. You can specify which fields to search by with
--     the @queryOptions@ parameter. If you prefix a search term with a
--     plus sign (+) documents must contain the term to be considered a
--     match. (This is the default, unless you configure the default
--     operator with the @queryOptions@ parameter.) You can use the @-@
--     (NOT), @|@ (OR), and @*@ (wildcard) operators to exclude particular
--     terms, find results that match any of the specified terms, or search
--     for a prefix. To search for a phrase rather than individual terms,
--     enclose the phrase in double quotes. For more information, see
--     <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-text.html Searching for Text>
--     in the /Amazon CloudSearch Developer Guide/.
-- -   @structured@: perform advanced searches by combining multiple
--     expressions to define the search criteria. You can also search
--     within particular fields, search for values and ranges of values,
--     and use advanced options such as term boosting, @matchall@, and
--     @near@. For more information, see
--     <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-compound-queries.html Constructing Compound Queries>
--     in the /Amazon CloudSearch Developer Guide/.
-- -   @lucene@: search using the Apache Lucene query parser syntax. For
--     more information, see
--     <http://lucene.apache.org/core/4_6_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package_description Apache Lucene Query Parser Syntax>.
-- -   @dismax@: search using the simplified subset of the Apache Lucene
--     query parser syntax defined by the DisMax query parser. For more
--     information, see
--     <http://wiki.apache.org/solr/DisMaxQParserPlugin#Query_Syntax DisMax Query Parser Syntax>.
search_queryParser :: Lens.Lens' Search (Prelude.Maybe QueryParser)
search_queryParser = Lens.lens (\Search' {queryParser} -> queryParser) (\s@Search' {} a -> s {queryParser = a} :: Search)

-- | Specifies the offset of the first search hit you want to return. Note
-- that the result set is zero-based; the first result is at index 0. You
-- can specify either the @start@ or @cursor@ parameter in a request, they
-- are mutually exclusive.
--
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results>
-- in the /Amazon CloudSearch Developer Guide/.
search_start :: Lens.Lens' Search (Prelude.Maybe Prelude.Integer)
search_start = Lens.lens (\Search' {start} -> start) (\s@Search' {} a -> s {start = a} :: Search)

-- | Specifies the field and expression values to include in the response.
-- Multiple fields or expressions are specified as a comma-separated list.
-- By default, a search response includes all return enabled fields
-- (@_all_fields@). To return only the document IDs for the matching
-- documents, specify @_no_fields@. To retrieve the relevance score
-- calculated for each document, specify @_score@.
search_return :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_return = Lens.lens (\Search' {return'} -> return') (\s@Search' {} a -> s {return' = a} :: Search)

-- | Retrieves a cursor value you can use to page through large result sets.
-- Use the @size@ parameter to control the number of hits to include in
-- each response. You can specify either the @cursor@ or @start@ parameter
-- in a request; they are mutually exclusive. To get the first cursor, set
-- the cursor value to @initial@. In subsequent requests, specify the
-- cursor value returned in the hits section of the response.
--
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/paginating-results.html Paginating Results>
-- in the /Amazon CloudSearch Developer Guide/.
search_cursor :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_cursor = Lens.lens (\Search' {cursor} -> cursor) (\s@Search' {} a -> s {cursor = a} :: Search)

-- | Specifies the maximum number of search hits to include in the response.
search_size :: Lens.Lens' Search (Prelude.Maybe Prelude.Integer)
search_size = Lens.lens (\Search' {size} -> size) (\s@Search' {} a -> s {size = a} :: Search)

-- | Specifies one or more fields for which to get statistics information.
-- Each specified field must be facet-enabled in the domain configuration.
-- The fields are specified in JSON using the form:
--
-- @{\"FIELD-A\":{},\"FIELD-B\":{}}@
--
-- There are currently no options supported for statistics.
search_stats :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_stats = Lens.lens (\Search' {stats} -> stats) (\s@Search' {} a -> s {stats = a} :: Search)

-- | Specifies one or more fields for which to get facet information, and
-- options that control how the facet information is returned. Each
-- specified field must be facet-enabled in the domain configuration. The
-- fields and options are specified in JSON using the form
-- @{\"FIELD\":{\"OPTION\":VALUE,\"OPTION:\"STRING\"},\"FIELD\":{\"OPTION\":VALUE,\"OPTION\":\"STRING\"}}@.
--
-- You can specify the following faceting options:
--
-- -   @buckets@ specifies an array of the facet values or ranges to count.
--     Ranges are specified using the same syntax that you use to search
--     for a range of values. For more information, see
--     <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching-ranges.html Searching for a Range of Values>
--     in the /Amazon CloudSearch Developer Guide/. Buckets are returned in
--     the order they are specified in the request. The @sort@ and @size@
--     options are not valid if you specify @buckets@.
--
-- -   @size@ specifies the maximum number of facets to include in the
--     results. By default, Amazon CloudSearch returns counts for the top
--     10. The @size@ parameter is only valid when you specify the @sort@
--     option; it cannot be used in conjunction with @buckets@.
--
-- -   @sort@ specifies how you want to sort the facets in the results:
--     @bucket@ or @count@. Specify @bucket@ to sort alphabetically or
--     numerically by facet value (in ascending order). Specify @count@ to
--     sort by the facet counts computed for each facet value (in
--     descending order). To retrieve facet counts for particular values or
--     ranges of values, use the @buckets@ option instead of @sort@.
--
-- If no facet options are specified, facet counts are computed for all
-- field values, the facets are sorted by facet count, and the top 10
-- facets are returned in the results.
--
-- To count particular buckets of values, use the @buckets@ option. For
-- example, the following request uses the @buckets@ option to calculate
-- and return facet counts by decade.
--
-- @ {\"year\":{\"buckets\":[\"[1970,1979]\",\"[1980,1989]\",\"[1990,1999]\",\"[2000,2009]\",\"[2010,}\"]}} @
--
-- To sort facets by facet count, use the @count@ option. For example, the
-- following request sets the @sort@ option to @count@ to sort the facet
-- values by facet count, with the facet values that have the most matching
-- documents listed first. Setting the @size@ option to 3 returns only the
-- top three facet values.
--
-- @ {\"year\":{\"sort\":\"count\",\"size\":3}} @
--
-- To sort the facets by value, use the @bucket@ option. For example, the
-- following request sets the @sort@ option to @bucket@ to sort the facet
-- values numerically by year, with earliest year listed first.
--
-- @ {\"year\":{\"sort\":\"bucket\"}} @
--
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/faceting.html Getting and Using Facet Information>
-- in the /Amazon CloudSearch Developer Guide/.
search_facet :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_facet = Lens.lens (\Search' {facet} -> facet) (\s@Search' {} a -> s {facet = a} :: Search)

-- | Specifies the fields or custom expressions to use to sort the search
-- results. Multiple fields or expressions are specified as a
-- comma-separated list. You must specify the sort direction (@asc@ or
-- @desc@) for each field; for example, @year desc,title asc@. To use a
-- field to sort results, the field must be sort-enabled in the domain
-- configuration. Array type fields cannot be used for sorting. If no
-- @sort@ parameter is specified, results are sorted by their default
-- relevance scores in descending order: @_score desc@. You can also sort
-- by document ID (@_id asc@) and version (@_version desc@).
--
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/sorting-results.html Sorting Results>
-- in the /Amazon CloudSearch Developer Guide/.
search_sort :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_sort = Lens.lens (\Search' {sort} -> sort) (\s@Search' {} a -> s {sort = a} :: Search)

-- | Configures options for the query parser specified in the @queryParser@
-- parameter. You specify the options in JSON using the following form
-- @{\"OPTION1\":\"VALUE1\",\"OPTION2\":VALUE2\"...\"OPTIONN\":\"VALUEN\"}.@
--
-- The options you can configure vary according to which parser you use:
--
-- -   @defaultOperator@: The default operator used to combine individual
--     terms in the search string. For example: @defaultOperator: \'or\'@.
--     For the @dismax@ parser, you specify a percentage that represents
--     the percentage of terms in the search string (rounded down) that
--     must match, rather than a default operator. A value of @0%@ is the
--     equivalent to OR, and a value of @100%@ is equivalent to AND. The
--     percentage must be specified as a value in the range 0-100 followed
--     by the percent (%) symbol. For example, @defaultOperator: 50%@.
--     Valid values: @and@, @or@, a percentage in the range 0%-100%
--     (@dismax@). Default: @and@ (@simple@, @structured@, @lucene@) or
--     @100@ (@dismax@). Valid for: @simple@, @structured@, @lucene@, and
--     @dismax@.
-- -   @fields@: An array of the fields to search when no fields are
--     specified in a search. If no fields are specified in a search and
--     this option is not specified, all text and text-array fields are
--     searched. You can specify a weight for each field to control the
--     relative importance of each field when Amazon CloudSearch calculates
--     relevance scores. To specify a field weight, append a caret (@^@)
--     symbol and the weight to the field name. For example, to boost the
--     importance of the @title@ field over the @description@ field you
--     could specify: @\"fields\":[\"title^5\",\"description\"]@. Valid
--     values: The name of any configured field and an optional numeric
--     value greater than zero. Default: All @text@ and @text-array@
--     fields. Valid for: @simple@, @structured@, @lucene@, and @dismax@.
-- -   @operators@: An array of the operators or special characters you
--     want to disable for the simple query parser. If you disable the
--     @and@, @or@, or @not@ operators, the corresponding operators (@+@,
--     @|@, @-@) have no special meaning and are dropped from the search
--     string. Similarly, disabling @prefix@ disables the wildcard operator
--     (@*@) and disabling @phrase@ disables the ability to search for
--     phrases by enclosing phrases in double quotes. Disabling precedence
--     disables the ability to control order of precedence using
--     parentheses. Disabling @near@ disables the ability to use the ~
--     operator to perform a sloppy phrase search. Disabling the @fuzzy@
--     operator disables the ability to use the ~ operator to perform a
--     fuzzy search. @escape@ disables the ability to use a backslash
--     (@\\@) to escape special characters within the search string.
--     Disabling whitespace is an advanced option that prevents the parser
--     from tokenizing on whitespace, which can be useful for Vietnamese.
--     (It prevents Vietnamese words from being split incorrectly.) For
--     example, you could disable all operators other than the phrase
--     operator to support just simple term and phrase queries:
--     @\"operators\":[\"and\",\"not\",\"or\", \"prefix\"]@. Valid values:
--     @and@, @escape@, @fuzzy@, @near@, @not@, @or@, @phrase@,
--     @precedence@, @prefix@, @whitespace@. Default: All operators and
--     special characters are enabled. Valid for: @simple@.
-- -   @phraseFields@: An array of the @text@ or @text-array@ fields you
--     want to use for phrase searches. When the terms in the search string
--     appear in close proximity within a field, the field scores higher.
--     You can specify a weight for each field to boost that score. The
--     @phraseSlop@ option controls how much the matches can deviate from
--     the search string and still be boosted. To specify a field weight,
--     append a caret (@^@) symbol and the weight to the field name. For
--     example, to boost phrase matches in the @title@ field over the
--     @abstract@ field, you could specify:
--     @\"phraseFields\":[\"title^3\", \"plot\"]@ Valid values: The name of
--     any @text@ or @text-array@ field and an optional numeric value
--     greater than zero. Default: No fields. If you don\'t specify any
--     fields with @phraseFields@, proximity scoring is disabled even if
--     @phraseSlop@ is specified. Valid for: @dismax@.
-- -   @phraseSlop@: An integer value that specifies how much matches can
--     deviate from the search phrase and still be boosted according to the
--     weights specified in the @phraseFields@ option; for example,
--     @phraseSlop: 2@. You must also specify @phraseFields@ to enable
--     proximity scoring. Valid values: positive integers. Default: 0.
--     Valid for: @dismax@.
-- -   @explicitPhraseSlop@: An integer value that specifies how much a
--     match can deviate from the search phrase when the phrase is enclosed
--     in double quotes in the search string. (Phrases that exceed this
--     proximity distance are not considered a match.) For example, to
--     specify a slop of three for dismax phrase queries, you would specify
--     @\"explicitPhraseSlop\":3@. Valid values: positive integers.
--     Default: 0. Valid for: @dismax@.
-- -   @tieBreaker@: When a term in the search string is found in a
--     document\'s field, a score is calculated for that field based on how
--     common the word is in that field compared to other documents. If the
--     term occurs in multiple fields within a document, by default only
--     the highest scoring field contributes to the document\'s overall
--     score. You can specify a @tieBreaker@ value to enable the matches in
--     lower-scoring fields to contribute to the document\'s score. That
--     way, if two documents have the same max field score for a particular
--     term, the score for the document that has matches in more fields
--     will be higher. The formula for calculating the score with a
--     tieBreaker is
--     @(max field score) + (tieBreaker) * (sum of the scores for the rest of the matching fields)@.
--     Set @tieBreaker@ to 0 to disregard all but the highest scoring field
--     (pure max): @\"tieBreaker\":0@. Set to 1 to sum the scores from all
--     fields (pure sum): @\"tieBreaker\":1@. Valid values: 0.0 to 1.0.
--     Default: 0.0. Valid for: @dismax@.
search_queryOptions :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_queryOptions = Lens.lens (\Search' {queryOptions} -> queryOptions) (\s@Search' {} a -> s {queryOptions = a} :: Search)

-- | Enables partial results to be returned if one or more index partitions
-- are unavailable. When your search index is partitioned across multiple
-- search instances, by default Amazon CloudSearch only returns results if
-- every partition can be queried. This means that the failure of a single
-- search instance can result in 5xx (internal server) errors. When you
-- enable partial results, Amazon CloudSearch returns whatever results are
-- available and includes the percentage of documents searched in the
-- search results (percent-searched). This enables you to more gracefully
-- degrade your users\' search experience. For example, rather than
-- displaying no results, you could display the partial results and a
-- message indicating that the results might be incomplete due to a
-- temporary system outage.
search_partial :: Lens.Lens' Search (Prelude.Maybe Prelude.Bool)
search_partial = Lens.lens (\Search' {partial} -> partial) (\s@Search' {} a -> s {partial = a} :: Search)

-- | Retrieves highlights for matches in the specified @text@ or @text-array@
-- fields. Each specified field must be highlight enabled in the domain
-- configuration. The fields and options are specified in JSON using the
-- form
-- @{\"FIELD\":{\"OPTION\":VALUE,\"OPTION:\"STRING\"},\"FIELD\":{\"OPTION\":VALUE,\"OPTION\":\"STRING\"}}@.
--
-- You can specify the following highlight options:
--
-- -   @format@: specifies the format of the data in the text field: @text@
--     or @html@. When data is returned as HTML, all non-alphanumeric
--     characters are encoded. The default is @html@.
-- -   @max_phrases@: specifies the maximum number of occurrences of the
--     search term(s) you want to highlight. By default, the first
--     occurrence is highlighted.
-- -   @pre_tag@: specifies the string to prepend to an occurrence of a
--     search term. The default for HTML highlights is @&lt;em&gt;@. The
--     default for text highlights is @*@.
-- -   @post_tag@: specifies the string to append to an occurrence of a
--     search term. The default for HTML highlights is @&lt;\/em&gt;@. The
--     default for text highlights is @*@.
--
-- If no highlight options are specified for a field, the returned field
-- text is treated as HTML and the first match is highlighted with emphasis
-- tags: @&lt;em>search-term&lt;\/em&gt;@.
--
-- For example, the following request retrieves highlights for the @actors@
-- and @title@ fields.
--
-- @{ \"actors\": {}, \"title\": {\"format\": \"text\",\"max_phrases\": 2,\"pre_tag\": \"\",\"post_tag\": \"\"} }@
search_highlight :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_highlight = Lens.lens (\Search' {highlight} -> highlight) (\s@Search' {} a -> s {highlight = a} :: Search)

-- | Defines one or more numeric expressions that can be used to sort results
-- or specify search or filter criteria. You can also specify expressions
-- as return fields.
--
-- You specify the expressions in JSON using the form
-- @{\"EXPRESSIONNAME\":\"EXPRESSION\"}@. You can define and use multiple
-- expressions in a search request. For example:
--
-- @ {\"expression1\":\"_score*rating\", \"expression2\":\"(1\/rank)*year\"} @
--
-- For information about the variables, operators, and functions you can
-- use in expressions, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html#writing-expressions Writing Expressions>
-- in the /Amazon CloudSearch Developer Guide/.
search_expr :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_expr = Lens.lens (\Search' {expr} -> expr) (\s@Search' {} a -> s {expr = a} :: Search)

-- | Specifies the search criteria for the request. How you specify the
-- search criteria depends on the query parser used for the request and the
-- parser options specified in the @queryOptions@ parameter. By default,
-- the @simple@ query parser is used to process requests. To use the
-- @structured@, @lucene@, or @dismax@ query parser, you must also specify
-- the @queryParser@ parameter.
--
-- For more information about specifying search criteria, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html Searching Your Data>
-- in the /Amazon CloudSearch Developer Guide/.
search_query :: Lens.Lens' Search Prelude.Text
search_query = Lens.lens (\Search' {query} -> query) (\s@Search' {} a -> s {query = a} :: Search)

instance Core.AWSRequest Search where
  type AWSResponse Search = SearchResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchResponse'
            Prelude.<$> (x Core..?> "facets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "stats" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "hits")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Search where
  hashWithSalt _salt Search' {..} =
    _salt `Prelude.hashWithSalt` filterQuery
      `Prelude.hashWithSalt` queryParser
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` return'
      `Prelude.hashWithSalt` cursor
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` stats
      `Prelude.hashWithSalt` facet
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` queryOptions
      `Prelude.hashWithSalt` partial
      `Prelude.hashWithSalt` highlight
      `Prelude.hashWithSalt` expr
      `Prelude.hashWithSalt` query

instance Prelude.NFData Search where
  rnf Search' {..} =
    Prelude.rnf filterQuery
      `Prelude.seq` Prelude.rnf queryParser
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf return'
      `Prelude.seq` Prelude.rnf cursor
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf stats
      `Prelude.seq` Prelude.rnf facet
      `Prelude.seq` Prelude.rnf sort
      `Prelude.seq` Prelude.rnf queryOptions
      `Prelude.seq` Prelude.rnf partial
      `Prelude.seq` Prelude.rnf highlight
      `Prelude.seq` Prelude.rnf expr
      `Prelude.seq` Prelude.rnf query

instance Core.ToHeaders Search where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath Search where
  toPath = Prelude.const "/2013-01-01/search"

instance Core.ToQuery Search where
  toQuery Search' {..} =
    Prelude.mconcat
      [ "fq" Core.=: filterQuery,
        "q.parser" Core.=: queryParser,
        "start" Core.=: start,
        "return" Core.=: return',
        "cursor" Core.=: cursor,
        "size" Core.=: size,
        "stats" Core.=: stats,
        "facet" Core.=: facet,
        "sort" Core.=: sort,
        "q.options" Core.=: queryOptions,
        "partial" Core.=: partial,
        "highlight" Core.=: highlight,
        "expr" Core.=: expr,
        "q" Core.=: query,
        "format=sdk&pretty=true"
      ]

-- | The result of a @Search@ request. Contains the documents that match the
-- specified search criteria and any requested fields, highlights, and
-- facet information.
--
-- /See:/ 'newSearchResponse' smart constructor.
data SearchResponse = SearchResponse'
  { -- | The requested facet information.
    facets :: Prelude.Maybe (Prelude.HashMap Prelude.Text BucketInfo),
    -- | The requested field statistics information.
    stats :: Prelude.Maybe (Prelude.HashMap Prelude.Text FieldStats),
    -- | The documents that match the search criteria.
    hits :: Prelude.Maybe Hits,
    -- | The status information returned for the search request.
    status :: Prelude.Maybe SearchStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facets', 'searchResponse_facets' - The requested facet information.
--
-- 'stats', 'searchResponse_stats' - The requested field statistics information.
--
-- 'hits', 'searchResponse_hits' - The documents that match the search criteria.
--
-- 'status', 'searchResponse_status' - The status information returned for the search request.
--
-- 'httpStatus', 'searchResponse_httpStatus' - The response's http status code.
newSearchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchResponse
newSearchResponse pHttpStatus_ =
  SearchResponse'
    { facets = Prelude.Nothing,
      stats = Prelude.Nothing,
      hits = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested facet information.
searchResponse_facets :: Lens.Lens' SearchResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text BucketInfo))
searchResponse_facets = Lens.lens (\SearchResponse' {facets} -> facets) (\s@SearchResponse' {} a -> s {facets = a} :: SearchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The requested field statistics information.
searchResponse_stats :: Lens.Lens' SearchResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text FieldStats))
searchResponse_stats = Lens.lens (\SearchResponse' {stats} -> stats) (\s@SearchResponse' {} a -> s {stats = a} :: SearchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The documents that match the search criteria.
searchResponse_hits :: Lens.Lens' SearchResponse (Prelude.Maybe Hits)
searchResponse_hits = Lens.lens (\SearchResponse' {hits} -> hits) (\s@SearchResponse' {} a -> s {hits = a} :: SearchResponse)

-- | The status information returned for the search request.
searchResponse_status :: Lens.Lens' SearchResponse (Prelude.Maybe SearchStatus)
searchResponse_status = Lens.lens (\SearchResponse' {status} -> status) (\s@SearchResponse' {} a -> s {status = a} :: SearchResponse)

-- | The response's http status code.
searchResponse_httpStatus :: Lens.Lens' SearchResponse Prelude.Int
searchResponse_httpStatus = Lens.lens (\SearchResponse' {httpStatus} -> httpStatus) (\s@SearchResponse' {} a -> s {httpStatus = a} :: SearchResponse)

instance Prelude.NFData SearchResponse where
  rnf SearchResponse' {..} =
    Prelude.rnf facets
      `Prelude.seq` Prelude.rnf stats
      `Prelude.seq` Prelude.rnf hits
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
