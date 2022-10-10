{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudSearchDomains.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Lens
  ( -- * Operations

    -- ** Search
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
    searchResponse_facets,
    searchResponse_stats,
    searchResponse_hits,
    searchResponse_status,
    searchResponse_httpStatus,

    -- ** Suggest
    suggest_size,
    suggest_query,
    suggest_suggester,
    suggestResponse_suggest,
    suggestResponse_status,
    suggestResponse_httpStatus,

    -- ** UploadDocuments
    uploadDocuments_contentType,
    uploadDocuments_documents,
    uploadDocumentsResponse_adds,
    uploadDocumentsResponse_status,
    uploadDocumentsResponse_warnings,
    uploadDocumentsResponse_deletes,
    uploadDocumentsResponse_httpStatus,

    -- * Types

    -- ** Bucket
    bucket_count,
    bucket_value,

    -- ** BucketInfo
    bucketInfo_buckets,

    -- ** DocumentServiceWarning
    documentServiceWarning_message,

    -- ** FieldStats
    fieldStats_missing,
    fieldStats_max,
    fieldStats_sumOfSquares,
    fieldStats_count,
    fieldStats_min,
    fieldStats_stddev,
    fieldStats_sum,
    fieldStats_mean,

    -- ** Hit
    hit_exprs,
    hit_highlights,
    hit_fields,
    hit_id,

    -- ** Hits
    hits_start,
    hits_hit,
    hits_cursor,
    hits_found,

    -- ** SearchStatus
    searchStatus_timems,
    searchStatus_rid,

    -- ** SuggestModel
    suggestModel_found,
    suggestModel_query,
    suggestModel_suggestions,

    -- ** SuggestStatus
    suggestStatus_timems,
    suggestStatus_rid,

    -- ** SuggestionMatch
    suggestionMatch_suggestion,
    suggestionMatch_score,
    suggestionMatch_id,
  )
where

import Amazonka.CloudSearchDomains.Search
import Amazonka.CloudSearchDomains.Suggest
import Amazonka.CloudSearchDomains.Types.Bucket
import Amazonka.CloudSearchDomains.Types.BucketInfo
import Amazonka.CloudSearchDomains.Types.DocumentServiceWarning
import Amazonka.CloudSearchDomains.Types.FieldStats
import Amazonka.CloudSearchDomains.Types.Hit
import Amazonka.CloudSearchDomains.Types.Hits
import Amazonka.CloudSearchDomains.Types.SearchStatus
import Amazonka.CloudSearchDomains.Types.SuggestModel
import Amazonka.CloudSearchDomains.Types.SuggestStatus
import Amazonka.CloudSearchDomains.Types.SuggestionMatch
import Amazonka.CloudSearchDomains.UploadDocuments
