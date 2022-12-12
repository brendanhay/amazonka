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
    search_cursor,
    search_expr,
    search_facet,
    search_filterQuery,
    search_highlight,
    search_partial,
    search_queryOptions,
    search_queryParser,
    search_return,
    search_size,
    search_sort,
    search_start,
    search_stats,
    search_query,
    searchResponse_facets,
    searchResponse_hits,
    searchResponse_stats,
    searchResponse_status,
    searchResponse_httpStatus,

    -- ** Suggest
    suggest_size,
    suggest_query,
    suggest_suggester,
    suggestResponse_status,
    suggestResponse_suggest,
    suggestResponse_httpStatus,

    -- ** UploadDocuments
    uploadDocuments_contentType,
    uploadDocuments_documents,
    uploadDocumentsResponse_adds,
    uploadDocumentsResponse_deletes,
    uploadDocumentsResponse_status,
    uploadDocumentsResponse_warnings,
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
    fieldStats_count,
    fieldStats_max,
    fieldStats_mean,
    fieldStats_min,
    fieldStats_missing,
    fieldStats_stddev,
    fieldStats_sum,
    fieldStats_sumOfSquares,

    -- ** Hit
    hit_exprs,
    hit_fields,
    hit_highlights,
    hit_id,

    -- ** Hits
    hits_cursor,
    hits_found,
    hits_hit,
    hits_start,

    -- ** SearchStatus
    searchStatus_rid,
    searchStatus_timems,

    -- ** SuggestModel
    suggestModel_found,
    suggestModel_query,
    suggestModel_suggestions,

    -- ** SuggestStatus
    suggestStatus_rid,
    suggestStatus_timems,

    -- ** SuggestionMatch
    suggestionMatch_id,
    suggestionMatch_score,
    suggestionMatch_suggestion,
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
