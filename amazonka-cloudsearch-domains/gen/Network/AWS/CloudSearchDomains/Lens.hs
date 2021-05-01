{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Lens
  ( -- * Operations

    -- ** UploadDocuments
    uploadDocuments_contentType,
    uploadDocuments_documents,
    uploadDocumentsResponse_status,
    uploadDocumentsResponse_warnings,
    uploadDocumentsResponse_deletes,
    uploadDocumentsResponse_adds,
    uploadDocumentsResponse_httpStatus,

    -- ** Search
    search_expr,
    search_partial,
    search_queryParser,
    search_queryOptions,
    search_cursor,
    search_return,
    search_stats,
    search_highlight,
    search_start,
    search_facet,
    search_size,
    search_sort,
    search_filterQuery,
    search_query,
    searchResponse_status,
    searchResponse_hits,
    searchResponse_stats,
    searchResponse_facets,
    searchResponse_httpStatus,

    -- ** Suggest
    suggest_size,
    suggest_query,
    suggest_suggester,
    suggestResponse_status,
    suggestResponse_suggest,
    suggestResponse_httpStatus,

    -- * Types

    -- ** Bucket
    bucket_value,
    bucket_count,

    -- ** BucketInfo
    bucketInfo_buckets,

    -- ** DocumentServiceWarning
    documentServiceWarning_message,

    -- ** FieldStats
    fieldStats_mean,
    fieldStats_missing,
    fieldStats_sum,
    fieldStats_min,
    fieldStats_max,
    fieldStats_stddev,
    fieldStats_count,
    fieldStats_sumOfSquares,

    -- ** Hit
    hit_id,
    hit_exprs,
    hit_fields,
    hit_highlights,

    -- ** Hits
    hits_found,
    hits_hit,
    hits_cursor,
    hits_start,

    -- ** SearchStatus
    searchStatus_timems,
    searchStatus_rid,

    -- ** SuggestModel
    suggestModel_suggestions,
    suggestModel_found,
    suggestModel_query,

    -- ** SuggestStatus
    suggestStatus_timems,
    suggestStatus_rid,

    -- ** SuggestionMatch
    suggestionMatch_suggestion,
    suggestionMatch_id,
    suggestionMatch_score,
  )
where

import Network.AWS.CloudSearchDomains.Search
import Network.AWS.CloudSearchDomains.Suggest
import Network.AWS.CloudSearchDomains.Types.Bucket
import Network.AWS.CloudSearchDomains.Types.BucketInfo
import Network.AWS.CloudSearchDomains.Types.DocumentServiceWarning
import Network.AWS.CloudSearchDomains.Types.FieldStats
import Network.AWS.CloudSearchDomains.Types.Hit
import Network.AWS.CloudSearchDomains.Types.Hits
import Network.AWS.CloudSearchDomains.Types.SearchStatus
import Network.AWS.CloudSearchDomains.Types.SuggestModel
import Network.AWS.CloudSearchDomains.Types.SuggestStatus
import Network.AWS.CloudSearchDomains.Types.SuggestionMatch
import Network.AWS.CloudSearchDomains.UploadDocuments
