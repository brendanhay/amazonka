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
    uploadDocumentsResponse_status,
    uploadDocumentsResponse_adds,
    uploadDocumentsResponse_warnings,
    uploadDocumentsResponse_deletes,
    uploadDocumentsResponse_httpStatus,

    -- ** Search
    search_expr,
    search_cursor,
    search_return,
    search_queryOptions,
    search_filterQuery,
    search_size,
    search_queryParser,
    search_start,
    search_highlight,
    search_stats,
    search_sort,
    search_facet,
    search_partial,
    search_query,
    searchResponse_status,
    searchResponse_facets,
    searchResponse_stats,
    searchResponse_hits,
    searchResponse_httpStatus,

    -- * Types

    -- ** Bucket
    bucket_value,
    bucket_count,

    -- ** BucketInfo
    bucketInfo_buckets,

    -- ** DocumentServiceWarning
    documentServiceWarning_message,

    -- ** FieldStats
    fieldStats_max,
    fieldStats_mean,
    fieldStats_count,
    fieldStats_missing,
    fieldStats_stddev,
    fieldStats_min,
    fieldStats_sumOfSquares,
    fieldStats_sum,

    -- ** Hit
    hit_exprs,
    hit_id,
    hit_highlights,
    hit_fields,

    -- ** Hits
    hits_cursor,
    hits_hit,
    hits_start,
    hits_found,

    -- ** SearchStatus
    searchStatus_rid,
    searchStatus_timems,

    -- ** SuggestModel
    suggestModel_found,
    suggestModel_suggestions,
    suggestModel_query,

    -- ** SuggestStatus
    suggestStatus_rid,
    suggestStatus_timems,

    -- ** SuggestionMatch
    suggestionMatch_suggestion,
    suggestionMatch_score,
    suggestionMatch_id,
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
