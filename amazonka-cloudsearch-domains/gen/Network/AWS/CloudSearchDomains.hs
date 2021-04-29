{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You use the AmazonCloudSearch2013 API to upload documents to a search
-- domain and search those documents.
--
-- The endpoints for submitting @UploadDocuments@, @Search@, and @Suggest@
-- requests are domain-specific. To get the endpoints for your domain, use
-- the Amazon CloudSearch configuration service @DescribeDomains@ action.
-- The domain endpoints are also displayed on the domain dashboard in the
-- Amazon CloudSearch console. You submit suggest requests to the search
-- endpoint.
--
-- For more information, see the
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide Amazon CloudSearch Developer Guide>.
module Network.AWS.CloudSearchDomains
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** SearchException
    _SearchException,

    -- ** DocumentServiceException
    _DocumentServiceException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UploadDocuments
    UploadDocuments (UploadDocuments'),
    newUploadDocuments,
    UploadDocumentsResponse (UploadDocumentsResponse'),
    newUploadDocumentsResponse,

    -- ** Search
    Search (Search'),
    newSearch,
    SearchResponse (SearchResponse'),
    newSearchResponse,

    -- ** Suggest
    Suggest (Suggest'),
    newSuggest,
    SuggestResponse (SuggestResponse'),
    newSuggestResponse,

    -- * Types

    -- ** ContentType
    ContentType (..),

    -- ** QueryParser
    QueryParser (..),

    -- ** Bucket
    Bucket (Bucket'),
    newBucket,

    -- ** BucketInfo
    BucketInfo (BucketInfo'),
    newBucketInfo,

    -- ** DocumentServiceWarning
    DocumentServiceWarning (DocumentServiceWarning'),
    newDocumentServiceWarning,

    -- ** FieldStats
    FieldStats (FieldStats'),
    newFieldStats,

    -- ** Hit
    Hit (Hit'),
    newHit,

    -- ** Hits
    Hits (Hits'),
    newHits,

    -- ** SearchStatus
    SearchStatus (SearchStatus'),
    newSearchStatus,

    -- ** SuggestModel
    SuggestModel (SuggestModel'),
    newSuggestModel,

    -- ** SuggestStatus
    SuggestStatus (SuggestStatus'),
    newSuggestStatus,

    -- ** SuggestionMatch
    SuggestionMatch (SuggestionMatch'),
    newSuggestionMatch,
  )
where

import Network.AWS.CloudSearchDomains.Lens
import Network.AWS.CloudSearchDomains.Search
import Network.AWS.CloudSearchDomains.Suggest
import Network.AWS.CloudSearchDomains.Types
import Network.AWS.CloudSearchDomains.UploadDocuments
import Network.AWS.CloudSearchDomains.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudSearchDomains'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
