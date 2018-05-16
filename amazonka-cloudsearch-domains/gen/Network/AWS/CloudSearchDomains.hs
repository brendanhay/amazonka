{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You use the AmazonCloudSearch2013 API to upload documents to a search domain and search those documents.
--
--
-- The endpoints for submitting @UploadDocuments@ , @Search@ , and @Suggest@ requests are domain-specific. To get the endpoints for your domain, use the Amazon CloudSearch configuration service @DescribeDomains@ action. The domain endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. You submit suggest requests to the search endpoint.
--
-- For more information, see the <http://docs.aws.amazon.com/cloudsearch/latest/developerguide Amazon CloudSearch Developer Guide> .
--
module Network.AWS.CloudSearchDomains
    (
    -- * Service Configuration
      cloudSearchDomains

    -- * Errors
    -- $errors

    -- ** DocumentServiceException
    , _DocumentServiceException

    -- ** SearchException
    , _SearchException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** Suggest
    , module Network.AWS.CloudSearchDomains.Suggest

    -- ** UploadDocuments
    , module Network.AWS.CloudSearchDomains.UploadDocuments

    -- ** Search
    , module Network.AWS.CloudSearchDomains.Search

    -- * Types

    -- ** ContentType
    , ContentType (..)

    -- ** QueryParser
    , QueryParser (..)

    -- ** Bucket
    , Bucket
    , bucket
    , bValue
    , bCount

    -- ** BucketInfo
    , BucketInfo
    , bucketInfo
    , biBuckets

    -- ** DocumentServiceWarning
    , DocumentServiceWarning
    , documentServiceWarning
    , dswMessage

    -- ** FieldStats
    , FieldStats
    , fieldStats
    , fsMax
    , fsMean
    , fsCount
    , fsMissing
    , fsStddev
    , fsMin
    , fsSumOfSquares
    , fsSum

    -- ** Hit
    , Hit
    , hit
    , hitExprs
    , hitId
    , hitHighlights
    , hitFields

    -- ** Hits
    , Hits
    , hits
    , hCursor
    , hHit
    , hStart
    , hFound

    -- ** SearchStatus
    , SearchStatus
    , searchStatus
    , sRid
    , sTimems

    -- ** SuggestModel
    , SuggestModel
    , suggestModel
    , smFound
    , smSuggestions
    , smQuery

    -- ** SuggestStatus
    , SuggestStatus
    , suggestStatus
    , ssRid
    , ssTimems

    -- ** SuggestionMatch
    , SuggestionMatch
    , suggestionMatch
    , smSuggestion
    , smScore
    , smId
    ) where

import Network.AWS.CloudSearchDomains.Search
import Network.AWS.CloudSearchDomains.Suggest
import Network.AWS.CloudSearchDomains.Types
import Network.AWS.CloudSearchDomains.UploadDocuments
import Network.AWS.CloudSearchDomains.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudSearchDomains'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
