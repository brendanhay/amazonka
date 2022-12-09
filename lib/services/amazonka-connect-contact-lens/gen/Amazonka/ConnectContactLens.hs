{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ConnectContactLens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-21@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Contact Lens for Amazon Connect enables you to analyze conversations
-- between customer and agents, by using speech transcription, natural
-- language processing, and intelligent search capabilities. It performs
-- sentiment analysis, detects issues, and enables you to automatically
-- categorize contacts.
--
-- Contact Lens for Amazon Connect provides both real-time and post-call
-- analytics of customer-agent conversations. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/analyze-conversations.html Analyze conversations using Contact Lens>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.ConnectContactLens
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListRealtimeContactAnalysisSegments
    ListRealtimeContactAnalysisSegments (ListRealtimeContactAnalysisSegments'),
    newListRealtimeContactAnalysisSegments,
    ListRealtimeContactAnalysisSegmentsResponse (ListRealtimeContactAnalysisSegmentsResponse'),
    newListRealtimeContactAnalysisSegmentsResponse,

    -- * Types

    -- ** SentimentValue
    SentimentValue (..),

    -- ** Categories
    Categories (Categories'),
    newCategories,

    -- ** CategoryDetails
    CategoryDetails (CategoryDetails'),
    newCategoryDetails,

    -- ** CharacterOffsets
    CharacterOffsets (CharacterOffsets'),
    newCharacterOffsets,

    -- ** IssueDetected
    IssueDetected (IssueDetected'),
    newIssueDetected,

    -- ** PointOfInterest
    PointOfInterest (PointOfInterest'),
    newPointOfInterest,

    -- ** RealtimeContactAnalysisSegment
    RealtimeContactAnalysisSegment (RealtimeContactAnalysisSegment'),
    newRealtimeContactAnalysisSegment,

    -- ** Transcript
    Transcript (Transcript'),
    newTranscript,
  )
where

import Amazonka.ConnectContactLens.Lens
import Amazonka.ConnectContactLens.ListRealtimeContactAnalysisSegments
import Amazonka.ConnectContactLens.Types
import Amazonka.ConnectContactLens.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ConnectContactLens'.

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
