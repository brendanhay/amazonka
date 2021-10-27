{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ConnectContactLens.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ConnectContactLens.Lens
  ( -- * Operations

    -- ** ListRealtimeContactAnalysisSegments
    listRealtimeContactAnalysisSegments_nextToken,
    listRealtimeContactAnalysisSegments_maxResults,
    listRealtimeContactAnalysisSegments_instanceId,
    listRealtimeContactAnalysisSegments_contactId,
    listRealtimeContactAnalysisSegmentsResponse_nextToken,
    listRealtimeContactAnalysisSegmentsResponse_httpStatus,
    listRealtimeContactAnalysisSegmentsResponse_segments,

    -- * Types

    -- ** Categories
    categories_matchedCategories,
    categories_matchedDetails,

    -- ** CategoryDetails
    categoryDetails_pointsOfInterest,

    -- ** CharacterOffsets
    characterOffsets_beginOffsetChar,
    characterOffsets_endOffsetChar,

    -- ** IssueDetected
    issueDetected_characterOffsets,

    -- ** PointOfInterest
    pointOfInterest_beginOffsetMillis,
    pointOfInterest_endOffsetMillis,

    -- ** RealtimeContactAnalysisSegment
    realtimeContactAnalysisSegment_categories,
    realtimeContactAnalysisSegment_transcript,

    -- ** Transcript
    transcript_issuesDetected,
    transcript_id,
    transcript_participantId,
    transcript_participantRole,
    transcript_content,
    transcript_beginOffsetMillis,
    transcript_endOffsetMillis,
    transcript_sentiment,
  )
where

import Network.AWS.ConnectContactLens.ListRealtimeContactAnalysisSegments
import Network.AWS.ConnectContactLens.Types.Categories
import Network.AWS.ConnectContactLens.Types.CategoryDetails
import Network.AWS.ConnectContactLens.Types.CharacterOffsets
import Network.AWS.ConnectContactLens.Types.IssueDetected
import Network.AWS.ConnectContactLens.Types.PointOfInterest
import Network.AWS.ConnectContactLens.Types.RealtimeContactAnalysisSegment
import Network.AWS.ConnectContactLens.Types.Transcript
