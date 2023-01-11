{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectContactLens.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectContactLens.Lens
  ( -- * Operations

    -- ** ListRealtimeContactAnalysisSegments
    listRealtimeContactAnalysisSegments_maxResults,
    listRealtimeContactAnalysisSegments_nextToken,
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

import Amazonka.ConnectContactLens.ListRealtimeContactAnalysisSegments
import Amazonka.ConnectContactLens.Types.Categories
import Amazonka.ConnectContactLens.Types.CategoryDetails
import Amazonka.ConnectContactLens.Types.CharacterOffsets
import Amazonka.ConnectContactLens.Types.IssueDetected
import Amazonka.ConnectContactLens.Types.PointOfInterest
import Amazonka.ConnectContactLens.Types.RealtimeContactAnalysisSegment
import Amazonka.ConnectContactLens.Types.Transcript
