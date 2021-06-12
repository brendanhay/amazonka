{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.MarkerRecordedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.MarkerRecordedEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @MarkerRecorded@ event.
--
-- /See:/ 'newMarkerRecordedEventAttributes' smart constructor.
data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes'
  { -- | The details of the marker.
    details :: Core.Maybe Core.Text,
    -- | The name of the marker.
    markerName :: Core.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @RecordMarker@ decision that
    -- requested this marker. This information can be useful for diagnosing
    -- problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MarkerRecordedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'markerRecordedEventAttributes_details' - The details of the marker.
--
-- 'markerName', 'markerRecordedEventAttributes_markerName' - The name of the marker.
--
-- 'decisionTaskCompletedEventId', 'markerRecordedEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @RecordMarker@ decision that
-- requested this marker. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
newMarkerRecordedEventAttributes ::
  -- | 'markerName'
  Core.Text ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  MarkerRecordedEventAttributes
newMarkerRecordedEventAttributes
  pMarkerName_
  pDecisionTaskCompletedEventId_ =
    MarkerRecordedEventAttributes'
      { details =
          Core.Nothing,
        markerName = pMarkerName_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The details of the marker.
markerRecordedEventAttributes_details :: Lens.Lens' MarkerRecordedEventAttributes (Core.Maybe Core.Text)
markerRecordedEventAttributes_details = Lens.lens (\MarkerRecordedEventAttributes' {details} -> details) (\s@MarkerRecordedEventAttributes' {} a -> s {details = a} :: MarkerRecordedEventAttributes)

-- | The name of the marker.
markerRecordedEventAttributes_markerName :: Lens.Lens' MarkerRecordedEventAttributes Core.Text
markerRecordedEventAttributes_markerName = Lens.lens (\MarkerRecordedEventAttributes' {markerName} -> markerName) (\s@MarkerRecordedEventAttributes' {} a -> s {markerName = a} :: MarkerRecordedEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @RecordMarker@ decision that
-- requested this marker. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
markerRecordedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' MarkerRecordedEventAttributes Core.Integer
markerRecordedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\MarkerRecordedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@MarkerRecordedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: MarkerRecordedEventAttributes)

instance Core.FromJSON MarkerRecordedEventAttributes where
  parseJSON =
    Core.withObject
      "MarkerRecordedEventAttributes"
      ( \x ->
          MarkerRecordedEventAttributes'
            Core.<$> (x Core..:? "details")
            Core.<*> (x Core..: "markerName")
            Core.<*> (x Core..: "decisionTaskCompletedEventId")
      )

instance Core.Hashable MarkerRecordedEventAttributes

instance Core.NFData MarkerRecordedEventAttributes
