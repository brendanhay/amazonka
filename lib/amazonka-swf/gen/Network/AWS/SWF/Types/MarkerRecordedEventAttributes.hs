{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.MarkerRecordedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.MarkerRecordedEventAttributes
  ( MarkerRecordedEventAttributes (..)
  -- * Smart constructor
  , mkMarkerRecordedEventAttributes
  -- * Lenses
  , mreaMarkerName
  , mreaDecisionTaskCompletedEventId
  , mreaDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Details as Types
import qualified Network.AWS.SWF.Types.MarkerName as Types

-- | Provides the details of the @MarkerRecorded@ event.
--
-- /See:/ 'mkMarkerRecordedEventAttributes' smart constructor.
data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes'
  { markerName :: Types.MarkerName
    -- ^ The name of the marker.
  , decisionTaskCompletedEventId :: Core.Integer
    -- ^ The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RecordMarker@ decision that requested this marker. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , details :: Core.Maybe Types.Details
    -- ^ The details of the marker.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MarkerRecordedEventAttributes' value with any optional fields omitted.
mkMarkerRecordedEventAttributes
    :: Types.MarkerName -- ^ 'markerName'
    -> Core.Integer -- ^ 'decisionTaskCompletedEventId'
    -> MarkerRecordedEventAttributes
mkMarkerRecordedEventAttributes markerName
  decisionTaskCompletedEventId
  = MarkerRecordedEventAttributes'{markerName,
                                   decisionTaskCompletedEventId, details = Core.Nothing}

-- | The name of the marker.
--
-- /Note:/ Consider using 'markerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mreaMarkerName :: Lens.Lens' MarkerRecordedEventAttributes Types.MarkerName
mreaMarkerName = Lens.field @"markerName"
{-# INLINEABLE mreaMarkerName #-}
{-# DEPRECATED markerName "Use generic-lens or generic-optics with 'markerName' instead"  #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RecordMarker@ decision that requested this marker. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mreaDecisionTaskCompletedEventId :: Lens.Lens' MarkerRecordedEventAttributes Core.Integer
mreaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# INLINEABLE mreaDecisionTaskCompletedEventId #-}
{-# DEPRECATED decisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead"  #-}

-- | The details of the marker.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mreaDetails :: Lens.Lens' MarkerRecordedEventAttributes (Core.Maybe Types.Details)
mreaDetails = Lens.field @"details"
{-# INLINEABLE mreaDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

instance Core.FromJSON MarkerRecordedEventAttributes where
        parseJSON
          = Core.withObject "MarkerRecordedEventAttributes" Core.$
              \ x ->
                MarkerRecordedEventAttributes' Core.<$>
                  (x Core..: "markerName") Core.<*>
                    x Core..: "decisionTaskCompletedEventId"
                    Core.<*> x Core..:? "details"
