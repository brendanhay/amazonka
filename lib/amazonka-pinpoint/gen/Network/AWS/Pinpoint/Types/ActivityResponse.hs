{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ActivityResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ActivityResponse
  ( ActivityResponse (..)
  -- * Smart constructor
  , mkActivityResponse
  -- * Lenses
  , aCampaignId
  , aId
  , aApplicationId
  , aEnd
  , aResult
  , aScheduledStart
  , aStart
  , aState
  , aSuccessfulEndpointCount
  , aTimezonesCompletedCount
  , aTimezonesTotalCount
  , aTotalEndpointCount
  , aTreatmentId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about an activity that was performed by a campaign.
--
-- /See:/ 'mkActivityResponse' smart constructor.
data ActivityResponse = ActivityResponse'
  { campaignId :: Core.Text
    -- ^ The unique identifier for the campaign that the activity applies to.
  , id :: Core.Text
    -- ^ The unique identifier for the activity.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application that the campaign applies to.
  , end :: Core.Maybe Core.Text
    -- ^ The actual time, in ISO 8601 format, when the activity was marked CANCELLED or COMPLETED.
  , result :: Core.Maybe Core.Text
    -- ^ Specifies whether the activity succeeded. Possible values are SUCCESS and FAIL.
  , scheduledStart :: Core.Maybe Core.Text
    -- ^ The scheduled start time, in ISO 8601 format, for the activity.
  , start :: Core.Maybe Core.Text
    -- ^ The actual start time, in ISO 8601 format, of the activity.
  , state :: Core.Maybe Core.Text
    -- ^ The current status of the activity. Possible values are: PENDING, INITIALIZING, RUNNING, PAUSED, CANCELLED, and COMPLETED.
  , successfulEndpointCount :: Core.Maybe Core.Int
    -- ^ The total number of endpoints that the campaign successfully delivered messages to.
  , timezonesCompletedCount :: Core.Maybe Core.Int
    -- ^ The total number of time zones that were completed.
  , timezonesTotalCount :: Core.Maybe Core.Int
    -- ^ The total number of unique time zones that are in the segment for the campaign.
  , totalEndpointCount :: Core.Maybe Core.Int
    -- ^ The total number of endpoints that the campaign attempted to deliver messages to.
  , treatmentId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the campaign treatment that the activity applies to. A treatment is a variation of a campaign that's used for A/B testing of a campaign.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityResponse' value with any optional fields omitted.
mkActivityResponse
    :: Core.Text -- ^ 'campaignId'
    -> Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'applicationId'
    -> ActivityResponse
mkActivityResponse campaignId id applicationId
  = ActivityResponse'{campaignId, id, applicationId,
                      end = Core.Nothing, result = Core.Nothing,
                      scheduledStart = Core.Nothing, start = Core.Nothing,
                      state = Core.Nothing, successfulEndpointCount = Core.Nothing,
                      timezonesCompletedCount = Core.Nothing,
                      timezonesTotalCount = Core.Nothing,
                      totalEndpointCount = Core.Nothing, treatmentId = Core.Nothing}

-- | The unique identifier for the campaign that the activity applies to.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCampaignId :: Lens.Lens' ActivityResponse Core.Text
aCampaignId = Lens.field @"campaignId"
{-# INLINEABLE aCampaignId #-}
{-# DEPRECATED campaignId "Use generic-lens or generic-optics with 'campaignId' instead"  #-}

-- | The unique identifier for the activity.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aId :: Lens.Lens' ActivityResponse Core.Text
aId = Lens.field @"id"
{-# INLINEABLE aId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The unique identifier for the application that the campaign applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApplicationId :: Lens.Lens' ActivityResponse Core.Text
aApplicationId = Lens.field @"applicationId"
{-# INLINEABLE aApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The actual time, in ISO 8601 format, when the activity was marked CANCELLED or COMPLETED.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnd :: Lens.Lens' ActivityResponse (Core.Maybe Core.Text)
aEnd = Lens.field @"end"
{-# INLINEABLE aEnd #-}
{-# DEPRECATED end "Use generic-lens or generic-optics with 'end' instead"  #-}

-- | Specifies whether the activity succeeded. Possible values are SUCCESS and FAIL.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResult :: Lens.Lens' ActivityResponse (Core.Maybe Core.Text)
aResult = Lens.field @"result"
{-# INLINEABLE aResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

-- | The scheduled start time, in ISO 8601 format, for the activity.
--
-- /Note:/ Consider using 'scheduledStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aScheduledStart :: Lens.Lens' ActivityResponse (Core.Maybe Core.Text)
aScheduledStart = Lens.field @"scheduledStart"
{-# INLINEABLE aScheduledStart #-}
{-# DEPRECATED scheduledStart "Use generic-lens or generic-optics with 'scheduledStart' instead"  #-}

-- | The actual start time, in ISO 8601 format, of the activity.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStart :: Lens.Lens' ActivityResponse (Core.Maybe Core.Text)
aStart = Lens.field @"start"
{-# INLINEABLE aStart #-}
{-# DEPRECATED start "Use generic-lens or generic-optics with 'start' instead"  #-}

-- | The current status of the activity. Possible values are: PENDING, INITIALIZING, RUNNING, PAUSED, CANCELLED, and COMPLETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aState :: Lens.Lens' ActivityResponse (Core.Maybe Core.Text)
aState = Lens.field @"state"
{-# INLINEABLE aState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The total number of endpoints that the campaign successfully delivered messages to.
--
-- /Note:/ Consider using 'successfulEndpointCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSuccessfulEndpointCount :: Lens.Lens' ActivityResponse (Core.Maybe Core.Int)
aSuccessfulEndpointCount = Lens.field @"successfulEndpointCount"
{-# INLINEABLE aSuccessfulEndpointCount #-}
{-# DEPRECATED successfulEndpointCount "Use generic-lens or generic-optics with 'successfulEndpointCount' instead"  #-}

-- | The total number of time zones that were completed.
--
-- /Note:/ Consider using 'timezonesCompletedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimezonesCompletedCount :: Lens.Lens' ActivityResponse (Core.Maybe Core.Int)
aTimezonesCompletedCount = Lens.field @"timezonesCompletedCount"
{-# INLINEABLE aTimezonesCompletedCount #-}
{-# DEPRECATED timezonesCompletedCount "Use generic-lens or generic-optics with 'timezonesCompletedCount' instead"  #-}

-- | The total number of unique time zones that are in the segment for the campaign.
--
-- /Note:/ Consider using 'timezonesTotalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimezonesTotalCount :: Lens.Lens' ActivityResponse (Core.Maybe Core.Int)
aTimezonesTotalCount = Lens.field @"timezonesTotalCount"
{-# INLINEABLE aTimezonesTotalCount #-}
{-# DEPRECATED timezonesTotalCount "Use generic-lens or generic-optics with 'timezonesTotalCount' instead"  #-}

-- | The total number of endpoints that the campaign attempted to deliver messages to.
--
-- /Note:/ Consider using 'totalEndpointCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTotalEndpointCount :: Lens.Lens' ActivityResponse (Core.Maybe Core.Int)
aTotalEndpointCount = Lens.field @"totalEndpointCount"
{-# INLINEABLE aTotalEndpointCount #-}
{-# DEPRECATED totalEndpointCount "Use generic-lens or generic-optics with 'totalEndpointCount' instead"  #-}

-- | The unique identifier for the campaign treatment that the activity applies to. A treatment is a variation of a campaign that's used for A/B testing of a campaign.
--
-- /Note:/ Consider using 'treatmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTreatmentId :: Lens.Lens' ActivityResponse (Core.Maybe Core.Text)
aTreatmentId = Lens.field @"treatmentId"
{-# INLINEABLE aTreatmentId #-}
{-# DEPRECATED treatmentId "Use generic-lens or generic-optics with 'treatmentId' instead"  #-}

instance Core.FromJSON ActivityResponse where
        parseJSON
          = Core.withObject "ActivityResponse" Core.$
              \ x ->
                ActivityResponse' Core.<$>
                  (x Core..: "CampaignId") Core.<*> x Core..: "Id" Core.<*>
                    x Core..: "ApplicationId"
                    Core.<*> x Core..:? "End"
                    Core.<*> x Core..:? "Result"
                    Core.<*> x Core..:? "ScheduledStart"
                    Core.<*> x Core..:? "Start"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "SuccessfulEndpointCount"
                    Core.<*> x Core..:? "TimezonesCompletedCount"
                    Core.<*> x Core..:? "TimezonesTotalCount"
                    Core.<*> x Core..:? "TotalEndpointCount"
                    Core.<*> x Core..:? "TreatmentId"
