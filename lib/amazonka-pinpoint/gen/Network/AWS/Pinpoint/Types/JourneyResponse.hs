{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JourneyResponse
  ( JourneyResponse (..)
  -- * Smart constructor
  , mkJourneyResponse
  -- * Lenses
  , jrName
  , jrId
  , jrApplicationId
  , jrActivities
  , jrCreationDate
  , jrLastModifiedDate
  , jrLimits
  , jrLocalTime
  , jrQuietTime
  , jrRefreshFrequency
  , jrSchedule
  , jrStartActivity
  , jrStartCondition
  , jrState
  , jrTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Activity as Types
import qualified Network.AWS.Pinpoint.Types.JourneyLimits as Types
import qualified Network.AWS.Pinpoint.Types.JourneySchedule as Types
import qualified Network.AWS.Pinpoint.Types.QuietTime as Types
import qualified Network.AWS.Pinpoint.Types.StartCondition as Types
import qualified Network.AWS.Pinpoint.Types.State as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status, configuration, and other settings for a journey.
--
-- /See:/ 'mkJourneyResponse' smart constructor.
data JourneyResponse = JourneyResponse'
  { name :: Core.Text
    -- ^ The name of the journey.
  , id :: Core.Text
    -- ^ The unique identifier for the journey.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application that the journey applies to.
  , activities :: Core.Maybe (Core.HashMap Core.Text Types.Activity)
    -- ^ A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date, in ISO 8601 format, when the journey was created.
  , lastModifiedDate :: Core.Maybe Core.Text
    -- ^ The date, in ISO 8601 format, when the journey was last modified.
  , limits :: Core.Maybe Types.JourneyLimits
    -- ^ The messaging and entry limits for the journey.
  , localTime :: Core.Maybe Core.Bool
    -- ^ Specifies whether the journey's scheduled start and end times use each participant's local time. If this value is true, the schedule uses each participant's local time.
  , quietTime :: Core.Maybe Types.QuietTime
    -- ^ The quiet time settings for the journey. Quiet time is a specific time range when a journey doesn't send messages to participants, if all the following conditions are met:
--
--
--     * The EndpointDemographic.Timezone property of the endpoint for the participant is set to a valid value.
--
--
--     * The current time in the participant's time zone is later than or equal to the time specified by the QuietTime.Start property for the journey.
--
--
--     * The current time in the participant's time zone is earlier than or equal to the time specified by the QuietTime.End property for the journey.
--
--
-- If any of the preceding conditions isn't met, the participant will receive messages from the journey, even if quiet time is enabled.
  , refreshFrequency :: Core.Maybe Core.Text
    -- ^ The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
  , schedule :: Core.Maybe Types.JourneySchedule
    -- ^ The schedule settings for the journey.
  , startActivity :: Core.Maybe Core.Text
    -- ^ The unique identifier for the first activity in the journey.
  , startCondition :: Core.Maybe Types.StartCondition
    -- ^ The segment that defines which users are participants in the journey.
  , state :: Core.Maybe Types.State
    -- ^ The current status of the journey. Possible values are:
--
--
--     * DRAFT - The journey is being developed and hasn't been published yet.
--
--
--     * ACTIVE - The journey has been developed and published. Depending on the journey's schedule, the journey may currently be running or scheduled to start running at a later time. If a journey's status is ACTIVE, you can't add, change, or remove activities from it.
--
--
--     * COMPLETED - The journey has been published and has finished running. All participants have entered the journey and no participants are waiting to complete the journey or any activities in the journey.
--
--
--     * CANCELLED - The journey has been stopped. If a journey's status is CANCELLED, you can't add, change, or remove activities or segment settings from the journey.
--
--
--     * CLOSED - The journey has been published and has started running. It may have also passed its scheduled end time, or passed its scheduled start time and a refresh frequency hasn't been specified for it. If a journey's status is CLOSED, you can't add participants to it, and no existing participants can enter the journey for the first time. However, any existing participants who are currently waiting to start an activity may continue the journey.
--
--
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ This object is not used or supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'JourneyResponse' value with any optional fields omitted.
mkJourneyResponse
    :: Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'applicationId'
    -> JourneyResponse
mkJourneyResponse name id applicationId
  = JourneyResponse'{name, id, applicationId,
                     activities = Core.Nothing, creationDate = Core.Nothing,
                     lastModifiedDate = Core.Nothing, limits = Core.Nothing,
                     localTime = Core.Nothing, quietTime = Core.Nothing,
                     refreshFrequency = Core.Nothing, schedule = Core.Nothing,
                     startActivity = Core.Nothing, startCondition = Core.Nothing,
                     state = Core.Nothing, tags = Core.Nothing}

-- | The name of the journey.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrName :: Lens.Lens' JourneyResponse Core.Text
jrName = Lens.field @"name"
{-# INLINEABLE jrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrId :: Lens.Lens' JourneyResponse Core.Text
jrId = Lens.field @"id"
{-# INLINEABLE jrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The unique identifier for the application that the journey applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrApplicationId :: Lens.Lens' JourneyResponse Core.Text
jrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE jrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrActivities :: Lens.Lens' JourneyResponse (Core.Maybe (Core.HashMap Core.Text Types.Activity))
jrActivities = Lens.field @"activities"
{-# INLINEABLE jrActivities #-}
{-# DEPRECATED activities "Use generic-lens or generic-optics with 'activities' instead"  #-}

-- | The date, in ISO 8601 format, when the journey was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrCreationDate :: Lens.Lens' JourneyResponse (Core.Maybe Core.Text)
jrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE jrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The date, in ISO 8601 format, when the journey was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrLastModifiedDate :: Lens.Lens' JourneyResponse (Core.Maybe Core.Text)
jrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE jrLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The messaging and entry limits for the journey.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrLimits :: Lens.Lens' JourneyResponse (Core.Maybe Types.JourneyLimits)
jrLimits = Lens.field @"limits"
{-# INLINEABLE jrLimits #-}
{-# DEPRECATED limits "Use generic-lens or generic-optics with 'limits' instead"  #-}

-- | Specifies whether the journey's scheduled start and end times use each participant's local time. If this value is true, the schedule uses each participant's local time.
--
-- /Note:/ Consider using 'localTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrLocalTime :: Lens.Lens' JourneyResponse (Core.Maybe Core.Bool)
jrLocalTime = Lens.field @"localTime"
{-# INLINEABLE jrLocalTime #-}
{-# DEPRECATED localTime "Use generic-lens or generic-optics with 'localTime' instead"  #-}

-- | The quiet time settings for the journey. Quiet time is a specific time range when a journey doesn't send messages to participants, if all the following conditions are met:
--
--
--     * The EndpointDemographic.Timezone property of the endpoint for the participant is set to a valid value.
--
--
--     * The current time in the participant's time zone is later than or equal to the time specified by the QuietTime.Start property for the journey.
--
--
--     * The current time in the participant's time zone is earlier than or equal to the time specified by the QuietTime.End property for the journey.
--
--
-- If any of the preceding conditions isn't met, the participant will receive messages from the journey, even if quiet time is enabled.
--
-- /Note:/ Consider using 'quietTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrQuietTime :: Lens.Lens' JourneyResponse (Core.Maybe Types.QuietTime)
jrQuietTime = Lens.field @"quietTime"
{-# INLINEABLE jrQuietTime #-}
{-# DEPRECATED quietTime "Use generic-lens or generic-optics with 'quietTime' instead"  #-}

-- | The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
--
-- /Note:/ Consider using 'refreshFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrRefreshFrequency :: Lens.Lens' JourneyResponse (Core.Maybe Core.Text)
jrRefreshFrequency = Lens.field @"refreshFrequency"
{-# INLINEABLE jrRefreshFrequency #-}
{-# DEPRECATED refreshFrequency "Use generic-lens or generic-optics with 'refreshFrequency' instead"  #-}

-- | The schedule settings for the journey.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrSchedule :: Lens.Lens' JourneyResponse (Core.Maybe Types.JourneySchedule)
jrSchedule = Lens.field @"schedule"
{-# INLINEABLE jrSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The unique identifier for the first activity in the journey.
--
-- /Note:/ Consider using 'startActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrStartActivity :: Lens.Lens' JourneyResponse (Core.Maybe Core.Text)
jrStartActivity = Lens.field @"startActivity"
{-# INLINEABLE jrStartActivity #-}
{-# DEPRECATED startActivity "Use generic-lens or generic-optics with 'startActivity' instead"  #-}

-- | The segment that defines which users are participants in the journey.
--
-- /Note:/ Consider using 'startCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrStartCondition :: Lens.Lens' JourneyResponse (Core.Maybe Types.StartCondition)
jrStartCondition = Lens.field @"startCondition"
{-# INLINEABLE jrStartCondition #-}
{-# DEPRECATED startCondition "Use generic-lens or generic-optics with 'startCondition' instead"  #-}

-- | The current status of the journey. Possible values are:
--
--
--     * DRAFT - The journey is being developed and hasn't been published yet.
--
--
--     * ACTIVE - The journey has been developed and published. Depending on the journey's schedule, the journey may currently be running or scheduled to start running at a later time. If a journey's status is ACTIVE, you can't add, change, or remove activities from it.
--
--
--     * COMPLETED - The journey has been published and has finished running. All participants have entered the journey and no participants are waiting to complete the journey or any activities in the journey.
--
--
--     * CANCELLED - The journey has been stopped. If a journey's status is CANCELLED, you can't add, change, or remove activities or segment settings from the journey.
--
--
--     * CLOSED - The journey has been published and has started running. It may have also passed its scheduled end time, or passed its scheduled start time and a refresh frequency hasn't been specified for it. If a journey's status is CLOSED, you can't add participants to it, and no existing participants can enter the journey for the first time. However, any existing participants who are currently waiting to start an activity may continue the journey.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrState :: Lens.Lens' JourneyResponse (Core.Maybe Types.State)
jrState = Lens.field @"state"
{-# INLINEABLE jrState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | This object is not used or supported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrTags :: Lens.Lens' JourneyResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
jrTags = Lens.field @"tags"
{-# INLINEABLE jrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON JourneyResponse where
        parseJSON
          = Core.withObject "JourneyResponse" Core.$
              \ x ->
                JourneyResponse' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Id" Core.<*>
                    x Core..: "ApplicationId"
                    Core.<*> x Core..:? "Activities"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "Limits"
                    Core.<*> x Core..:? "LocalTime"
                    Core.<*> x Core..:? "QuietTime"
                    Core.<*> x Core..:? "RefreshFrequency"
                    Core.<*> x Core..:? "Schedule"
                    Core.<*> x Core..:? "StartActivity"
                    Core.<*> x Core..:? "StartCondition"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "tags"
