{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteJourneyRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.WriteJourneyRequest
  ( WriteJourneyRequest (..)
  -- * Smart constructor
  , mkWriteJourneyRequest
  -- * Lenses
  , wjrName
  , wjrActivities
  , wjrCreationDate
  , wjrLastModifiedDate
  , wjrLimits
  , wjrLocalTime
  , wjrQuietTime
  , wjrRefreshFrequency
  , wjrSchedule
  , wjrStartActivity
  , wjrStartCondition
  , wjrState
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Activity as Types
import qualified Network.AWS.Pinpoint.Types.JourneyLimits as Types
import qualified Network.AWS.Pinpoint.Types.JourneySchedule as Types
import qualified Network.AWS.Pinpoint.Types.QuietTime as Types
import qualified Network.AWS.Pinpoint.Types.StartCondition as Types
import qualified Network.AWS.Pinpoint.Types.State as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the configuration and other settings for a journey.
--
-- /See:/ 'mkWriteJourneyRequest' smart constructor.
data WriteJourneyRequest = WriteJourneyRequest'
  { name :: Core.Text
    -- ^ The name of the journey. A journey name can contain a maximum of 150 characters. The characters can be alphanumeric characters or symbols, such as underscores (_) or hyphens (-). A journey name can't contain any spaces.
  , activities :: Core.Maybe (Core.HashMap Core.Text Types.Activity)
    -- ^ A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity. An activity identifier can contain a maximum of 100 characters. The characters must be alphanumeric characters.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date, in ISO 8601 format, when the journey was created.
  , lastModifiedDate :: Core.Maybe Core.Text
    -- ^ The date, in ISO 8601 format, when the journey was last modified.
  , limits :: Core.Maybe Types.JourneyLimits
    -- ^ The messaging and entry limits for the journey.
  , localTime :: Core.Maybe Core.Bool
    -- ^ Specifies whether the journey's scheduled start and end times use each participant's local time. To base the schedule on each participant's local time, set this value to true.
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
    -- ^ The unique identifier for the first activity in the journey. The identifier for this activity can contain a maximum of 128 characters. The characters must be alphanumeric characters.
  , startCondition :: Core.Maybe Types.StartCondition
    -- ^ The segment that defines which users are participants in the journey.
  , state :: Core.Maybe Types.State
    -- ^ The status of the journey. Valid values are:
--
--
--     * DRAFT - Saves the journey and doesn't publish it.
--
--
--     * ACTIVE - Saves and publishes the journey. Depending on the journey's schedule, the journey starts running immediately or at the scheduled start time. If a journey's status is ACTIVE, you can't add, change, or remove activities from it.
--
--
-- The CANCELLED, COMPLETED, and CLOSED values are not supported in requests to create or update a journey. To cancel a journey, use the <link>Journey State resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'WriteJourneyRequest' value with any optional fields omitted.
mkWriteJourneyRequest
    :: Core.Text -- ^ 'name'
    -> WriteJourneyRequest
mkWriteJourneyRequest name
  = WriteJourneyRequest'{name, activities = Core.Nothing,
                         creationDate = Core.Nothing, lastModifiedDate = Core.Nothing,
                         limits = Core.Nothing, localTime = Core.Nothing,
                         quietTime = Core.Nothing, refreshFrequency = Core.Nothing,
                         schedule = Core.Nothing, startActivity = Core.Nothing,
                         startCondition = Core.Nothing, state = Core.Nothing}

-- | The name of the journey. A journey name can contain a maximum of 150 characters. The characters can be alphanumeric characters or symbols, such as underscores (_) or hyphens (-). A journey name can't contain any spaces.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrName :: Lens.Lens' WriteJourneyRequest Core.Text
wjrName = Lens.field @"name"
{-# INLINEABLE wjrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity. An activity identifier can contain a maximum of 100 characters. The characters must be alphanumeric characters.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrActivities :: Lens.Lens' WriteJourneyRequest (Core.Maybe (Core.HashMap Core.Text Types.Activity))
wjrActivities = Lens.field @"activities"
{-# INLINEABLE wjrActivities #-}
{-# DEPRECATED activities "Use generic-lens or generic-optics with 'activities' instead"  #-}

-- | The date, in ISO 8601 format, when the journey was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrCreationDate :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Text)
wjrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE wjrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The date, in ISO 8601 format, when the journey was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrLastModifiedDate :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Text)
wjrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE wjrLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The messaging and entry limits for the journey.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrLimits :: Lens.Lens' WriteJourneyRequest (Core.Maybe Types.JourneyLimits)
wjrLimits = Lens.field @"limits"
{-# INLINEABLE wjrLimits #-}
{-# DEPRECATED limits "Use generic-lens or generic-optics with 'limits' instead"  #-}

-- | Specifies whether the journey's scheduled start and end times use each participant's local time. To base the schedule on each participant's local time, set this value to true.
--
-- /Note:/ Consider using 'localTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrLocalTime :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Bool)
wjrLocalTime = Lens.field @"localTime"
{-# INLINEABLE wjrLocalTime #-}
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
wjrQuietTime :: Lens.Lens' WriteJourneyRequest (Core.Maybe Types.QuietTime)
wjrQuietTime = Lens.field @"quietTime"
{-# INLINEABLE wjrQuietTime #-}
{-# DEPRECATED quietTime "Use generic-lens or generic-optics with 'quietTime' instead"  #-}

-- | The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
--
-- /Note:/ Consider using 'refreshFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrRefreshFrequency :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Text)
wjrRefreshFrequency = Lens.field @"refreshFrequency"
{-# INLINEABLE wjrRefreshFrequency #-}
{-# DEPRECATED refreshFrequency "Use generic-lens or generic-optics with 'refreshFrequency' instead"  #-}

-- | The schedule settings for the journey.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrSchedule :: Lens.Lens' WriteJourneyRequest (Core.Maybe Types.JourneySchedule)
wjrSchedule = Lens.field @"schedule"
{-# INLINEABLE wjrSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The unique identifier for the first activity in the journey. The identifier for this activity can contain a maximum of 128 characters. The characters must be alphanumeric characters.
--
-- /Note:/ Consider using 'startActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrStartActivity :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Text)
wjrStartActivity = Lens.field @"startActivity"
{-# INLINEABLE wjrStartActivity #-}
{-# DEPRECATED startActivity "Use generic-lens or generic-optics with 'startActivity' instead"  #-}

-- | The segment that defines which users are participants in the journey.
--
-- /Note:/ Consider using 'startCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrStartCondition :: Lens.Lens' WriteJourneyRequest (Core.Maybe Types.StartCondition)
wjrStartCondition = Lens.field @"startCondition"
{-# INLINEABLE wjrStartCondition #-}
{-# DEPRECATED startCondition "Use generic-lens or generic-optics with 'startCondition' instead"  #-}

-- | The status of the journey. Valid values are:
--
--
--     * DRAFT - Saves the journey and doesn't publish it.
--
--
--     * ACTIVE - Saves and publishes the journey. Depending on the journey's schedule, the journey starts running immediately or at the scheduled start time. If a journey's status is ACTIVE, you can't add, change, or remove activities from it.
--
--
-- The CANCELLED, COMPLETED, and CLOSED values are not supported in requests to create or update a journey. To cancel a journey, use the <link>Journey State resource.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrState :: Lens.Lens' WriteJourneyRequest (Core.Maybe Types.State)
wjrState = Lens.field @"state"
{-# INLINEABLE wjrState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON WriteJourneyRequest where
        toJSON WriteJourneyRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("Activities" Core..=) Core.<$> activities,
                  ("CreationDate" Core..=) Core.<$> creationDate,
                  ("LastModifiedDate" Core..=) Core.<$> lastModifiedDate,
                  ("Limits" Core..=) Core.<$> limits,
                  ("LocalTime" Core..=) Core.<$> localTime,
                  ("QuietTime" Core..=) Core.<$> quietTime,
                  ("RefreshFrequency" Core..=) Core.<$> refreshFrequency,
                  ("Schedule" Core..=) Core.<$> schedule,
                  ("StartActivity" Core..=) Core.<$> startActivity,
                  ("StartCondition" Core..=) Core.<$> startCondition,
                  ("State" Core..=) Core.<$> state])
