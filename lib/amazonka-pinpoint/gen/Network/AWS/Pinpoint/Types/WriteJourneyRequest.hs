-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteJourneyRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteJourneyRequest
  ( WriteJourneyRequest (..),

    -- * Smart constructor
    mkWriteJourneyRequest,

    -- * Lenses
    wjrState,
    wjrLastModifiedDate,
    wjrSchedule,
    wjrLocalTime,
    wjrActivities,
    wjrLimits,
    wjrQuietTime,
    wjrStartActivity,
    wjrCreationDate,
    wjrStartCondition,
    wjrRefreshFrequency,
    wjrName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Activity
import Network.AWS.Pinpoint.Types.JourneyLimits
import Network.AWS.Pinpoint.Types.JourneySchedule
import Network.AWS.Pinpoint.Types.QuietTime
import Network.AWS.Pinpoint.Types.StartCondition
import Network.AWS.Pinpoint.Types.State
import qualified Network.AWS.Prelude as Lude

-- | Specifies the configuration and other settings for a journey.
--
-- /See:/ 'mkWriteJourneyRequest' smart constructor.
data WriteJourneyRequest = WriteJourneyRequest'
  { state ::
      Lude.Maybe State,
    lastModifiedDate :: Lude.Maybe Lude.Text,
    schedule :: Lude.Maybe JourneySchedule,
    localTime :: Lude.Maybe Lude.Bool,
    activities ::
      Lude.Maybe (Lude.HashMap Lude.Text (Activity)),
    limits :: Lude.Maybe JourneyLimits,
    quietTime :: Lude.Maybe QuietTime,
    startActivity :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Text,
    startCondition :: Lude.Maybe StartCondition,
    refreshFrequency :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WriteJourneyRequest' with the minimum fields required to make a request.
--
-- * 'activities' - A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity. An activity identifier can contain a maximum of 100 characters. The characters must be alphanumeric characters.
-- * 'creationDate' - The date, in ISO 8601 format, when the journey was created.
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the journey was last modified.
-- * 'limits' - The messaging and entry limits for the journey.
-- * 'localTime' - Specifies whether the journey's scheduled start and end times use each participant's local time. To base the schedule on each participant's local time, set this value to true.
-- * 'name' - The name of the journey. A journey name can contain a maximum of 150 characters. The characters can be alphanumeric characters or symbols, such as underscores (_) or hyphens (-). A journey name can't contain any spaces.
-- * 'quietTime' - The quiet time settings for the journey. Quiet time is a specific time range when a journey doesn't send messages to participants, if all the following conditions are met:
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
-- * 'refreshFrequency' - The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
-- * 'schedule' - The schedule settings for the journey.
-- * 'startActivity' - The unique identifier for the first activity in the journey. The identifier for this activity can contain a maximum of 128 characters. The characters must be alphanumeric characters.
-- * 'startCondition' - The segment that defines which users are participants in the journey.
-- * 'state' - The status of the journey. Valid values are:
--
--
--     * DRAFT - Saves the journey and doesn't publish it.
--
--
--     * ACTIVE - Saves and publishes the journey. Depending on the journey's schedule, the journey starts running immediately or at the scheduled start time. If a journey's status is ACTIVE, you can't add, change, or remove activities from it.
--
--
-- The CANCELLED, COMPLETED, and CLOSED values are not supported in requests to create or update a journey. To cancel a journey, use the <link>Journey State resource.
mkWriteJourneyRequest ::
  -- | 'name'
  Lude.Text ->
  WriteJourneyRequest
mkWriteJourneyRequest pName_ =
  WriteJourneyRequest'
    { state = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      schedule = Lude.Nothing,
      localTime = Lude.Nothing,
      activities = Lude.Nothing,
      limits = Lude.Nothing,
      quietTime = Lude.Nothing,
      startActivity = Lude.Nothing,
      creationDate = Lude.Nothing,
      startCondition = Lude.Nothing,
      refreshFrequency = Lude.Nothing,
      name = pName_
    }

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
wjrState :: Lens.Lens' WriteJourneyRequest (Lude.Maybe State)
wjrState = Lens.lens (state :: WriteJourneyRequest -> Lude.Maybe State) (\s a -> s {state = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date, in ISO 8601 format, when the journey was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrLastModifiedDate :: Lens.Lens' WriteJourneyRequest (Lude.Maybe Lude.Text)
wjrLastModifiedDate = Lens.lens (lastModifiedDate :: WriteJourneyRequest -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The schedule settings for the journey.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrSchedule :: Lens.Lens' WriteJourneyRequest (Lude.Maybe JourneySchedule)
wjrSchedule = Lens.lens (schedule :: WriteJourneyRequest -> Lude.Maybe JourneySchedule) (\s a -> s {schedule = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | Specifies whether the journey's scheduled start and end times use each participant's local time. To base the schedule on each participant's local time, set this value to true.
--
-- /Note:/ Consider using 'localTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrLocalTime :: Lens.Lens' WriteJourneyRequest (Lude.Maybe Lude.Bool)
wjrLocalTime = Lens.lens (localTime :: WriteJourneyRequest -> Lude.Maybe Lude.Bool) (\s a -> s {localTime = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrLocalTime "Use generic-lens or generic-optics with 'localTime' instead." #-}

-- | A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity. An activity identifier can contain a maximum of 100 characters. The characters must be alphanumeric characters.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrActivities :: Lens.Lens' WriteJourneyRequest (Lude.Maybe (Lude.HashMap Lude.Text (Activity)))
wjrActivities = Lens.lens (activities :: WriteJourneyRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Activity))) (\s a -> s {activities = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

-- | The messaging and entry limits for the journey.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrLimits :: Lens.Lens' WriteJourneyRequest (Lude.Maybe JourneyLimits)
wjrLimits = Lens.lens (limits :: WriteJourneyRequest -> Lude.Maybe JourneyLimits) (\s a -> s {limits = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

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
wjrQuietTime :: Lens.Lens' WriteJourneyRequest (Lude.Maybe QuietTime)
wjrQuietTime = Lens.lens (quietTime :: WriteJourneyRequest -> Lude.Maybe QuietTime) (\s a -> s {quietTime = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrQuietTime "Use generic-lens or generic-optics with 'quietTime' instead." #-}

-- | The unique identifier for the first activity in the journey. The identifier for this activity can contain a maximum of 128 characters. The characters must be alphanumeric characters.
--
-- /Note:/ Consider using 'startActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrStartActivity :: Lens.Lens' WriteJourneyRequest (Lude.Maybe Lude.Text)
wjrStartActivity = Lens.lens (startActivity :: WriteJourneyRequest -> Lude.Maybe Lude.Text) (\s a -> s {startActivity = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrStartActivity "Use generic-lens or generic-optics with 'startActivity' instead." #-}

-- | The date, in ISO 8601 format, when the journey was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrCreationDate :: Lens.Lens' WriteJourneyRequest (Lude.Maybe Lude.Text)
wjrCreationDate = Lens.lens (creationDate :: WriteJourneyRequest -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The segment that defines which users are participants in the journey.
--
-- /Note:/ Consider using 'startCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrStartCondition :: Lens.Lens' WriteJourneyRequest (Lude.Maybe StartCondition)
wjrStartCondition = Lens.lens (startCondition :: WriteJourneyRequest -> Lude.Maybe StartCondition) (\s a -> s {startCondition = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrStartCondition "Use generic-lens or generic-optics with 'startCondition' instead." #-}

-- | The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
--
-- /Note:/ Consider using 'refreshFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrRefreshFrequency :: Lens.Lens' WriteJourneyRequest (Lude.Maybe Lude.Text)
wjrRefreshFrequency = Lens.lens (refreshFrequency :: WriteJourneyRequest -> Lude.Maybe Lude.Text) (\s a -> s {refreshFrequency = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrRefreshFrequency "Use generic-lens or generic-optics with 'refreshFrequency' instead." #-}

-- | The name of the journey. A journey name can contain a maximum of 150 characters. The characters can be alphanumeric characters or symbols, such as underscores (_) or hyphens (-). A journey name can't contain any spaces.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wjrName :: Lens.Lens' WriteJourneyRequest Lude.Text
wjrName = Lens.lens (name :: WriteJourneyRequest -> Lude.Text) (\s a -> s {name = a} :: WriteJourneyRequest)
{-# DEPRECATED wjrName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON WriteJourneyRequest where
  toJSON WriteJourneyRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("State" Lude..=) Lude.<$> state,
            ("LastModifiedDate" Lude..=) Lude.<$> lastModifiedDate,
            ("Schedule" Lude..=) Lude.<$> schedule,
            ("LocalTime" Lude..=) Lude.<$> localTime,
            ("Activities" Lude..=) Lude.<$> activities,
            ("Limits" Lude..=) Lude.<$> limits,
            ("QuietTime" Lude..=) Lude.<$> quietTime,
            ("StartActivity" Lude..=) Lude.<$> startActivity,
            ("CreationDate" Lude..=) Lude.<$> creationDate,
            ("StartCondition" Lude..=) Lude.<$> startCondition,
            ("RefreshFrequency" Lude..=) Lude.<$> refreshFrequency,
            Lude.Just ("Name" Lude..= name)
          ]
      )
