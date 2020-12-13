{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyResponse
  ( JourneyResponse (..),

    -- * Smart constructor
    mkJourneyResponse,

    -- * Lenses
    jState,
    jLastModifiedDate,
    jSchedule,
    jLocalTime,
    jActivities,
    jLimits,
    jQuietTime,
    jApplicationId,
    jName,
    jId,
    jStartActivity,
    jCreationDate,
    jStartCondition,
    jRefreshFrequency,
    jTags,
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

-- | Provides information about the status, configuration, and other settings for a journey.
--
-- /See:/ 'mkJourneyResponse' smart constructor.
data JourneyResponse = JourneyResponse'
  { -- | The current status of the journey. Possible values are:
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
    state :: Lude.Maybe State,
    -- | The date, in ISO 8601 format, when the journey was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Text,
    -- | The schedule settings for the journey.
    schedule :: Lude.Maybe JourneySchedule,
    -- | Specifies whether the journey's scheduled start and end times use each participant's local time. If this value is true, the schedule uses each participant's local time.
    localTime :: Lude.Maybe Lude.Bool,
    -- | A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity.
    activities :: Lude.Maybe (Lude.HashMap Lude.Text (Activity)),
    -- | The messaging and entry limits for the journey.
    limits :: Lude.Maybe JourneyLimits,
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
    quietTime :: Lude.Maybe QuietTime,
    -- | The unique identifier for the application that the journey applies to.
    applicationId :: Lude.Text,
    -- | The name of the journey.
    name :: Lude.Text,
    -- | The unique identifier for the journey.
    id :: Lude.Text,
    -- | The unique identifier for the first activity in the journey.
    startActivity :: Lude.Maybe Lude.Text,
    -- | The date, in ISO 8601 format, when the journey was created.
    creationDate :: Lude.Maybe Lude.Text,
    -- | The segment that defines which users are participants in the journey.
    startCondition :: Lude.Maybe StartCondition,
    -- | The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
    refreshFrequency :: Lude.Maybe Lude.Text,
    -- | This object is not used or supported.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneyResponse' with the minimum fields required to make a request.
--
-- * 'state' - The current status of the journey. Possible values are:
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
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the journey was last modified.
-- * 'schedule' - The schedule settings for the journey.
-- * 'localTime' - Specifies whether the journey's scheduled start and end times use each participant's local time. If this value is true, the schedule uses each participant's local time.
-- * 'activities' - A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity.
-- * 'limits' - The messaging and entry limits for the journey.
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
-- * 'applicationId' - The unique identifier for the application that the journey applies to.
-- * 'name' - The name of the journey.
-- * 'id' - The unique identifier for the journey.
-- * 'startActivity' - The unique identifier for the first activity in the journey.
-- * 'creationDate' - The date, in ISO 8601 format, when the journey was created.
-- * 'startCondition' - The segment that defines which users are participants in the journey.
-- * 'refreshFrequency' - The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
-- * 'tags' - This object is not used or supported.
mkJourneyResponse ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  JourneyResponse
mkJourneyResponse pApplicationId_ pName_ pId_ =
  JourneyResponse'
    { state = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      schedule = Lude.Nothing,
      localTime = Lude.Nothing,
      activities = Lude.Nothing,
      limits = Lude.Nothing,
      quietTime = Lude.Nothing,
      applicationId = pApplicationId_,
      name = pName_,
      id = pId_,
      startActivity = Lude.Nothing,
      creationDate = Lude.Nothing,
      startCondition = Lude.Nothing,
      refreshFrequency = Lude.Nothing,
      tags = Lude.Nothing
    }

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
jState :: Lens.Lens' JourneyResponse (Lude.Maybe State)
jState = Lens.lens (state :: JourneyResponse -> Lude.Maybe State) (\s a -> s {state = a} :: JourneyResponse)
{-# DEPRECATED jState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date, in ISO 8601 format, when the journey was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLastModifiedDate :: Lens.Lens' JourneyResponse (Lude.Maybe Lude.Text)
jLastModifiedDate = Lens.lens (lastModifiedDate :: JourneyResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: JourneyResponse)
{-# DEPRECATED jLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The schedule settings for the journey.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jSchedule :: Lens.Lens' JourneyResponse (Lude.Maybe JourneySchedule)
jSchedule = Lens.lens (schedule :: JourneyResponse -> Lude.Maybe JourneySchedule) (\s a -> s {schedule = a} :: JourneyResponse)
{-# DEPRECATED jSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | Specifies whether the journey's scheduled start and end times use each participant's local time. If this value is true, the schedule uses each participant's local time.
--
-- /Note:/ Consider using 'localTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLocalTime :: Lens.Lens' JourneyResponse (Lude.Maybe Lude.Bool)
jLocalTime = Lens.lens (localTime :: JourneyResponse -> Lude.Maybe Lude.Bool) (\s a -> s {localTime = a} :: JourneyResponse)
{-# DEPRECATED jLocalTime "Use generic-lens or generic-optics with 'localTime' instead." #-}

-- | A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jActivities :: Lens.Lens' JourneyResponse (Lude.Maybe (Lude.HashMap Lude.Text (Activity)))
jActivities = Lens.lens (activities :: JourneyResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Activity))) (\s a -> s {activities = a} :: JourneyResponse)
{-# DEPRECATED jActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

-- | The messaging and entry limits for the journey.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLimits :: Lens.Lens' JourneyResponse (Lude.Maybe JourneyLimits)
jLimits = Lens.lens (limits :: JourneyResponse -> Lude.Maybe JourneyLimits) (\s a -> s {limits = a} :: JourneyResponse)
{-# DEPRECATED jLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

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
jQuietTime :: Lens.Lens' JourneyResponse (Lude.Maybe QuietTime)
jQuietTime = Lens.lens (quietTime :: JourneyResponse -> Lude.Maybe QuietTime) (\s a -> s {quietTime = a} :: JourneyResponse)
{-# DEPRECATED jQuietTime "Use generic-lens or generic-optics with 'quietTime' instead." #-}

-- | The unique identifier for the application that the journey applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jApplicationId :: Lens.Lens' JourneyResponse Lude.Text
jApplicationId = Lens.lens (applicationId :: JourneyResponse -> Lude.Text) (\s a -> s {applicationId = a} :: JourneyResponse)
{-# DEPRECATED jApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the journey.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jName :: Lens.Lens' JourneyResponse Lude.Text
jName = Lens.lens (name :: JourneyResponse -> Lude.Text) (\s a -> s {name = a} :: JourneyResponse)
{-# DEPRECATED jName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jId :: Lens.Lens' JourneyResponse Lude.Text
jId = Lens.lens (id :: JourneyResponse -> Lude.Text) (\s a -> s {id = a} :: JourneyResponse)
{-# DEPRECATED jId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The unique identifier for the first activity in the journey.
--
-- /Note:/ Consider using 'startActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStartActivity :: Lens.Lens' JourneyResponse (Lude.Maybe Lude.Text)
jStartActivity = Lens.lens (startActivity :: JourneyResponse -> Lude.Maybe Lude.Text) (\s a -> s {startActivity = a} :: JourneyResponse)
{-# DEPRECATED jStartActivity "Use generic-lens or generic-optics with 'startActivity' instead." #-}

-- | The date, in ISO 8601 format, when the journey was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreationDate :: Lens.Lens' JourneyResponse (Lude.Maybe Lude.Text)
jCreationDate = Lens.lens (creationDate :: JourneyResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: JourneyResponse)
{-# DEPRECATED jCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The segment that defines which users are participants in the journey.
--
-- /Note:/ Consider using 'startCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStartCondition :: Lens.Lens' JourneyResponse (Lude.Maybe StartCondition)
jStartCondition = Lens.lens (startCondition :: JourneyResponse -> Lude.Maybe StartCondition) (\s a -> s {startCondition = a} :: JourneyResponse)
{-# DEPRECATED jStartCondition "Use generic-lens or generic-optics with 'startCondition' instead." #-}

-- | The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
--
-- /Note:/ Consider using 'refreshFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jRefreshFrequency :: Lens.Lens' JourneyResponse (Lude.Maybe Lude.Text)
jRefreshFrequency = Lens.lens (refreshFrequency :: JourneyResponse -> Lude.Maybe Lude.Text) (\s a -> s {refreshFrequency = a} :: JourneyResponse)
{-# DEPRECATED jRefreshFrequency "Use generic-lens or generic-optics with 'refreshFrequency' instead." #-}

-- | This object is not used or supported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTags :: Lens.Lens' JourneyResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jTags = Lens.lens (tags :: JourneyResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: JourneyResponse)
{-# DEPRECATED jTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON JourneyResponse where
  parseJSON =
    Lude.withObject
      "JourneyResponse"
      ( \x ->
          JourneyResponse'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Schedule")
            Lude.<*> (x Lude..:? "LocalTime")
            Lude.<*> (x Lude..:? "Activities" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Limits")
            Lude.<*> (x Lude..:? "QuietTime")
            Lude.<*> (x Lude..: "ApplicationId")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..:? "StartActivity")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "StartCondition")
            Lude.<*> (x Lude..:? "RefreshFrequency")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
