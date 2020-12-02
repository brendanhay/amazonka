{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Activity
import Network.AWS.Pinpoint.Types.JourneyLimits
import Network.AWS.Pinpoint.Types.JourneySchedule
import Network.AWS.Pinpoint.Types.QuietTime
import Network.AWS.Pinpoint.Types.StartCondition
import Network.AWS.Pinpoint.Types.State
import Network.AWS.Prelude

-- | Provides information about the status, configuration, and other settings for a journey.
--
--
--
-- /See:/ 'journeyResponse' smart constructor.
data JourneyResponse = JourneyResponse'
  { _jState :: !(Maybe State),
    _jLastModifiedDate :: !(Maybe Text),
    _jSchedule :: !(Maybe JourneySchedule),
    _jLocalTime :: !(Maybe Bool),
    _jActivities :: !(Maybe (Map Text (Activity))),
    _jLimits :: !(Maybe JourneyLimits),
    _jQuietTime :: !(Maybe QuietTime),
    _jStartActivity :: !(Maybe Text),
    _jCreationDate :: !(Maybe Text),
    _jStartCondition :: !(Maybe StartCondition),
    _jRefreshFrequency :: !(Maybe Text),
    _jTags :: !(Maybe (Map Text (Text))),
    _jName :: !Text,
    _jId :: !Text,
    _jApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jState' - The current status of the journey. Possible values are:     * DRAFT - The journey is being developed and hasn't been published yet.     * ACTIVE - The journey has been developed and published. Depending on the journey's schedule, the journey may currently be running or scheduled to start running at a later time. If a journey's status is ACTIVE, you can't add, change, or remove activities from it.     * COMPLETED - The journey has been published and has finished running. All participants have entered the journey and no participants are waiting to complete the journey or any activities in the journey.     * CANCELLED - The journey has been stopped. If a journey's status is CANCELLED, you can't add, change, or remove activities or segment settings from the journey.     * CLOSED - The journey has been published and has started running. It may have also passed its scheduled end time, or passed its scheduled start time and a refresh frequency hasn't been specified for it. If a journey's status is CLOSED, you can't add participants to it, and no existing participants can enter the journey for the first time. However, any existing participants who are currently waiting to start an activity may continue the journey.
--
-- * 'jLastModifiedDate' - The date, in ISO 8601 format, when the journey was last modified.
--
-- * 'jSchedule' - The schedule settings for the journey.
--
-- * 'jLocalTime' - Specifies whether the journey's scheduled start and end times use each participant's local time. If this value is true, the schedule uses each participant's local time.
--
-- * 'jActivities' - A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity.
--
-- * 'jLimits' - The messaging and entry limits for the journey.
--
-- * 'jQuietTime' - The quiet time settings for the journey. Quiet time is a specific time range when a journey doesn't send messages to participants, if all the following conditions are met:     * The EndpointDemographic.Timezone property of the endpoint for the participant is set to a valid value.     * The current time in the participant's time zone is later than or equal to the time specified by the QuietTime.Start property for the journey.     * The current time in the participant's time zone is earlier than or equal to the time specified by the QuietTime.End property for the journey. If any of the preceding conditions isn't met, the participant will receive messages from the journey, even if quiet time is enabled.
--
-- * 'jStartActivity' - The unique identifier for the first activity in the journey.
--
-- * 'jCreationDate' - The date, in ISO 8601 format, when the journey was created.
--
-- * 'jStartCondition' - The segment that defines which users are participants in the journey.
--
-- * 'jRefreshFrequency' - The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
--
-- * 'jTags' - This object is not used or supported.
--
-- * 'jName' - The name of the journey.
--
-- * 'jId' - The unique identifier for the journey.
--
-- * 'jApplicationId' - The unique identifier for the application that the journey applies to.
journeyResponse ::
  -- | 'jName'
  Text ->
  -- | 'jId'
  Text ->
  -- | 'jApplicationId'
  Text ->
  JourneyResponse
journeyResponse pName_ pId_ pApplicationId_ =
  JourneyResponse'
    { _jState = Nothing,
      _jLastModifiedDate = Nothing,
      _jSchedule = Nothing,
      _jLocalTime = Nothing,
      _jActivities = Nothing,
      _jLimits = Nothing,
      _jQuietTime = Nothing,
      _jStartActivity = Nothing,
      _jCreationDate = Nothing,
      _jStartCondition = Nothing,
      _jRefreshFrequency = Nothing,
      _jTags = Nothing,
      _jName = pName_,
      _jId = pId_,
      _jApplicationId = pApplicationId_
    }

-- | The current status of the journey. Possible values are:     * DRAFT - The journey is being developed and hasn't been published yet.     * ACTIVE - The journey has been developed and published. Depending on the journey's schedule, the journey may currently be running or scheduled to start running at a later time. If a journey's status is ACTIVE, you can't add, change, or remove activities from it.     * COMPLETED - The journey has been published and has finished running. All participants have entered the journey and no participants are waiting to complete the journey or any activities in the journey.     * CANCELLED - The journey has been stopped. If a journey's status is CANCELLED, you can't add, change, or remove activities or segment settings from the journey.     * CLOSED - The journey has been published and has started running. It may have also passed its scheduled end time, or passed its scheduled start time and a refresh frequency hasn't been specified for it. If a journey's status is CLOSED, you can't add participants to it, and no existing participants can enter the journey for the first time. However, any existing participants who are currently waiting to start an activity may continue the journey.
jState :: Lens' JourneyResponse (Maybe State)
jState = lens _jState (\s a -> s {_jState = a})

-- | The date, in ISO 8601 format, when the journey was last modified.
jLastModifiedDate :: Lens' JourneyResponse (Maybe Text)
jLastModifiedDate = lens _jLastModifiedDate (\s a -> s {_jLastModifiedDate = a})

-- | The schedule settings for the journey.
jSchedule :: Lens' JourneyResponse (Maybe JourneySchedule)
jSchedule = lens _jSchedule (\s a -> s {_jSchedule = a})

-- | Specifies whether the journey's scheduled start and end times use each participant's local time. If this value is true, the schedule uses each participant's local time.
jLocalTime :: Lens' JourneyResponse (Maybe Bool)
jLocalTime = lens _jLocalTime (\s a -> s {_jLocalTime = a})

-- | A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity.
jActivities :: Lens' JourneyResponse (HashMap Text (Activity))
jActivities = lens _jActivities (\s a -> s {_jActivities = a}) . _Default . _Map

-- | The messaging and entry limits for the journey.
jLimits :: Lens' JourneyResponse (Maybe JourneyLimits)
jLimits = lens _jLimits (\s a -> s {_jLimits = a})

-- | The quiet time settings for the journey. Quiet time is a specific time range when a journey doesn't send messages to participants, if all the following conditions are met:     * The EndpointDemographic.Timezone property of the endpoint for the participant is set to a valid value.     * The current time in the participant's time zone is later than or equal to the time specified by the QuietTime.Start property for the journey.     * The current time in the participant's time zone is earlier than or equal to the time specified by the QuietTime.End property for the journey. If any of the preceding conditions isn't met, the participant will receive messages from the journey, even if quiet time is enabled.
jQuietTime :: Lens' JourneyResponse (Maybe QuietTime)
jQuietTime = lens _jQuietTime (\s a -> s {_jQuietTime = a})

-- | The unique identifier for the first activity in the journey.
jStartActivity :: Lens' JourneyResponse (Maybe Text)
jStartActivity = lens _jStartActivity (\s a -> s {_jStartActivity = a})

-- | The date, in ISO 8601 format, when the journey was created.
jCreationDate :: Lens' JourneyResponse (Maybe Text)
jCreationDate = lens _jCreationDate (\s a -> s {_jCreationDate = a})

-- | The segment that defines which users are participants in the journey.
jStartCondition :: Lens' JourneyResponse (Maybe StartCondition)
jStartCondition = lens _jStartCondition (\s a -> s {_jStartCondition = a})

-- | The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
jRefreshFrequency :: Lens' JourneyResponse (Maybe Text)
jRefreshFrequency = lens _jRefreshFrequency (\s a -> s {_jRefreshFrequency = a})

-- | This object is not used or supported.
jTags :: Lens' JourneyResponse (HashMap Text (Text))
jTags = lens _jTags (\s a -> s {_jTags = a}) . _Default . _Map

-- | The name of the journey.
jName :: Lens' JourneyResponse Text
jName = lens _jName (\s a -> s {_jName = a})

-- | The unique identifier for the journey.
jId :: Lens' JourneyResponse Text
jId = lens _jId (\s a -> s {_jId = a})

-- | The unique identifier for the application that the journey applies to.
jApplicationId :: Lens' JourneyResponse Text
jApplicationId = lens _jApplicationId (\s a -> s {_jApplicationId = a})

instance FromJSON JourneyResponse where
  parseJSON =
    withObject
      "JourneyResponse"
      ( \x ->
          JourneyResponse'
            <$> (x .:? "State")
            <*> (x .:? "LastModifiedDate")
            <*> (x .:? "Schedule")
            <*> (x .:? "LocalTime")
            <*> (x .:? "Activities" .!= mempty)
            <*> (x .:? "Limits")
            <*> (x .:? "QuietTime")
            <*> (x .:? "StartActivity")
            <*> (x .:? "CreationDate")
            <*> (x .:? "StartCondition")
            <*> (x .:? "RefreshFrequency")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "Name")
            <*> (x .: "Id")
            <*> (x .: "ApplicationId")
      )

instance Hashable JourneyResponse

instance NFData JourneyResponse
