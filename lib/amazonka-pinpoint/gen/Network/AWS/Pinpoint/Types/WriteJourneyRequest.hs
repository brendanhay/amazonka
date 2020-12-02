{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteJourneyRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteJourneyRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Activity
import Network.AWS.Pinpoint.Types.JourneyLimits
import Network.AWS.Pinpoint.Types.JourneySchedule
import Network.AWS.Pinpoint.Types.QuietTime
import Network.AWS.Pinpoint.Types.StartCondition
import Network.AWS.Pinpoint.Types.State
import Network.AWS.Prelude

-- | Specifies the configuration and other settings for a journey.
--
--
--
-- /See:/ 'writeJourneyRequest' smart constructor.
data WriteJourneyRequest = WriteJourneyRequest'
  { _wjrState ::
      !(Maybe State),
    _wjrLastModifiedDate :: !(Maybe Text),
    _wjrSchedule :: !(Maybe JourneySchedule),
    _wjrLocalTime :: !(Maybe Bool),
    _wjrActivities :: !(Maybe (Map Text (Activity))),
    _wjrLimits :: !(Maybe JourneyLimits),
    _wjrQuietTime :: !(Maybe QuietTime),
    _wjrStartActivity :: !(Maybe Text),
    _wjrCreationDate :: !(Maybe Text),
    _wjrStartCondition :: !(Maybe StartCondition),
    _wjrRefreshFrequency :: !(Maybe Text),
    _wjrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WriteJourneyRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wjrState' - The status of the journey. Valid values are:     * DRAFT - Saves the journey and doesn't publish it.     * ACTIVE - Saves and publishes the journey. Depending on the journey's schedule, the journey starts running immediately or at the scheduled start time. If a journey's status is ACTIVE, you can't add, change, or remove activities from it. The CANCELLED, COMPLETED, and CLOSED values are not supported in requests to create or update a journey. To cancel a journey, use the <link>Journey State resource.
--
-- * 'wjrLastModifiedDate' - The date, in ISO 8601 format, when the journey was last modified.
--
-- * 'wjrSchedule' - The schedule settings for the journey.
--
-- * 'wjrLocalTime' - Specifies whether the journey's scheduled start and end times use each participant's local time. To base the schedule on each participant's local time, set this value to true.
--
-- * 'wjrActivities' - A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity. An activity identifier can contain a maximum of 100 characters. The characters must be alphanumeric characters.
--
-- * 'wjrLimits' - The messaging and entry limits for the journey.
--
-- * 'wjrQuietTime' - The quiet time settings for the journey. Quiet time is a specific time range when a journey doesn't send messages to participants, if all the following conditions are met:     * The EndpointDemographic.Timezone property of the endpoint for the participant is set to a valid value.     * The current time in the participant's time zone is later than or equal to the time specified by the QuietTime.Start property for the journey.     * The current time in the participant's time zone is earlier than or equal to the time specified by the QuietTime.End property for the journey. If any of the preceding conditions isn't met, the participant will receive messages from the journey, even if quiet time is enabled.
--
-- * 'wjrStartActivity' - The unique identifier for the first activity in the journey. The identifier for this activity can contain a maximum of 128 characters. The characters must be alphanumeric characters.
--
-- * 'wjrCreationDate' - The date, in ISO 8601 format, when the journey was created.
--
-- * 'wjrStartCondition' - The segment that defines which users are participants in the journey.
--
-- * 'wjrRefreshFrequency' - The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
--
-- * 'wjrName' - The name of the journey. A journey name can contain a maximum of 150 characters. The characters can be alphanumeric characters or symbols, such as underscores (_) or hyphens (-). A journey name can't contain any spaces.
writeJourneyRequest ::
  -- | 'wjrName'
  Text ->
  WriteJourneyRequest
writeJourneyRequest pName_ =
  WriteJourneyRequest'
    { _wjrState = Nothing,
      _wjrLastModifiedDate = Nothing,
      _wjrSchedule = Nothing,
      _wjrLocalTime = Nothing,
      _wjrActivities = Nothing,
      _wjrLimits = Nothing,
      _wjrQuietTime = Nothing,
      _wjrStartActivity = Nothing,
      _wjrCreationDate = Nothing,
      _wjrStartCondition = Nothing,
      _wjrRefreshFrequency = Nothing,
      _wjrName = pName_
    }

-- | The status of the journey. Valid values are:     * DRAFT - Saves the journey and doesn't publish it.     * ACTIVE - Saves and publishes the journey. Depending on the journey's schedule, the journey starts running immediately or at the scheduled start time. If a journey's status is ACTIVE, you can't add, change, or remove activities from it. The CANCELLED, COMPLETED, and CLOSED values are not supported in requests to create or update a journey. To cancel a journey, use the <link>Journey State resource.
wjrState :: Lens' WriteJourneyRequest (Maybe State)
wjrState = lens _wjrState (\s a -> s {_wjrState = a})

-- | The date, in ISO 8601 format, when the journey was last modified.
wjrLastModifiedDate :: Lens' WriteJourneyRequest (Maybe Text)
wjrLastModifiedDate = lens _wjrLastModifiedDate (\s a -> s {_wjrLastModifiedDate = a})

-- | The schedule settings for the journey.
wjrSchedule :: Lens' WriteJourneyRequest (Maybe JourneySchedule)
wjrSchedule = lens _wjrSchedule (\s a -> s {_wjrSchedule = a})

-- | Specifies whether the journey's scheduled start and end times use each participant's local time. To base the schedule on each participant's local time, set this value to true.
wjrLocalTime :: Lens' WriteJourneyRequest (Maybe Bool)
wjrLocalTime = lens _wjrLocalTime (\s a -> s {_wjrLocalTime = a})

-- | A map that contains a set of Activity objects, one object for each activity in the journey. For each Activity object, the key is the unique identifier (string) for an activity and the value is the settings for the activity. An activity identifier can contain a maximum of 100 characters. The characters must be alphanumeric characters.
wjrActivities :: Lens' WriteJourneyRequest (HashMap Text (Activity))
wjrActivities = lens _wjrActivities (\s a -> s {_wjrActivities = a}) . _Default . _Map

-- | The messaging and entry limits for the journey.
wjrLimits :: Lens' WriteJourneyRequest (Maybe JourneyLimits)
wjrLimits = lens _wjrLimits (\s a -> s {_wjrLimits = a})

-- | The quiet time settings for the journey. Quiet time is a specific time range when a journey doesn't send messages to participants, if all the following conditions are met:     * The EndpointDemographic.Timezone property of the endpoint for the participant is set to a valid value.     * The current time in the participant's time zone is later than or equal to the time specified by the QuietTime.Start property for the journey.     * The current time in the participant's time zone is earlier than or equal to the time specified by the QuietTime.End property for the journey. If any of the preceding conditions isn't met, the participant will receive messages from the journey, even if quiet time is enabled.
wjrQuietTime :: Lens' WriteJourneyRequest (Maybe QuietTime)
wjrQuietTime = lens _wjrQuietTime (\s a -> s {_wjrQuietTime = a})

-- | The unique identifier for the first activity in the journey. The identifier for this activity can contain a maximum of 128 characters. The characters must be alphanumeric characters.
wjrStartActivity :: Lens' WriteJourneyRequest (Maybe Text)
wjrStartActivity = lens _wjrStartActivity (\s a -> s {_wjrStartActivity = a})

-- | The date, in ISO 8601 format, when the journey was created.
wjrCreationDate :: Lens' WriteJourneyRequest (Maybe Text)
wjrCreationDate = lens _wjrCreationDate (\s a -> s {_wjrCreationDate = a})

-- | The segment that defines which users are participants in the journey.
wjrStartCondition :: Lens' WriteJourneyRequest (Maybe StartCondition)
wjrStartCondition = lens _wjrStartCondition (\s a -> s {_wjrStartCondition = a})

-- | The frequency with which Amazon Pinpoint evaluates segment and event data for the journey, as a duration in ISO 8601 format.
wjrRefreshFrequency :: Lens' WriteJourneyRequest (Maybe Text)
wjrRefreshFrequency = lens _wjrRefreshFrequency (\s a -> s {_wjrRefreshFrequency = a})

-- | The name of the journey. A journey name can contain a maximum of 150 characters. The characters can be alphanumeric characters or symbols, such as underscores (_) or hyphens (-). A journey name can't contain any spaces.
wjrName :: Lens' WriteJourneyRequest Text
wjrName = lens _wjrName (\s a -> s {_wjrName = a})

instance Hashable WriteJourneyRequest

instance NFData WriteJourneyRequest

instance ToJSON WriteJourneyRequest where
  toJSON WriteJourneyRequest' {..} =
    object
      ( catMaybes
          [ ("State" .=) <$> _wjrState,
            ("LastModifiedDate" .=) <$> _wjrLastModifiedDate,
            ("Schedule" .=) <$> _wjrSchedule,
            ("LocalTime" .=) <$> _wjrLocalTime,
            ("Activities" .=) <$> _wjrActivities,
            ("Limits" .=) <$> _wjrLimits,
            ("QuietTime" .=) <$> _wjrQuietTime,
            ("StartActivity" .=) <$> _wjrStartActivity,
            ("CreationDate" .=) <$> _wjrCreationDate,
            ("StartCondition" .=) <$> _wjrStartCondition,
            ("RefreshFrequency" .=) <$> _wjrRefreshFrequency,
            Just ("Name" .= _wjrName)
          ]
      )
