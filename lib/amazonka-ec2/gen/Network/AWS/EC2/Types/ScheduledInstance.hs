{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstance where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstanceRecurrence
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstance' smart constructor.
data ScheduledInstance = ScheduledInstance'
  { _siPreviousSlotEndTime ::
      !(Maybe ISO8601),
    _siPlatform :: !(Maybe Text),
    _siTermStartDate :: !(Maybe ISO8601),
    _siInstanceCount :: !(Maybe Int),
    _siScheduledInstanceId :: !(Maybe Text),
    _siHourlyPrice :: !(Maybe Text),
    _siCreateDate :: !(Maybe ISO8601),
    _siSlotDurationInHours :: !(Maybe Int),
    _siTotalScheduledInstanceHours :: !(Maybe Int),
    _siInstanceType :: !(Maybe Text),
    _siRecurrence :: !(Maybe ScheduledInstanceRecurrence),
    _siAvailabilityZone :: !(Maybe Text),
    _siTermEndDate :: !(Maybe ISO8601),
    _siNextSlotStartTime :: !(Maybe ISO8601),
    _siNetworkPlatform :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siPreviousSlotEndTime' - The time that the previous schedule ended or will end.
--
-- * 'siPlatform' - The platform (@Linux/UNIX@ or @Windows@ ).
--
-- * 'siTermStartDate' - The start date for the Scheduled Instance.
--
-- * 'siInstanceCount' - The number of instances.
--
-- * 'siScheduledInstanceId' - The Scheduled Instance ID.
--
-- * 'siHourlyPrice' - The hourly price for a single instance.
--
-- * 'siCreateDate' - The date when the Scheduled Instance was purchased.
--
-- * 'siSlotDurationInHours' - The number of hours in the schedule.
--
-- * 'siTotalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
--
-- * 'siInstanceType' - The instance type.
--
-- * 'siRecurrence' - The schedule recurrence.
--
-- * 'siAvailabilityZone' - The Availability Zone.
--
-- * 'siTermEndDate' - The end date for the Scheduled Instance.
--
-- * 'siNextSlotStartTime' - The time for the next schedule to start.
--
-- * 'siNetworkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
scheduledInstance ::
  ScheduledInstance
scheduledInstance =
  ScheduledInstance'
    { _siPreviousSlotEndTime = Nothing,
      _siPlatform = Nothing,
      _siTermStartDate = Nothing,
      _siInstanceCount = Nothing,
      _siScheduledInstanceId = Nothing,
      _siHourlyPrice = Nothing,
      _siCreateDate = Nothing,
      _siSlotDurationInHours = Nothing,
      _siTotalScheduledInstanceHours = Nothing,
      _siInstanceType = Nothing,
      _siRecurrence = Nothing,
      _siAvailabilityZone = Nothing,
      _siTermEndDate = Nothing,
      _siNextSlotStartTime = Nothing,
      _siNetworkPlatform = Nothing
    }

-- | The time that the previous schedule ended or will end.
siPreviousSlotEndTime :: Lens' ScheduledInstance (Maybe UTCTime)
siPreviousSlotEndTime = lens _siPreviousSlotEndTime (\s a -> s {_siPreviousSlotEndTime = a}) . mapping _Time

-- | The platform (@Linux/UNIX@ or @Windows@ ).
siPlatform :: Lens' ScheduledInstance (Maybe Text)
siPlatform = lens _siPlatform (\s a -> s {_siPlatform = a})

-- | The start date for the Scheduled Instance.
siTermStartDate :: Lens' ScheduledInstance (Maybe UTCTime)
siTermStartDate = lens _siTermStartDate (\s a -> s {_siTermStartDate = a}) . mapping _Time

-- | The number of instances.
siInstanceCount :: Lens' ScheduledInstance (Maybe Int)
siInstanceCount = lens _siInstanceCount (\s a -> s {_siInstanceCount = a})

-- | The Scheduled Instance ID.
siScheduledInstanceId :: Lens' ScheduledInstance (Maybe Text)
siScheduledInstanceId = lens _siScheduledInstanceId (\s a -> s {_siScheduledInstanceId = a})

-- | The hourly price for a single instance.
siHourlyPrice :: Lens' ScheduledInstance (Maybe Text)
siHourlyPrice = lens _siHourlyPrice (\s a -> s {_siHourlyPrice = a})

-- | The date when the Scheduled Instance was purchased.
siCreateDate :: Lens' ScheduledInstance (Maybe UTCTime)
siCreateDate = lens _siCreateDate (\s a -> s {_siCreateDate = a}) . mapping _Time

-- | The number of hours in the schedule.
siSlotDurationInHours :: Lens' ScheduledInstance (Maybe Int)
siSlotDurationInHours = lens _siSlotDurationInHours (\s a -> s {_siSlotDurationInHours = a})

-- | The total number of hours for a single instance for the entire term.
siTotalScheduledInstanceHours :: Lens' ScheduledInstance (Maybe Int)
siTotalScheduledInstanceHours = lens _siTotalScheduledInstanceHours (\s a -> s {_siTotalScheduledInstanceHours = a})

-- | The instance type.
siInstanceType :: Lens' ScheduledInstance (Maybe Text)
siInstanceType = lens _siInstanceType (\s a -> s {_siInstanceType = a})

-- | The schedule recurrence.
siRecurrence :: Lens' ScheduledInstance (Maybe ScheduledInstanceRecurrence)
siRecurrence = lens _siRecurrence (\s a -> s {_siRecurrence = a})

-- | The Availability Zone.
siAvailabilityZone :: Lens' ScheduledInstance (Maybe Text)
siAvailabilityZone = lens _siAvailabilityZone (\s a -> s {_siAvailabilityZone = a})

-- | The end date for the Scheduled Instance.
siTermEndDate :: Lens' ScheduledInstance (Maybe UTCTime)
siTermEndDate = lens _siTermEndDate (\s a -> s {_siTermEndDate = a}) . mapping _Time

-- | The time for the next schedule to start.
siNextSlotStartTime :: Lens' ScheduledInstance (Maybe UTCTime)
siNextSlotStartTime = lens _siNextSlotStartTime (\s a -> s {_siNextSlotStartTime = a}) . mapping _Time

-- | The network platform (@EC2-Classic@ or @EC2-VPC@ ).
siNetworkPlatform :: Lens' ScheduledInstance (Maybe Text)
siNetworkPlatform = lens _siNetworkPlatform (\s a -> s {_siNetworkPlatform = a})

instance FromXML ScheduledInstance where
  parseXML x =
    ScheduledInstance'
      <$> (x .@? "previousSlotEndTime")
      <*> (x .@? "platform")
      <*> (x .@? "termStartDate")
      <*> (x .@? "instanceCount")
      <*> (x .@? "scheduledInstanceId")
      <*> (x .@? "hourlyPrice")
      <*> (x .@? "createDate")
      <*> (x .@? "slotDurationInHours")
      <*> (x .@? "totalScheduledInstanceHours")
      <*> (x .@? "instanceType")
      <*> (x .@? "recurrence")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "termEndDate")
      <*> (x .@? "nextSlotStartTime")
      <*> (x .@? "networkPlatform")

instance Hashable ScheduledInstance

instance NFData ScheduledInstance
