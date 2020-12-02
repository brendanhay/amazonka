{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstanceAvailability where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstanceRecurrence
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a schedule that is available for your Scheduled Instances.
--
--
--
-- /See:/ 'scheduledInstanceAvailability' smart constructor.
data ScheduledInstanceAvailability = ScheduledInstanceAvailability'
  { _siaMaxTermDurationInDays ::
      !(Maybe Int),
    _siaPlatform :: !(Maybe Text),
    _siaPurchaseToken ::
      !(Maybe Text),
    _siaHourlyPrice ::
      !(Maybe Text),
    _siaAvailableInstanceCount ::
      !(Maybe Int),
    _siaSlotDurationInHours ::
      !(Maybe Int),
    _siaTotalScheduledInstanceHours ::
      !(Maybe Int),
    _siaInstanceType ::
      !(Maybe Text),
    _siaRecurrence ::
      !( Maybe
           ScheduledInstanceRecurrence
       ),
    _siaAvailabilityZone ::
      !(Maybe Text),
    _siaMinTermDurationInDays ::
      !(Maybe Int),
    _siaFirstSlotStartTime ::
      !(Maybe ISO8601),
    _siaNetworkPlatform ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstanceAvailability' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siaMaxTermDurationInDays' - The maximum term. The only possible value is 365 days.
--
-- * 'siaPlatform' - The platform (@Linux/UNIX@ or @Windows@ ).
--
-- * 'siaPurchaseToken' - The purchase token. This token expires in two hours.
--
-- * 'siaHourlyPrice' - The hourly price for a single instance.
--
-- * 'siaAvailableInstanceCount' - The number of available instances.
--
-- * 'siaSlotDurationInHours' - The number of hours in the schedule.
--
-- * 'siaTotalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
--
-- * 'siaInstanceType' - The instance type. You can specify one of the C3, C4, M4, or R3 instance types.
--
-- * 'siaRecurrence' - The schedule recurrence.
--
-- * 'siaAvailabilityZone' - The Availability Zone.
--
-- * 'siaMinTermDurationInDays' - The minimum term. The only possible value is 365 days.
--
-- * 'siaFirstSlotStartTime' - The time period for the first schedule to start.
--
-- * 'siaNetworkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
scheduledInstanceAvailability ::
  ScheduledInstanceAvailability
scheduledInstanceAvailability =
  ScheduledInstanceAvailability'
    { _siaMaxTermDurationInDays =
        Nothing,
      _siaPlatform = Nothing,
      _siaPurchaseToken = Nothing,
      _siaHourlyPrice = Nothing,
      _siaAvailableInstanceCount = Nothing,
      _siaSlotDurationInHours = Nothing,
      _siaTotalScheduledInstanceHours = Nothing,
      _siaInstanceType = Nothing,
      _siaRecurrence = Nothing,
      _siaAvailabilityZone = Nothing,
      _siaMinTermDurationInDays = Nothing,
      _siaFirstSlotStartTime = Nothing,
      _siaNetworkPlatform = Nothing
    }

-- | The maximum term. The only possible value is 365 days.
siaMaxTermDurationInDays :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaMaxTermDurationInDays = lens _siaMaxTermDurationInDays (\s a -> s {_siaMaxTermDurationInDays = a})

-- | The platform (@Linux/UNIX@ or @Windows@ ).
siaPlatform :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaPlatform = lens _siaPlatform (\s a -> s {_siaPlatform = a})

-- | The purchase token. This token expires in two hours.
siaPurchaseToken :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaPurchaseToken = lens _siaPurchaseToken (\s a -> s {_siaPurchaseToken = a})

-- | The hourly price for a single instance.
siaHourlyPrice :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaHourlyPrice = lens _siaHourlyPrice (\s a -> s {_siaHourlyPrice = a})

-- | The number of available instances.
siaAvailableInstanceCount :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaAvailableInstanceCount = lens _siaAvailableInstanceCount (\s a -> s {_siaAvailableInstanceCount = a})

-- | The number of hours in the schedule.
siaSlotDurationInHours :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaSlotDurationInHours = lens _siaSlotDurationInHours (\s a -> s {_siaSlotDurationInHours = a})

-- | The total number of hours for a single instance for the entire term.
siaTotalScheduledInstanceHours :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaTotalScheduledInstanceHours = lens _siaTotalScheduledInstanceHours (\s a -> s {_siaTotalScheduledInstanceHours = a})

-- | The instance type. You can specify one of the C3, C4, M4, or R3 instance types.
siaInstanceType :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaInstanceType = lens _siaInstanceType (\s a -> s {_siaInstanceType = a})

-- | The schedule recurrence.
siaRecurrence :: Lens' ScheduledInstanceAvailability (Maybe ScheduledInstanceRecurrence)
siaRecurrence = lens _siaRecurrence (\s a -> s {_siaRecurrence = a})

-- | The Availability Zone.
siaAvailabilityZone :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaAvailabilityZone = lens _siaAvailabilityZone (\s a -> s {_siaAvailabilityZone = a})

-- | The minimum term. The only possible value is 365 days.
siaMinTermDurationInDays :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaMinTermDurationInDays = lens _siaMinTermDurationInDays (\s a -> s {_siaMinTermDurationInDays = a})

-- | The time period for the first schedule to start.
siaFirstSlotStartTime :: Lens' ScheduledInstanceAvailability (Maybe UTCTime)
siaFirstSlotStartTime = lens _siaFirstSlotStartTime (\s a -> s {_siaFirstSlotStartTime = a}) . mapping _Time

-- | The network platform (@EC2-Classic@ or @EC2-VPC@ ).
siaNetworkPlatform :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaNetworkPlatform = lens _siaNetworkPlatform (\s a -> s {_siaNetworkPlatform = a})

instance FromXML ScheduledInstanceAvailability where
  parseXML x =
    ScheduledInstanceAvailability'
      <$> (x .@? "maxTermDurationInDays")
      <*> (x .@? "platform")
      <*> (x .@? "purchaseToken")
      <*> (x .@? "hourlyPrice")
      <*> (x .@? "availableInstanceCount")
      <*> (x .@? "slotDurationInHours")
      <*> (x .@? "totalScheduledInstanceHours")
      <*> (x .@? "instanceType")
      <*> (x .@? "recurrence")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "minTermDurationInDays")
      <*> (x .@? "firstSlotStartTime")
      <*> (x .@? "networkPlatform")

instance Hashable ScheduledInstanceAvailability

instance NFData ScheduledInstanceAvailability
