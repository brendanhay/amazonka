{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatus where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceState
import Network.AWS.EC2.Types.InstanceStatusEvent
import Network.AWS.EC2.Types.InstanceStatusSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the status of an instance.
--
--
--
-- /See:/ 'instanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { _iInstanceId ::
      !(Maybe Text),
    _iOutpostARN :: !(Maybe Text),
    _iSystemStatus :: !(Maybe InstanceStatusSummary),
    _iEvents :: !(Maybe [InstanceStatusEvent]),
    _iAvailabilityZone :: !(Maybe Text),
    _iInstanceStatus :: !(Maybe InstanceStatusSummary),
    _iInstanceState :: !(Maybe InstanceState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iInstanceId' - The ID of the instance.
--
-- * 'iOutpostARN' - The Amazon Resource Name (ARN) of the Outpost.
--
-- * 'iSystemStatus' - Reports impaired functionality that stems from issues related to the systems that support an instance, such as hardware failures and network connectivity problems.
--
-- * 'iEvents' - Any scheduled events associated with the instance.
--
-- * 'iAvailabilityZone' - The Availability Zone of the instance.
--
-- * 'iInstanceStatus' - Reports impaired functionality that stems from issues internal to the instance, such as impaired reachability.
--
-- * 'iInstanceState' - The intended state of the instance. 'DescribeInstanceStatus' requires that an instance be in the @running@ state.
instanceStatus ::
  InstanceStatus
instanceStatus =
  InstanceStatus'
    { _iInstanceId = Nothing,
      _iOutpostARN = Nothing,
      _iSystemStatus = Nothing,
      _iEvents = Nothing,
      _iAvailabilityZone = Nothing,
      _iInstanceStatus = Nothing,
      _iInstanceState = Nothing
    }

-- | The ID of the instance.
iInstanceId :: Lens' InstanceStatus (Maybe Text)
iInstanceId = lens _iInstanceId (\s a -> s {_iInstanceId = a})

-- | The Amazon Resource Name (ARN) of the Outpost.
iOutpostARN :: Lens' InstanceStatus (Maybe Text)
iOutpostARN = lens _iOutpostARN (\s a -> s {_iOutpostARN = a})

-- | Reports impaired functionality that stems from issues related to the systems that support an instance, such as hardware failures and network connectivity problems.
iSystemStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
iSystemStatus = lens _iSystemStatus (\s a -> s {_iSystemStatus = a})

-- | Any scheduled events associated with the instance.
iEvents :: Lens' InstanceStatus [InstanceStatusEvent]
iEvents = lens _iEvents (\s a -> s {_iEvents = a}) . _Default . _Coerce

-- | The Availability Zone of the instance.
iAvailabilityZone :: Lens' InstanceStatus (Maybe Text)
iAvailabilityZone = lens _iAvailabilityZone (\s a -> s {_iAvailabilityZone = a})

-- | Reports impaired functionality that stems from issues internal to the instance, such as impaired reachability.
iInstanceStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
iInstanceStatus = lens _iInstanceStatus (\s a -> s {_iInstanceStatus = a})

-- | The intended state of the instance. 'DescribeInstanceStatus' requires that an instance be in the @running@ state.
iInstanceState :: Lens' InstanceStatus (Maybe InstanceState)
iInstanceState = lens _iInstanceState (\s a -> s {_iInstanceState = a})

instance FromXML InstanceStatus where
  parseXML x =
    InstanceStatus'
      <$> (x .@? "instanceId")
      <*> (x .@? "outpostArn")
      <*> (x .@? "systemStatus")
      <*> (x .@? "eventsSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "availabilityZone")
      <*> (x .@? "instanceStatus")
      <*> (x .@? "instanceState")

instance Hashable InstanceStatus

instance NFData InstanceStatus
