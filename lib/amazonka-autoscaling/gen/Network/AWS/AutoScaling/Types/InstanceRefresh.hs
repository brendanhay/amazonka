{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceRefresh
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceRefresh where

import Network.AWS.AutoScaling.Types.InstanceRefreshStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an instance refresh for an Auto Scaling group.
--
--
--
-- /See:/ 'instanceRefresh' smart constructor.
data InstanceRefresh = InstanceRefresh'
  { _irStatus ::
      !(Maybe InstanceRefreshStatus),
    _irStartTime :: !(Maybe ISO8601),
    _irInstancesToUpdate :: !(Maybe Nat),
    _irPercentageComplete :: !(Maybe Nat),
    _irAutoScalingGroupName :: !(Maybe Text),
    _irEndTime :: !(Maybe ISO8601),
    _irStatusReason :: !(Maybe Text),
    _irInstanceRefreshId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceRefresh' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irStatus' - The current status for the instance refresh operation:     * @Pending@ - The request was created, but the operation has not started.     * @InProgress@ - The operation is in progress.     * @Successful@ - The operation completed successfully.     * @Failed@ - The operation failed to complete. You can troubleshoot using the status reason and the scaling activities.      * @Cancelling@ - An ongoing operation is being cancelled. Cancellation does not roll back any replacements that have already been completed, but it prevents new replacements from being started.      * @Cancelled@ - The operation is cancelled.
--
-- * 'irStartTime' - The date and time at which the instance refresh began.
--
-- * 'irInstancesToUpdate' - The number of instances remaining to update before the instance refresh is complete.
--
-- * 'irPercentageComplete' - The percentage of the instance refresh that is complete. For each instance replacement, Amazon EC2 Auto Scaling tracks the instance's health status and warm-up time. When the instance's health status changes to healthy and the specified warm-up time passes, the instance is considered updated and added to the percentage complete.
--
-- * 'irAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'irEndTime' - The date and time at which the instance refresh ended.
--
-- * 'irStatusReason' - Provides more details about the current status of the instance refresh.
--
-- * 'irInstanceRefreshId' - The instance refresh ID.
instanceRefresh ::
  InstanceRefresh
instanceRefresh =
  InstanceRefresh'
    { _irStatus = Nothing,
      _irStartTime = Nothing,
      _irInstancesToUpdate = Nothing,
      _irPercentageComplete = Nothing,
      _irAutoScalingGroupName = Nothing,
      _irEndTime = Nothing,
      _irStatusReason = Nothing,
      _irInstanceRefreshId = Nothing
    }

-- | The current status for the instance refresh operation:     * @Pending@ - The request was created, but the operation has not started.     * @InProgress@ - The operation is in progress.     * @Successful@ - The operation completed successfully.     * @Failed@ - The operation failed to complete. You can troubleshoot using the status reason and the scaling activities.      * @Cancelling@ - An ongoing operation is being cancelled. Cancellation does not roll back any replacements that have already been completed, but it prevents new replacements from being started.      * @Cancelled@ - The operation is cancelled.
irStatus :: Lens' InstanceRefresh (Maybe InstanceRefreshStatus)
irStatus = lens _irStatus (\s a -> s {_irStatus = a})

-- | The date and time at which the instance refresh began.
irStartTime :: Lens' InstanceRefresh (Maybe UTCTime)
irStartTime = lens _irStartTime (\s a -> s {_irStartTime = a}) . mapping _Time

-- | The number of instances remaining to update before the instance refresh is complete.
irInstancesToUpdate :: Lens' InstanceRefresh (Maybe Natural)
irInstancesToUpdate = lens _irInstancesToUpdate (\s a -> s {_irInstancesToUpdate = a}) . mapping _Nat

-- | The percentage of the instance refresh that is complete. For each instance replacement, Amazon EC2 Auto Scaling tracks the instance's health status and warm-up time. When the instance's health status changes to healthy and the specified warm-up time passes, the instance is considered updated and added to the percentage complete.
irPercentageComplete :: Lens' InstanceRefresh (Maybe Natural)
irPercentageComplete = lens _irPercentageComplete (\s a -> s {_irPercentageComplete = a}) . mapping _Nat

-- | The name of the Auto Scaling group.
irAutoScalingGroupName :: Lens' InstanceRefresh (Maybe Text)
irAutoScalingGroupName = lens _irAutoScalingGroupName (\s a -> s {_irAutoScalingGroupName = a})

-- | The date and time at which the instance refresh ended.
irEndTime :: Lens' InstanceRefresh (Maybe UTCTime)
irEndTime = lens _irEndTime (\s a -> s {_irEndTime = a}) . mapping _Time

-- | Provides more details about the current status of the instance refresh.
irStatusReason :: Lens' InstanceRefresh (Maybe Text)
irStatusReason = lens _irStatusReason (\s a -> s {_irStatusReason = a})

-- | The instance refresh ID.
irInstanceRefreshId :: Lens' InstanceRefresh (Maybe Text)
irInstanceRefreshId = lens _irInstanceRefreshId (\s a -> s {_irInstanceRefreshId = a})

instance FromXML InstanceRefresh where
  parseXML x =
    InstanceRefresh'
      <$> (x .@? "Status")
      <*> (x .@? "StartTime")
      <*> (x .@? "InstancesToUpdate")
      <*> (x .@? "PercentageComplete")
      <*> (x .@? "AutoScalingGroupName")
      <*> (x .@? "EndTime")
      <*> (x .@? "StatusReason")
      <*> (x .@? "InstanceRefreshId")

instance Hashable InstanceRefresh

instance NFData InstanceRefresh
