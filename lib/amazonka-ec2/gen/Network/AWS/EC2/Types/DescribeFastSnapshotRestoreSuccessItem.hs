{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DescribeFastSnapshotRestoreSuccessItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes fast snapshot restores for a snapshot.
--
--
--
-- /See:/ 'describeFastSnapshotRestoreSuccessItem' smart constructor.
data DescribeFastSnapshotRestoreSuccessItem = DescribeFastSnapshotRestoreSuccessItem'
  { _dfsrsiDisablingTime ::
      !( Maybe
           ISO8601
       ),
    _dfsrsiState ::
      !( Maybe
           FastSnapshotRestoreStateCode
       ),
    _dfsrsiOwnerAlias ::
      !(Maybe Text),
    _dfsrsiDisabledTime ::
      !( Maybe
           ISO8601
       ),
    _dfsrsiEnabledTime ::
      !( Maybe
           ISO8601
       ),
    _dfsrsiOptimizingTime ::
      !( Maybe
           ISO8601
       ),
    _dfsrsiOwnerId ::
      !(Maybe Text),
    _dfsrsiStateTransitionReason ::
      !(Maybe Text),
    _dfsrsiAvailabilityZone ::
      !(Maybe Text),
    _dfsrsiSnapshotId ::
      !(Maybe Text),
    _dfsrsiEnablingTime ::
      !( Maybe
           ISO8601
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFastSnapshotRestoreSuccessItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrsiDisablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
--
-- * 'dfsrsiState' - The state of fast snapshot restores.
--
-- * 'dfsrsiOwnerAlias' - The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
--
-- * 'dfsrsiDisabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
--
-- * 'dfsrsiEnabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
--
-- * 'dfsrsiOptimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
--
-- * 'dfsrsiOwnerId' - The ID of the AWS account that enabled fast snapshot restores on the snapshot.
--
-- * 'dfsrsiStateTransitionReason' - The reason for the state transition. The possible values are as follows:     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
--
-- * 'dfsrsiAvailabilityZone' - The Availability Zone.
--
-- * 'dfsrsiSnapshotId' - The ID of the snapshot.
--
-- * 'dfsrsiEnablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
describeFastSnapshotRestoreSuccessItem ::
  DescribeFastSnapshotRestoreSuccessItem
describeFastSnapshotRestoreSuccessItem =
  DescribeFastSnapshotRestoreSuccessItem'
    { _dfsrsiDisablingTime =
        Nothing,
      _dfsrsiState = Nothing,
      _dfsrsiOwnerAlias = Nothing,
      _dfsrsiDisabledTime = Nothing,
      _dfsrsiEnabledTime = Nothing,
      _dfsrsiOptimizingTime = Nothing,
      _dfsrsiOwnerId = Nothing,
      _dfsrsiStateTransitionReason = Nothing,
      _dfsrsiAvailabilityZone = Nothing,
      _dfsrsiSnapshotId = Nothing,
      _dfsrsiEnablingTime = Nothing
    }

-- | The time at which fast snapshot restores entered the @disabling@ state.
dfsrsiDisablingTime :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dfsrsiDisablingTime = lens _dfsrsiDisablingTime (\s a -> s {_dfsrsiDisablingTime = a}) . mapping _Time

-- | The state of fast snapshot restores.
dfsrsiState :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe FastSnapshotRestoreStateCode)
dfsrsiState = lens _dfsrsiState (\s a -> s {_dfsrsiState = a})

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
dfsrsiOwnerAlias :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe Text)
dfsrsiOwnerAlias = lens _dfsrsiOwnerAlias (\s a -> s {_dfsrsiOwnerAlias = a})

-- | The time at which fast snapshot restores entered the @disabled@ state.
dfsrsiDisabledTime :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dfsrsiDisabledTime = lens _dfsrsiDisabledTime (\s a -> s {_dfsrsiDisabledTime = a}) . mapping _Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
dfsrsiEnabledTime :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dfsrsiEnabledTime = lens _dfsrsiEnabledTime (\s a -> s {_dfsrsiEnabledTime = a}) . mapping _Time

-- | The time at which fast snapshot restores entered the @optimizing@ state.
dfsrsiOptimizingTime :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dfsrsiOptimizingTime = lens _dfsrsiOptimizingTime (\s a -> s {_dfsrsiOptimizingTime = a}) . mapping _Time

-- | The ID of the AWS account that enabled fast snapshot restores on the snapshot.
dfsrsiOwnerId :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe Text)
dfsrsiOwnerId = lens _dfsrsiOwnerId (\s a -> s {_dfsrsiOwnerId = a})

-- | The reason for the state transition. The possible values are as follows:     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
dfsrsiStateTransitionReason :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe Text)
dfsrsiStateTransitionReason = lens _dfsrsiStateTransitionReason (\s a -> s {_dfsrsiStateTransitionReason = a})

-- | The Availability Zone.
dfsrsiAvailabilityZone :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe Text)
dfsrsiAvailabilityZone = lens _dfsrsiAvailabilityZone (\s a -> s {_dfsrsiAvailabilityZone = a})

-- | The ID of the snapshot.
dfsrsiSnapshotId :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe Text)
dfsrsiSnapshotId = lens _dfsrsiSnapshotId (\s a -> s {_dfsrsiSnapshotId = a})

-- | The time at which fast snapshot restores entered the @enabling@ state.
dfsrsiEnablingTime :: Lens' DescribeFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dfsrsiEnablingTime = lens _dfsrsiEnablingTime (\s a -> s {_dfsrsiEnablingTime = a}) . mapping _Time

instance FromXML DescribeFastSnapshotRestoreSuccessItem where
  parseXML x =
    DescribeFastSnapshotRestoreSuccessItem'
      <$> (x .@? "disablingTime")
      <*> (x .@? "state")
      <*> (x .@? "ownerAlias")
      <*> (x .@? "disabledTime")
      <*> (x .@? "enabledTime")
      <*> (x .@? "optimizingTime")
      <*> (x .@? "ownerId")
      <*> (x .@? "stateTransitionReason")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "snapshotId")
      <*> (x .@? "enablingTime")

instance Hashable DescribeFastSnapshotRestoreSuccessItem

instance NFData DescribeFastSnapshotRestoreSuccessItem
