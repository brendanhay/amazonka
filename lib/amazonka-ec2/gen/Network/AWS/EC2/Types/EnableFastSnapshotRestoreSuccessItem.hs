{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes fast snapshot restores that were successfully enabled.
--
--
--
-- /See:/ 'enableFastSnapshotRestoreSuccessItem' smart constructor.
data EnableFastSnapshotRestoreSuccessItem = EnableFastSnapshotRestoreSuccessItem'
  { _efsrsiDisablingTime ::
      !(Maybe ISO8601),
    _efsrsiState ::
      !( Maybe
           FastSnapshotRestoreStateCode
       ),
    _efsrsiOwnerAlias ::
      !(Maybe Text),
    _efsrsiDisabledTime ::
      !(Maybe ISO8601),
    _efsrsiEnabledTime ::
      !(Maybe ISO8601),
    _efsrsiOptimizingTime ::
      !(Maybe ISO8601),
    _efsrsiOwnerId ::
      !(Maybe Text),
    _efsrsiStateTransitionReason ::
      !(Maybe Text),
    _efsrsiAvailabilityZone ::
      !(Maybe Text),
    _efsrsiSnapshotId ::
      !(Maybe Text),
    _efsrsiEnablingTime ::
      !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableFastSnapshotRestoreSuccessItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efsrsiDisablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
--
-- * 'efsrsiState' - The state of fast snapshot restores.
--
-- * 'efsrsiOwnerAlias' - The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
--
-- * 'efsrsiDisabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
--
-- * 'efsrsiEnabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
--
-- * 'efsrsiOptimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
--
-- * 'efsrsiOwnerId' - The ID of the AWS account that enabled fast snapshot restores on the snapshot.
--
-- * 'efsrsiStateTransitionReason' - The reason for the state transition. The possible values are as follows:     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
--
-- * 'efsrsiAvailabilityZone' - The Availability Zone.
--
-- * 'efsrsiSnapshotId' - The ID of the snapshot.
--
-- * 'efsrsiEnablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
enableFastSnapshotRestoreSuccessItem ::
  EnableFastSnapshotRestoreSuccessItem
enableFastSnapshotRestoreSuccessItem =
  EnableFastSnapshotRestoreSuccessItem'
    { _efsrsiDisablingTime =
        Nothing,
      _efsrsiState = Nothing,
      _efsrsiOwnerAlias = Nothing,
      _efsrsiDisabledTime = Nothing,
      _efsrsiEnabledTime = Nothing,
      _efsrsiOptimizingTime = Nothing,
      _efsrsiOwnerId = Nothing,
      _efsrsiStateTransitionReason = Nothing,
      _efsrsiAvailabilityZone = Nothing,
      _efsrsiSnapshotId = Nothing,
      _efsrsiEnablingTime = Nothing
    }

-- | The time at which fast snapshot restores entered the @disabling@ state.
efsrsiDisablingTime :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
efsrsiDisablingTime = lens _efsrsiDisablingTime (\s a -> s {_efsrsiDisablingTime = a}) . mapping _Time

-- | The state of fast snapshot restores.
efsrsiState :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe FastSnapshotRestoreStateCode)
efsrsiState = lens _efsrsiState (\s a -> s {_efsrsiState = a})

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
efsrsiOwnerAlias :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe Text)
efsrsiOwnerAlias = lens _efsrsiOwnerAlias (\s a -> s {_efsrsiOwnerAlias = a})

-- | The time at which fast snapshot restores entered the @disabled@ state.
efsrsiDisabledTime :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
efsrsiDisabledTime = lens _efsrsiDisabledTime (\s a -> s {_efsrsiDisabledTime = a}) . mapping _Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
efsrsiEnabledTime :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
efsrsiEnabledTime = lens _efsrsiEnabledTime (\s a -> s {_efsrsiEnabledTime = a}) . mapping _Time

-- | The time at which fast snapshot restores entered the @optimizing@ state.
efsrsiOptimizingTime :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
efsrsiOptimizingTime = lens _efsrsiOptimizingTime (\s a -> s {_efsrsiOptimizingTime = a}) . mapping _Time

-- | The ID of the AWS account that enabled fast snapshot restores on the snapshot.
efsrsiOwnerId :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe Text)
efsrsiOwnerId = lens _efsrsiOwnerId (\s a -> s {_efsrsiOwnerId = a})

-- | The reason for the state transition. The possible values are as follows:     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
efsrsiStateTransitionReason :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe Text)
efsrsiStateTransitionReason = lens _efsrsiStateTransitionReason (\s a -> s {_efsrsiStateTransitionReason = a})

-- | The Availability Zone.
efsrsiAvailabilityZone :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe Text)
efsrsiAvailabilityZone = lens _efsrsiAvailabilityZone (\s a -> s {_efsrsiAvailabilityZone = a})

-- | The ID of the snapshot.
efsrsiSnapshotId :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe Text)
efsrsiSnapshotId = lens _efsrsiSnapshotId (\s a -> s {_efsrsiSnapshotId = a})

-- | The time at which fast snapshot restores entered the @enabling@ state.
efsrsiEnablingTime :: Lens' EnableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
efsrsiEnablingTime = lens _efsrsiEnablingTime (\s a -> s {_efsrsiEnablingTime = a}) . mapping _Time

instance FromXML EnableFastSnapshotRestoreSuccessItem where
  parseXML x =
    EnableFastSnapshotRestoreSuccessItem'
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

instance Hashable EnableFastSnapshotRestoreSuccessItem

instance NFData EnableFastSnapshotRestoreSuccessItem
