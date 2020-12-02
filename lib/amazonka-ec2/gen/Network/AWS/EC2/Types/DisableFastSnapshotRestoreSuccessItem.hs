{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes fast snapshot restores that were successfully disabled.
--
--
--
-- /See:/ 'disableFastSnapshotRestoreSuccessItem' smart constructor.
data DisableFastSnapshotRestoreSuccessItem = DisableFastSnapshotRestoreSuccessItem'
  { _dDisablingTime ::
      !( Maybe
           ISO8601
       ),
    _dState ::
      !( Maybe
           FastSnapshotRestoreStateCode
       ),
    _dOwnerAlias ::
      !(Maybe Text),
    _dDisabledTime ::
      !( Maybe
           ISO8601
       ),
    _dEnabledTime ::
      !( Maybe
           ISO8601
       ),
    _dOptimizingTime ::
      !( Maybe
           ISO8601
       ),
    _dOwnerId ::
      !(Maybe Text),
    _dStateTransitionReason ::
      !(Maybe Text),
    _dAvailabilityZone ::
      !(Maybe Text),
    _dSnapshotId ::
      !(Maybe Text),
    _dEnablingTime ::
      !( Maybe
           ISO8601
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableFastSnapshotRestoreSuccessItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDisablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
--
-- * 'dState' - The state of fast snapshot restores for the snapshot.
--
-- * 'dOwnerAlias' - The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
--
-- * 'dDisabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
--
-- * 'dEnabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
--
-- * 'dOptimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
--
-- * 'dOwnerId' - The ID of the AWS account that enabled fast snapshot restores on the snapshot.
--
-- * 'dStateTransitionReason' - The reason for the state transition. The possible values are as follows:     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
--
-- * 'dAvailabilityZone' - The Availability Zone.
--
-- * 'dSnapshotId' - The ID of the snapshot.
--
-- * 'dEnablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
disableFastSnapshotRestoreSuccessItem ::
  DisableFastSnapshotRestoreSuccessItem
disableFastSnapshotRestoreSuccessItem =
  DisableFastSnapshotRestoreSuccessItem'
    { _dDisablingTime = Nothing,
      _dState = Nothing,
      _dOwnerAlias = Nothing,
      _dDisabledTime = Nothing,
      _dEnabledTime = Nothing,
      _dOptimizingTime = Nothing,
      _dOwnerId = Nothing,
      _dStateTransitionReason = Nothing,
      _dAvailabilityZone = Nothing,
      _dSnapshotId = Nothing,
      _dEnablingTime = Nothing
    }

-- | The time at which fast snapshot restores entered the @disabling@ state.
dDisablingTime :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dDisablingTime = lens _dDisablingTime (\s a -> s {_dDisablingTime = a}) . mapping _Time

-- | The state of fast snapshot restores for the snapshot.
dState :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe FastSnapshotRestoreStateCode)
dState = lens _dState (\s a -> s {_dState = a})

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
dOwnerAlias :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe Text)
dOwnerAlias = lens _dOwnerAlias (\s a -> s {_dOwnerAlias = a})

-- | The time at which fast snapshot restores entered the @disabled@ state.
dDisabledTime :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dDisabledTime = lens _dDisabledTime (\s a -> s {_dDisabledTime = a}) . mapping _Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
dEnabledTime :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dEnabledTime = lens _dEnabledTime (\s a -> s {_dEnabledTime = a}) . mapping _Time

-- | The time at which fast snapshot restores entered the @optimizing@ state.
dOptimizingTime :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dOptimizingTime = lens _dOptimizingTime (\s a -> s {_dOptimizingTime = a}) . mapping _Time

-- | The ID of the AWS account that enabled fast snapshot restores on the snapshot.
dOwnerId :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe Text)
dOwnerId = lens _dOwnerId (\s a -> s {_dOwnerId = a})

-- | The reason for the state transition. The possible values are as follows:     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
dStateTransitionReason :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe Text)
dStateTransitionReason = lens _dStateTransitionReason (\s a -> s {_dStateTransitionReason = a})

-- | The Availability Zone.
dAvailabilityZone :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe Text)
dAvailabilityZone = lens _dAvailabilityZone (\s a -> s {_dAvailabilityZone = a})

-- | The ID of the snapshot.
dSnapshotId :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe Text)
dSnapshotId = lens _dSnapshotId (\s a -> s {_dSnapshotId = a})

-- | The time at which fast snapshot restores entered the @enabling@ state.
dEnablingTime :: Lens' DisableFastSnapshotRestoreSuccessItem (Maybe UTCTime)
dEnablingTime = lens _dEnablingTime (\s a -> s {_dEnablingTime = a}) . mapping _Time

instance FromXML DisableFastSnapshotRestoreSuccessItem where
  parseXML x =
    DisableFastSnapshotRestoreSuccessItem'
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

instance Hashable DisableFastSnapshotRestoreSuccessItem

instance NFData DisableFastSnapshotRestoreSuccessItem
