{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BundleTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BundleTask where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BundleTaskError
import Network.AWS.EC2.Types.BundleTaskState
import Network.AWS.EC2.Types.Storage
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a bundle task.
--
--
--
-- /See:/ 'bundleTask' smart constructor.
data BundleTask = BundleTask'
  { _btBundleTaskError ::
      !(Maybe BundleTaskError),
    _btBundleId :: !Text,
    _btInstanceId :: !Text,
    _btProgress :: !Text,
    _btStartTime :: !ISO8601,
    _btState :: !BundleTaskState,
    _btStorage :: !Storage,
    _btUpdateTime :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BundleTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btBundleTaskError' - If the task fails, a description of the error.
--
-- * 'btBundleId' - The ID of the bundle task.
--
-- * 'btInstanceId' - The ID of the instance associated with this bundle task.
--
-- * 'btProgress' - The level of task completion, as a percent (for example, 20%).
--
-- * 'btStartTime' - The time this task started.
--
-- * 'btState' - The state of the task.
--
-- * 'btStorage' - The Amazon S3 storage locations.
--
-- * 'btUpdateTime' - The time of the most recent update for the task.
bundleTask ::
  -- | 'btBundleId'
  Text ->
  -- | 'btInstanceId'
  Text ->
  -- | 'btProgress'
  Text ->
  -- | 'btStartTime'
  UTCTime ->
  -- | 'btState'
  BundleTaskState ->
  -- | 'btStorage'
  Storage ->
  -- | 'btUpdateTime'
  UTCTime ->
  BundleTask
bundleTask
  pBundleId_
  pInstanceId_
  pProgress_
  pStartTime_
  pState_
  pStorage_
  pUpdateTime_ =
    BundleTask'
      { _btBundleTaskError = Nothing,
        _btBundleId = pBundleId_,
        _btInstanceId = pInstanceId_,
        _btProgress = pProgress_,
        _btStartTime = _Time # pStartTime_,
        _btState = pState_,
        _btStorage = pStorage_,
        _btUpdateTime = _Time # pUpdateTime_
      }

-- | If the task fails, a description of the error.
btBundleTaskError :: Lens' BundleTask (Maybe BundleTaskError)
btBundleTaskError = lens _btBundleTaskError (\s a -> s {_btBundleTaskError = a})

-- | The ID of the bundle task.
btBundleId :: Lens' BundleTask Text
btBundleId = lens _btBundleId (\s a -> s {_btBundleId = a})

-- | The ID of the instance associated with this bundle task.
btInstanceId :: Lens' BundleTask Text
btInstanceId = lens _btInstanceId (\s a -> s {_btInstanceId = a})

-- | The level of task completion, as a percent (for example, 20%).
btProgress :: Lens' BundleTask Text
btProgress = lens _btProgress (\s a -> s {_btProgress = a})

-- | The time this task started.
btStartTime :: Lens' BundleTask UTCTime
btStartTime = lens _btStartTime (\s a -> s {_btStartTime = a}) . _Time

-- | The state of the task.
btState :: Lens' BundleTask BundleTaskState
btState = lens _btState (\s a -> s {_btState = a})

-- | The Amazon S3 storage locations.
btStorage :: Lens' BundleTask Storage
btStorage = lens _btStorage (\s a -> s {_btStorage = a})

-- | The time of the most recent update for the task.
btUpdateTime :: Lens' BundleTask UTCTime
btUpdateTime = lens _btUpdateTime (\s a -> s {_btUpdateTime = a}) . _Time

instance FromXML BundleTask where
  parseXML x =
    BundleTask'
      <$> (x .@? "error")
      <*> (x .@ "bundleId")
      <*> (x .@ "instanceId")
      <*> (x .@ "progress")
      <*> (x .@ "startTime")
      <*> (x .@ "state")
      <*> (x .@ "storage")
      <*> (x .@ "updateTime")

instance Hashable BundleTask

instance NFData BundleTask
