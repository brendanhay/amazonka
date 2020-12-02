{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskScheduledEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskScheduledEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a task scheduled during an execution.
--
--
--
-- /See:/ 'taskScheduledEventDetails' smart constructor.
data TaskScheduledEventDetails = TaskScheduledEventDetails'
  { _tasHeartbeatInSeconds ::
      !(Maybe Integer),
    _tasTimeoutInSeconds ::
      !(Maybe Integer),
    _tasResourceType :: !Text,
    _tasResource :: !Text,
    _tasRegion :: !Text,
    _tasParameters :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskScheduledEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tasHeartbeatInSeconds' - The maximum allowed duration between two heartbeats for the task.
--
-- * 'tasTimeoutInSeconds' - The maximum allowed duration of the task.
--
-- * 'tasResourceType' - The action of the resource called by a task state.
--
-- * 'tasResource' - The service name of the resource in a task state.
--
-- * 'tasRegion' - The region of the scheduled task
--
-- * 'tasParameters' - The JSON data passed to the resource referenced in a task state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
taskScheduledEventDetails ::
  -- | 'tasResourceType'
  Text ->
  -- | 'tasResource'
  Text ->
  -- | 'tasRegion'
  Text ->
  -- | 'tasParameters'
  Text ->
  TaskScheduledEventDetails
taskScheduledEventDetails
  pResourceType_
  pResource_
  pRegion_
  pParameters_ =
    TaskScheduledEventDetails'
      { _tasHeartbeatInSeconds = Nothing,
        _tasTimeoutInSeconds = Nothing,
        _tasResourceType = pResourceType_,
        _tasResource = pResource_,
        _tasRegion = pRegion_,
        _tasParameters = _Sensitive # pParameters_
      }

-- | The maximum allowed duration between two heartbeats for the task.
tasHeartbeatInSeconds :: Lens' TaskScheduledEventDetails (Maybe Integer)
tasHeartbeatInSeconds = lens _tasHeartbeatInSeconds (\s a -> s {_tasHeartbeatInSeconds = a})

-- | The maximum allowed duration of the task.
tasTimeoutInSeconds :: Lens' TaskScheduledEventDetails (Maybe Integer)
tasTimeoutInSeconds = lens _tasTimeoutInSeconds (\s a -> s {_tasTimeoutInSeconds = a})

-- | The action of the resource called by a task state.
tasResourceType :: Lens' TaskScheduledEventDetails Text
tasResourceType = lens _tasResourceType (\s a -> s {_tasResourceType = a})

-- | The service name of the resource in a task state.
tasResource :: Lens' TaskScheduledEventDetails Text
tasResource = lens _tasResource (\s a -> s {_tasResource = a})

-- | The region of the scheduled task
tasRegion :: Lens' TaskScheduledEventDetails Text
tasRegion = lens _tasRegion (\s a -> s {_tasRegion = a})

-- | The JSON data passed to the resource referenced in a task state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
tasParameters :: Lens' TaskScheduledEventDetails Text
tasParameters = lens _tasParameters (\s a -> s {_tasParameters = a}) . _Sensitive

instance FromJSON TaskScheduledEventDetails where
  parseJSON =
    withObject
      "TaskScheduledEventDetails"
      ( \x ->
          TaskScheduledEventDetails'
            <$> (x .:? "heartbeatInSeconds")
            <*> (x .:? "timeoutInSeconds")
            <*> (x .: "resourceType")
            <*> (x .: "resource")
            <*> (x .: "region")
            <*> (x .: "parameters")
      )

instance Hashable TaskScheduledEventDetails

instance NFData TaskScheduledEventDetails
