{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a task submitted to a resource .
--
--
--
-- /See:/ 'taskSubmittedEventDetails' smart constructor.
data TaskSubmittedEventDetails = TaskSubmittedEventDetails'
  { _tOutput ::
      !(Maybe (Sensitive Text)),
    _tOutputDetails ::
      !( Maybe
           HistoryEventExecutionDataDetails
       ),
    _tResourceType :: !Text,
    _tResource :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskSubmittedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tOutput' - The response from a resource when a task has started. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'tOutputDetails' - Contains details about the output of an execution history event.
--
-- * 'tResourceType' - The action of the resource called by a task state.
--
-- * 'tResource' - The service name of the resource in a task state.
taskSubmittedEventDetails ::
  -- | 'tResourceType'
  Text ->
  -- | 'tResource'
  Text ->
  TaskSubmittedEventDetails
taskSubmittedEventDetails pResourceType_ pResource_ =
  TaskSubmittedEventDetails'
    { _tOutput = Nothing,
      _tOutputDetails = Nothing,
      _tResourceType = pResourceType_,
      _tResource = pResource_
    }

-- | The response from a resource when a task has started. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
tOutput :: Lens' TaskSubmittedEventDetails (Maybe Text)
tOutput = lens _tOutput (\s a -> s {_tOutput = a}) . mapping _Sensitive

-- | Contains details about the output of an execution history event.
tOutputDetails :: Lens' TaskSubmittedEventDetails (Maybe HistoryEventExecutionDataDetails)
tOutputDetails = lens _tOutputDetails (\s a -> s {_tOutputDetails = a})

-- | The action of the resource called by a task state.
tResourceType :: Lens' TaskSubmittedEventDetails Text
tResourceType = lens _tResourceType (\s a -> s {_tResourceType = a})

-- | The service name of the resource in a task state.
tResource :: Lens' TaskSubmittedEventDetails Text
tResource = lens _tResource (\s a -> s {_tResource = a})

instance FromJSON TaskSubmittedEventDetails where
  parseJSON =
    withObject
      "TaskSubmittedEventDetails"
      ( \x ->
          TaskSubmittedEventDetails'
            <$> (x .:? "output")
            <*> (x .:? "outputDetails")
            <*> (x .: "resourceType")
            <*> (x .: "resource")
      )

instance Hashable TaskSubmittedEventDetails

instance NFData TaskSubmittedEventDetails
