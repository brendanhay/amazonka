{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskSucceededEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the successful completion of a task state.
--
--
--
-- /See:/ 'taskSucceededEventDetails' smart constructor.
data TaskSucceededEventDetails = TaskSucceededEventDetails'
  { _tsedsOutput ::
      !(Maybe (Sensitive Text)),
    _tsedsOutputDetails ::
      !( Maybe
           HistoryEventExecutionDataDetails
       ),
    _tsedsResourceType :: !Text,
    _tsedsResource :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskSucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsedsOutput' - The full JSON response from a resource when a task has succeeded. This response becomes the output of the related task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'tsedsOutputDetails' - Contains details about the output of an execution history event.
--
-- * 'tsedsResourceType' - The action of the resource called by a task state.
--
-- * 'tsedsResource' - The service name of the resource in a task state.
taskSucceededEventDetails ::
  -- | 'tsedsResourceType'
  Text ->
  -- | 'tsedsResource'
  Text ->
  TaskSucceededEventDetails
taskSucceededEventDetails pResourceType_ pResource_ =
  TaskSucceededEventDetails'
    { _tsedsOutput = Nothing,
      _tsedsOutputDetails = Nothing,
      _tsedsResourceType = pResourceType_,
      _tsedsResource = pResource_
    }

-- | The full JSON response from a resource when a task has succeeded. This response becomes the output of the related task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
tsedsOutput :: Lens' TaskSucceededEventDetails (Maybe Text)
tsedsOutput = lens _tsedsOutput (\s a -> s {_tsedsOutput = a}) . mapping _Sensitive

-- | Contains details about the output of an execution history event.
tsedsOutputDetails :: Lens' TaskSucceededEventDetails (Maybe HistoryEventExecutionDataDetails)
tsedsOutputDetails = lens _tsedsOutputDetails (\s a -> s {_tsedsOutputDetails = a})

-- | The action of the resource called by a task state.
tsedsResourceType :: Lens' TaskSucceededEventDetails Text
tsedsResourceType = lens _tsedsResourceType (\s a -> s {_tsedsResourceType = a})

-- | The service name of the resource in a task state.
tsedsResource :: Lens' TaskSucceededEventDetails Text
tsedsResource = lens _tsedsResource (\s a -> s {_tsedsResource = a})

instance FromJSON TaskSucceededEventDetails where
  parseJSON =
    withObject
      "TaskSucceededEventDetails"
      ( \x ->
          TaskSucceededEventDetails'
            <$> (x .:? "output")
            <*> (x .:? "outputDetails")
            <*> (x .: "resourceType")
            <*> (x .: "resource")
      )

instance Hashable TaskSucceededEventDetails

instance NFData TaskSucceededEventDetails
