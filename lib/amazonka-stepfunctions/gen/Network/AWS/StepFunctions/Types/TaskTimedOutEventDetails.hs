{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a resource timeout that occurred during an execution.
--
--
--
-- /See:/ 'taskTimedOutEventDetails' smart constructor.
data TaskTimedOutEventDetails = TaskTimedOutEventDetails'
  { _ttoedError ::
      !(Maybe (Sensitive Text)),
    _ttoedCause :: !(Maybe (Sensitive Text)),
    _ttoedResourceType :: !Text,
    _ttoedResource :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskTimedOutEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttoedError' - The error code of the failure.
--
-- * 'ttoedCause' - A more detailed explanation of the cause of the failure.
--
-- * 'ttoedResourceType' - The action of the resource called by a task state.
--
-- * 'ttoedResource' - The service name of the resource in a task state.
taskTimedOutEventDetails ::
  -- | 'ttoedResourceType'
  Text ->
  -- | 'ttoedResource'
  Text ->
  TaskTimedOutEventDetails
taskTimedOutEventDetails pResourceType_ pResource_ =
  TaskTimedOutEventDetails'
    { _ttoedError = Nothing,
      _ttoedCause = Nothing,
      _ttoedResourceType = pResourceType_,
      _ttoedResource = pResource_
    }

-- | The error code of the failure.
ttoedError :: Lens' TaskTimedOutEventDetails (Maybe Text)
ttoedError = lens _ttoedError (\s a -> s {_ttoedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
ttoedCause :: Lens' TaskTimedOutEventDetails (Maybe Text)
ttoedCause = lens _ttoedCause (\s a -> s {_ttoedCause = a}) . mapping _Sensitive

-- | The action of the resource called by a task state.
ttoedResourceType :: Lens' TaskTimedOutEventDetails Text
ttoedResourceType = lens _ttoedResourceType (\s a -> s {_ttoedResourceType = a})

-- | The service name of the resource in a task state.
ttoedResource :: Lens' TaskTimedOutEventDetails Text
ttoedResource = lens _ttoedResource (\s a -> s {_ttoedResource = a})

instance FromJSON TaskTimedOutEventDetails where
  parseJSON =
    withObject
      "TaskTimedOutEventDetails"
      ( \x ->
          TaskTimedOutEventDetails'
            <$> (x .:? "error")
            <*> (x .:? "cause")
            <*> (x .: "resourceType")
            <*> (x .: "resource")
      )

instance Hashable TaskTimedOutEventDetails

instance NFData TaskTimedOutEventDetails
