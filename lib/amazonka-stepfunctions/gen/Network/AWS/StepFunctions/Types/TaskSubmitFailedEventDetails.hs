{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a task that failed to submit during an execution.
--
--
--
-- /See:/ 'taskSubmitFailedEventDetails' smart constructor.
data TaskSubmitFailedEventDetails = TaskSubmitFailedEventDetails'
  { _tsfedError ::
      !(Maybe (Sensitive Text)),
    _tsfedCause ::
      !(Maybe (Sensitive Text)),
    _tsfedResourceType :: !Text,
    _tsfedResource :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskSubmitFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsfedError' - The error code of the failure.
--
-- * 'tsfedCause' - A more detailed explanation of the cause of the failure.
--
-- * 'tsfedResourceType' - The action of the resource called by a task state.
--
-- * 'tsfedResource' - The service name of the resource in a task state.
taskSubmitFailedEventDetails ::
  -- | 'tsfedResourceType'
  Text ->
  -- | 'tsfedResource'
  Text ->
  TaskSubmitFailedEventDetails
taskSubmitFailedEventDetails pResourceType_ pResource_ =
  TaskSubmitFailedEventDetails'
    { _tsfedError = Nothing,
      _tsfedCause = Nothing,
      _tsfedResourceType = pResourceType_,
      _tsfedResource = pResource_
    }

-- | The error code of the failure.
tsfedError :: Lens' TaskSubmitFailedEventDetails (Maybe Text)
tsfedError = lens _tsfedError (\s a -> s {_tsfedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
tsfedCause :: Lens' TaskSubmitFailedEventDetails (Maybe Text)
tsfedCause = lens _tsfedCause (\s a -> s {_tsfedCause = a}) . mapping _Sensitive

-- | The action of the resource called by a task state.
tsfedResourceType :: Lens' TaskSubmitFailedEventDetails Text
tsfedResourceType = lens _tsfedResourceType (\s a -> s {_tsfedResourceType = a})

-- | The service name of the resource in a task state.
tsfedResource :: Lens' TaskSubmitFailedEventDetails Text
tsfedResource = lens _tsfedResource (\s a -> s {_tsfedResource = a})

instance FromJSON TaskSubmitFailedEventDetails where
  parseJSON =
    withObject
      "TaskSubmitFailedEventDetails"
      ( \x ->
          TaskSubmitFailedEventDetails'
            <$> (x .:? "error")
            <*> (x .:? "cause")
            <*> (x .: "resourceType")
            <*> (x .: "resource")
      )

instance Hashable TaskSubmitFailedEventDetails

instance NFData TaskSubmitFailedEventDetails
