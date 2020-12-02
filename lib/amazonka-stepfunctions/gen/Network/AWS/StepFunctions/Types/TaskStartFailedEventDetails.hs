{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a task that failed to start during an execution.
--
--
--
-- /See:/ 'taskStartFailedEventDetails' smart constructor.
data TaskStartFailedEventDetails = TaskStartFailedEventDetails'
  { _tsfedsError ::
      !(Maybe (Sensitive Text)),
    _tsfedsCause ::
      !(Maybe (Sensitive Text)),
    _tsfedsResourceType :: !Text,
    _tsfedsResource :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskStartFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsfedsError' - The error code of the failure.
--
-- * 'tsfedsCause' - A more detailed explanation of the cause of the failure.
--
-- * 'tsfedsResourceType' - The action of the resource called by a task state.
--
-- * 'tsfedsResource' - The service name of the resource in a task state.
taskStartFailedEventDetails ::
  -- | 'tsfedsResourceType'
  Text ->
  -- | 'tsfedsResource'
  Text ->
  TaskStartFailedEventDetails
taskStartFailedEventDetails pResourceType_ pResource_ =
  TaskStartFailedEventDetails'
    { _tsfedsError = Nothing,
      _tsfedsCause = Nothing,
      _tsfedsResourceType = pResourceType_,
      _tsfedsResource = pResource_
    }

-- | The error code of the failure.
tsfedsError :: Lens' TaskStartFailedEventDetails (Maybe Text)
tsfedsError = lens _tsfedsError (\s a -> s {_tsfedsError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
tsfedsCause :: Lens' TaskStartFailedEventDetails (Maybe Text)
tsfedsCause = lens _tsfedsCause (\s a -> s {_tsfedsCause = a}) . mapping _Sensitive

-- | The action of the resource called by a task state.
tsfedsResourceType :: Lens' TaskStartFailedEventDetails Text
tsfedsResourceType = lens _tsfedsResourceType (\s a -> s {_tsfedsResourceType = a})

-- | The service name of the resource in a task state.
tsfedsResource :: Lens' TaskStartFailedEventDetails Text
tsfedsResource = lens _tsfedsResource (\s a -> s {_tsfedsResource = a})

instance FromJSON TaskStartFailedEventDetails where
  parseJSON =
    withObject
      "TaskStartFailedEventDetails"
      ( \x ->
          TaskStartFailedEventDetails'
            <$> (x .:? "error")
            <*> (x .:? "cause")
            <*> (x .: "resourceType")
            <*> (x .: "resource")
      )

instance Hashable TaskStartFailedEventDetails

instance NFData TaskStartFailedEventDetails
