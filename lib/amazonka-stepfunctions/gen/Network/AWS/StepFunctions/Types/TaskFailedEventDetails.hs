{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskFailedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a task failure event.
--
--
--
-- /See:/ 'taskFailedEventDetails' smart constructor.
data TaskFailedEventDetails = TaskFailedEventDetails'
  { _tfedError ::
      !(Maybe (Sensitive Text)),
    _tfedCause :: !(Maybe (Sensitive Text)),
    _tfedResourceType :: !Text,
    _tfedResource :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfedError' - The error code of the failure.
--
-- * 'tfedCause' - A more detailed explanation of the cause of the failure.
--
-- * 'tfedResourceType' - The action of the resource called by a task state.
--
-- * 'tfedResource' - The service name of the resource in a task state.
taskFailedEventDetails ::
  -- | 'tfedResourceType'
  Text ->
  -- | 'tfedResource'
  Text ->
  TaskFailedEventDetails
taskFailedEventDetails pResourceType_ pResource_ =
  TaskFailedEventDetails'
    { _tfedError = Nothing,
      _tfedCause = Nothing,
      _tfedResourceType = pResourceType_,
      _tfedResource = pResource_
    }

-- | The error code of the failure.
tfedError :: Lens' TaskFailedEventDetails (Maybe Text)
tfedError = lens _tfedError (\s a -> s {_tfedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
tfedCause :: Lens' TaskFailedEventDetails (Maybe Text)
tfedCause = lens _tfedCause (\s a -> s {_tfedCause = a}) . mapping _Sensitive

-- | The action of the resource called by a task state.
tfedResourceType :: Lens' TaskFailedEventDetails Text
tfedResourceType = lens _tfedResourceType (\s a -> s {_tfedResourceType = a})

-- | The service name of the resource in a task state.
tfedResource :: Lens' TaskFailedEventDetails Text
tfedResource = lens _tfedResource (\s a -> s {_tfedResource = a})

instance FromJSON TaskFailedEventDetails where
  parseJSON =
    withObject
      "TaskFailedEventDetails"
      ( \x ->
          TaskFailedEventDetails'
            <$> (x .:? "error")
            <*> (x .:? "cause")
            <*> (x .: "resourceType")
            <*> (x .: "resource")
      )

instance Hashable TaskFailedEventDetails

instance NFData TaskFailedEventDetails
