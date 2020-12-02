{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTaskStatus where

import Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the status of an export task.
--
--
--
-- /See:/ 'exportTaskStatus' smart constructor.
data ExportTaskStatus = ExportTaskStatus'
  { _etsCode ::
      !(Maybe ExportTaskStatusCode),
    _etsMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportTaskStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etsCode' - The status code of the export task.
--
-- * 'etsMessage' - The status message related to the status code.
exportTaskStatus ::
  ExportTaskStatus
exportTaskStatus =
  ExportTaskStatus' {_etsCode = Nothing, _etsMessage = Nothing}

-- | The status code of the export task.
etsCode :: Lens' ExportTaskStatus (Maybe ExportTaskStatusCode)
etsCode = lens _etsCode (\s a -> s {_etsCode = a})

-- | The status message related to the status code.
etsMessage :: Lens' ExportTaskStatus (Maybe Text)
etsMessage = lens _etsMessage (\s a -> s {_etsMessage = a})

instance FromJSON ExportTaskStatus where
  parseJSON =
    withObject
      "ExportTaskStatus"
      (\x -> ExportTaskStatus' <$> (x .:? "code") <*> (x .:? "message"))

instance Hashable ExportTaskStatus

instance NFData ExportTaskStatus
