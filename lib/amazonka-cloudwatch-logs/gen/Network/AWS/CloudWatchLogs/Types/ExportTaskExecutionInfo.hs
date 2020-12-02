{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the status of an export task.
--
--
--
-- /See:/ 'exportTaskExecutionInfo' smart constructor.
data ExportTaskExecutionInfo = ExportTaskExecutionInfo'
  { _eteiCreationTime ::
      !(Maybe Nat),
    _eteiCompletionTime :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportTaskExecutionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eteiCreationTime' - The creation time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'eteiCompletionTime' - The completion time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
exportTaskExecutionInfo ::
  ExportTaskExecutionInfo
exportTaskExecutionInfo =
  ExportTaskExecutionInfo'
    { _eteiCreationTime = Nothing,
      _eteiCompletionTime = Nothing
    }

-- | The creation time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
eteiCreationTime :: Lens' ExportTaskExecutionInfo (Maybe Natural)
eteiCreationTime = lens _eteiCreationTime (\s a -> s {_eteiCreationTime = a}) . mapping _Nat

-- | The completion time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
eteiCompletionTime :: Lens' ExportTaskExecutionInfo (Maybe Natural)
eteiCompletionTime = lens _eteiCompletionTime (\s a -> s {_eteiCompletionTime = a}) . mapping _Nat

instance FromJSON ExportTaskExecutionInfo where
  parseJSON =
    withObject
      "ExportTaskExecutionInfo"
      ( \x ->
          ExportTaskExecutionInfo'
            <$> (x .:? "creationTime") <*> (x .:? "completionTime")
      )

instance Hashable ExportTaskExecutionInfo

instance NFData ExportTaskExecutionInfo
