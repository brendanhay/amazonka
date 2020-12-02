{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.BatchDeleteImportDataError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.BatchDeleteImportDataError where

import Network.AWS.Discovery.Types.BatchDeleteImportDataErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Error messages returned for each import task that you deleted as a response for this command.
--
--
--
-- /See:/ 'batchDeleteImportDataError' smart constructor.
data BatchDeleteImportDataError = BatchDeleteImportDataError'
  { _bdideImportTaskId ::
      !(Maybe Text),
    _bdideErrorCode ::
      !( Maybe
           BatchDeleteImportDataErrorCode
       ),
    _bdideErrorDescription ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDeleteImportDataError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdideImportTaskId' - The unique import ID associated with the error that occurred.
--
-- * 'bdideErrorCode' - The type of error that occurred for a specific import task.
--
-- * 'bdideErrorDescription' - The description of the error that occurred for a specific import task.
batchDeleteImportDataError ::
  BatchDeleteImportDataError
batchDeleteImportDataError =
  BatchDeleteImportDataError'
    { _bdideImportTaskId = Nothing,
      _bdideErrorCode = Nothing,
      _bdideErrorDescription = Nothing
    }

-- | The unique import ID associated with the error that occurred.
bdideImportTaskId :: Lens' BatchDeleteImportDataError (Maybe Text)
bdideImportTaskId = lens _bdideImportTaskId (\s a -> s {_bdideImportTaskId = a})

-- | The type of error that occurred for a specific import task.
bdideErrorCode :: Lens' BatchDeleteImportDataError (Maybe BatchDeleteImportDataErrorCode)
bdideErrorCode = lens _bdideErrorCode (\s a -> s {_bdideErrorCode = a})

-- | The description of the error that occurred for a specific import task.
bdideErrorDescription :: Lens' BatchDeleteImportDataError (Maybe Text)
bdideErrorDescription = lens _bdideErrorDescription (\s a -> s {_bdideErrorDescription = a})

instance FromJSON BatchDeleteImportDataError where
  parseJSON =
    withObject
      "BatchDeleteImportDataError"
      ( \x ->
          BatchDeleteImportDataError'
            <$> (x .:? "importTaskId")
            <*> (x .:? "errorCode")
            <*> (x .:? "errorDescription")
      )

instance Hashable BatchDeleteImportDataError

instance NFData BatchDeleteImportDataError
