{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BatchDescribeMergeConflictsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchDescribeMergeConflictsError where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about errors in a BatchDescribeMergeConflicts operation.
--
--
--
-- /See:/ 'batchDescribeMergeConflictsError' smart constructor.
data BatchDescribeMergeConflictsError = BatchDescribeMergeConflictsError'
  { _bdmceFilePath ::
      !Text,
    _bdmceExceptionName ::
      !Text,
    _bdmceMessage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDescribeMergeConflictsError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmceFilePath' - The path to the file.
--
-- * 'bdmceExceptionName' - The name of the exception.
--
-- * 'bdmceMessage' - The message provided by the exception.
batchDescribeMergeConflictsError ::
  -- | 'bdmceFilePath'
  Text ->
  -- | 'bdmceExceptionName'
  Text ->
  -- | 'bdmceMessage'
  Text ->
  BatchDescribeMergeConflictsError
batchDescribeMergeConflictsError
  pFilePath_
  pExceptionName_
  pMessage_ =
    BatchDescribeMergeConflictsError'
      { _bdmceFilePath = pFilePath_,
        _bdmceExceptionName = pExceptionName_,
        _bdmceMessage = pMessage_
      }

-- | The path to the file.
bdmceFilePath :: Lens' BatchDescribeMergeConflictsError Text
bdmceFilePath = lens _bdmceFilePath (\s a -> s {_bdmceFilePath = a})

-- | The name of the exception.
bdmceExceptionName :: Lens' BatchDescribeMergeConflictsError Text
bdmceExceptionName = lens _bdmceExceptionName (\s a -> s {_bdmceExceptionName = a})

-- | The message provided by the exception.
bdmceMessage :: Lens' BatchDescribeMergeConflictsError Text
bdmceMessage = lens _bdmceMessage (\s a -> s {_bdmceMessage = a})

instance FromJSON BatchDescribeMergeConflictsError where
  parseJSON =
    withObject
      "BatchDescribeMergeConflictsError"
      ( \x ->
          BatchDescribeMergeConflictsError'
            <$> (x .: "filePath") <*> (x .: "exceptionName") <*> (x .: "message")
      )

instance Hashable BatchDescribeMergeConflictsError

instance NFData BatchDescribeMergeConflictsError
