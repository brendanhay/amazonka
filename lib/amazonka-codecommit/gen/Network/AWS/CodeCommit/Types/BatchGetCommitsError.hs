{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BatchGetCommitsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchGetCommitsError where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about errors in a BatchGetCommits operation.
--
--
--
-- /See:/ 'batchGetCommitsError' smart constructor.
data BatchGetCommitsError = BatchGetCommitsError'
  { _bgceCommitId ::
      !(Maybe Text),
    _bgceErrorCode :: !(Maybe Text),
    _bgceErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetCommitsError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgceCommitId' - A commit ID that either could not be found or was not in a valid format.
--
-- * 'bgceErrorCode' - An error code that specifies whether the commit ID was not valid or not found.
--
-- * 'bgceErrorMessage' - An error message that provides detail about why the commit ID either was not found or was not valid.
batchGetCommitsError ::
  BatchGetCommitsError
batchGetCommitsError =
  BatchGetCommitsError'
    { _bgceCommitId = Nothing,
      _bgceErrorCode = Nothing,
      _bgceErrorMessage = Nothing
    }

-- | A commit ID that either could not be found or was not in a valid format.
bgceCommitId :: Lens' BatchGetCommitsError (Maybe Text)
bgceCommitId = lens _bgceCommitId (\s a -> s {_bgceCommitId = a})

-- | An error code that specifies whether the commit ID was not valid or not found.
bgceErrorCode :: Lens' BatchGetCommitsError (Maybe Text)
bgceErrorCode = lens _bgceErrorCode (\s a -> s {_bgceErrorCode = a})

-- | An error message that provides detail about why the commit ID either was not found or was not valid.
bgceErrorMessage :: Lens' BatchGetCommitsError (Maybe Text)
bgceErrorMessage = lens _bgceErrorMessage (\s a -> s {_bgceErrorMessage = a})

instance FromJSON BatchGetCommitsError where
  parseJSON =
    withObject
      "BatchGetCommitsError"
      ( \x ->
          BatchGetCommitsError'
            <$> (x .:? "commitId")
            <*> (x .:? "errorCode")
            <*> (x .:? "errorMessage")
      )

instance Hashable BatchGetCommitsError

instance NFData BatchGetCommitsError
