{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchGetCommits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the contents of one or more commits in a repository.
module Network.AWS.CodeCommit.BatchGetCommits
  ( -- * Creating a Request
    batchGetCommits,
    BatchGetCommits,

    -- * Request Lenses
    bgcCommitIds,
    bgcRepositoryName,

    -- * Destructuring the Response
    batchGetCommitsResponse,
    BatchGetCommitsResponse,

    -- * Response Lenses
    bgcrsCommits,
    bgcrsErrors,
    bgcrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetCommits' smart constructor.
data BatchGetCommits = BatchGetCommits'
  { _bgcCommitIds :: ![Text],
    _bgcRepositoryName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetCommits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgcCommitIds' - The full commit IDs of the commits to get information about.
--
-- * 'bgcRepositoryName' - The name of the repository that contains the commits.
batchGetCommits ::
  -- | 'bgcRepositoryName'
  Text ->
  BatchGetCommits
batchGetCommits pRepositoryName_ =
  BatchGetCommits'
    { _bgcCommitIds = mempty,
      _bgcRepositoryName = pRepositoryName_
    }

-- | The full commit IDs of the commits to get information about.
bgcCommitIds :: Lens' BatchGetCommits [Text]
bgcCommitIds = lens _bgcCommitIds (\s a -> s {_bgcCommitIds = a}) . _Coerce

-- | The name of the repository that contains the commits.
bgcRepositoryName :: Lens' BatchGetCommits Text
bgcRepositoryName = lens _bgcRepositoryName (\s a -> s {_bgcRepositoryName = a})

instance AWSRequest BatchGetCommits where
  type Rs BatchGetCommits = BatchGetCommitsResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          BatchGetCommitsResponse'
            <$> (x .?> "commits" .!@ mempty)
            <*> (x .?> "errors" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable BatchGetCommits

instance NFData BatchGetCommits

instance ToHeaders BatchGetCommits where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.BatchGetCommits" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchGetCommits where
  toJSON BatchGetCommits' {..} =
    object
      ( catMaybes
          [ Just ("commitIds" .= _bgcCommitIds),
            Just ("repositoryName" .= _bgcRepositoryName)
          ]
      )

instance ToPath BatchGetCommits where
  toPath = const "/"

instance ToQuery BatchGetCommits where
  toQuery = const mempty

-- | /See:/ 'batchGetCommitsResponse' smart constructor.
data BatchGetCommitsResponse = BatchGetCommitsResponse'
  { _bgcrsCommits ::
      !(Maybe [Commit]),
    _bgcrsErrors ::
      !(Maybe [BatchGetCommitsError]),
    _bgcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetCommitsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgcrsCommits' - An array of commit data type objects, each of which contains information about a specified commit.
--
-- * 'bgcrsErrors' - Returns any commit IDs for which information could not be found. For example, if one of the commit IDs was a shortened SHA ID or that commit was not found in the specified repository, the ID returns an error object with more information.
--
-- * 'bgcrsResponseStatus' - -- | The response status code.
batchGetCommitsResponse ::
  -- | 'bgcrsResponseStatus'
  Int ->
  BatchGetCommitsResponse
batchGetCommitsResponse pResponseStatus_ =
  BatchGetCommitsResponse'
    { _bgcrsCommits = Nothing,
      _bgcrsErrors = Nothing,
      _bgcrsResponseStatus = pResponseStatus_
    }

-- | An array of commit data type objects, each of which contains information about a specified commit.
bgcrsCommits :: Lens' BatchGetCommitsResponse [Commit]
bgcrsCommits = lens _bgcrsCommits (\s a -> s {_bgcrsCommits = a}) . _Default . _Coerce

-- | Returns any commit IDs for which information could not be found. For example, if one of the commit IDs was a shortened SHA ID or that commit was not found in the specified repository, the ID returns an error object with more information.
bgcrsErrors :: Lens' BatchGetCommitsResponse [BatchGetCommitsError]
bgcrsErrors = lens _bgcrsErrors (\s a -> s {_bgcrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bgcrsResponseStatus :: Lens' BatchGetCommitsResponse Int
bgcrsResponseStatus = lens _bgcrsResponseStatus (\s a -> s {_bgcrsResponseStatus = a})

instance NFData BatchGetCommitsResponse
