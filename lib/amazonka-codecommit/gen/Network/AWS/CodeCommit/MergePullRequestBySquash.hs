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
-- Module      : Network.AWS.CodeCommit.MergePullRequestBySquash
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the squash merge strategy. If the merge is successful, it closes the pull request.
module Network.AWS.CodeCommit.MergePullRequestBySquash
  ( -- * Creating a Request
    mergePullRequestBySquash,
    MergePullRequestBySquash,

    -- * Request Lenses
    mprbsEmail,
    mprbsAuthorName,
    mprbsConflictDetailLevel,
    mprbsCommitMessage,
    mprbsConflictResolution,
    mprbsConflictResolutionStrategy,
    mprbsKeepEmptyFolders,
    mprbsSourceCommitId,
    mprbsPullRequestId,
    mprbsRepositoryName,

    -- * Destructuring the Response
    mergePullRequestBySquashResponse,
    MergePullRequestBySquashResponse,

    -- * Response Lenses
    mprbsrsPullRequest,
    mprbsrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'mergePullRequestBySquash' smart constructor.
data MergePullRequestBySquash = MergePullRequestBySquash'
  { _mprbsEmail ::
      !(Maybe Text),
    _mprbsAuthorName :: !(Maybe Text),
    _mprbsConflictDetailLevel ::
      !(Maybe ConflictDetailLevelTypeEnum),
    _mprbsCommitMessage :: !(Maybe Text),
    _mprbsConflictResolution ::
      !(Maybe ConflictResolution),
    _mprbsConflictResolutionStrategy ::
      !( Maybe
           ConflictResolutionStrategyTypeEnum
       ),
    _mprbsKeepEmptyFolders :: !(Maybe Bool),
    _mprbsSourceCommitId :: !(Maybe Text),
    _mprbsPullRequestId :: !Text,
    _mprbsRepositoryName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MergePullRequestBySquash' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mprbsEmail' - The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- * 'mprbsAuthorName' - The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- * 'mprbsConflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- * 'mprbsCommitMessage' - The commit message to include in the commit information for the merge.
--
-- * 'mprbsConflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- * 'mprbsConflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- * 'mprbsKeepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
--
-- * 'mprbsSourceCommitId' - The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
--
-- * 'mprbsPullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- * 'mprbsRepositoryName' - The name of the repository where the pull request was created.
mergePullRequestBySquash ::
  -- | 'mprbsPullRequestId'
  Text ->
  -- | 'mprbsRepositoryName'
  Text ->
  MergePullRequestBySquash
mergePullRequestBySquash pPullRequestId_ pRepositoryName_ =
  MergePullRequestBySquash'
    { _mprbsEmail = Nothing,
      _mprbsAuthorName = Nothing,
      _mprbsConflictDetailLevel = Nothing,
      _mprbsCommitMessage = Nothing,
      _mprbsConflictResolution = Nothing,
      _mprbsConflictResolutionStrategy = Nothing,
      _mprbsKeepEmptyFolders = Nothing,
      _mprbsSourceCommitId = Nothing,
      _mprbsPullRequestId = pPullRequestId_,
      _mprbsRepositoryName = pRepositoryName_
    }

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
mprbsEmail :: Lens' MergePullRequestBySquash (Maybe Text)
mprbsEmail = lens _mprbsEmail (\s a -> s {_mprbsEmail = a})

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
mprbsAuthorName :: Lens' MergePullRequestBySquash (Maybe Text)
mprbsAuthorName = lens _mprbsAuthorName (\s a -> s {_mprbsAuthorName = a})

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
mprbsConflictDetailLevel :: Lens' MergePullRequestBySquash (Maybe ConflictDetailLevelTypeEnum)
mprbsConflictDetailLevel = lens _mprbsConflictDetailLevel (\s a -> s {_mprbsConflictDetailLevel = a})

-- | The commit message to include in the commit information for the merge.
mprbsCommitMessage :: Lens' MergePullRequestBySquash (Maybe Text)
mprbsCommitMessage = lens _mprbsCommitMessage (\s a -> s {_mprbsCommitMessage = a})

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
mprbsConflictResolution :: Lens' MergePullRequestBySquash (Maybe ConflictResolution)
mprbsConflictResolution = lens _mprbsConflictResolution (\s a -> s {_mprbsConflictResolution = a})

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
mprbsConflictResolutionStrategy :: Lens' MergePullRequestBySquash (Maybe ConflictResolutionStrategyTypeEnum)
mprbsConflictResolutionStrategy = lens _mprbsConflictResolutionStrategy (\s a -> s {_mprbsConflictResolutionStrategy = a})

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
mprbsKeepEmptyFolders :: Lens' MergePullRequestBySquash (Maybe Bool)
mprbsKeepEmptyFolders = lens _mprbsKeepEmptyFolders (\s a -> s {_mprbsKeepEmptyFolders = a})

-- | The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
mprbsSourceCommitId :: Lens' MergePullRequestBySquash (Maybe Text)
mprbsSourceCommitId = lens _mprbsSourceCommitId (\s a -> s {_mprbsSourceCommitId = a})

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
mprbsPullRequestId :: Lens' MergePullRequestBySquash Text
mprbsPullRequestId = lens _mprbsPullRequestId (\s a -> s {_mprbsPullRequestId = a})

-- | The name of the repository where the pull request was created.
mprbsRepositoryName :: Lens' MergePullRequestBySquash Text
mprbsRepositoryName = lens _mprbsRepositoryName (\s a -> s {_mprbsRepositoryName = a})

instance AWSRequest MergePullRequestBySquash where
  type Rs MergePullRequestBySquash = MergePullRequestBySquashResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          MergePullRequestBySquashResponse'
            <$> (x .?> "pullRequest") <*> (pure (fromEnum s))
      )

instance Hashable MergePullRequestBySquash

instance NFData MergePullRequestBySquash

instance ToHeaders MergePullRequestBySquash where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.MergePullRequestBySquash" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON MergePullRequestBySquash where
  toJSON MergePullRequestBySquash' {..} =
    object
      ( catMaybes
          [ ("email" .=) <$> _mprbsEmail,
            ("authorName" .=) <$> _mprbsAuthorName,
            ("conflictDetailLevel" .=) <$> _mprbsConflictDetailLevel,
            ("commitMessage" .=) <$> _mprbsCommitMessage,
            ("conflictResolution" .=) <$> _mprbsConflictResolution,
            ("conflictResolutionStrategy" .=)
              <$> _mprbsConflictResolutionStrategy,
            ("keepEmptyFolders" .=) <$> _mprbsKeepEmptyFolders,
            ("sourceCommitId" .=) <$> _mprbsSourceCommitId,
            Just ("pullRequestId" .= _mprbsPullRequestId),
            Just ("repositoryName" .= _mprbsRepositoryName)
          ]
      )

instance ToPath MergePullRequestBySquash where
  toPath = const "/"

instance ToQuery MergePullRequestBySquash where
  toQuery = const mempty

-- | /See:/ 'mergePullRequestBySquashResponse' smart constructor.
data MergePullRequestBySquashResponse = MergePullRequestBySquashResponse'
  { _mprbsrsPullRequest ::
      !(Maybe PullRequest),
    _mprbsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MergePullRequestBySquashResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mprbsrsPullRequest' - Undocumented member.
--
-- * 'mprbsrsResponseStatus' - -- | The response status code.
mergePullRequestBySquashResponse ::
  -- | 'mprbsrsResponseStatus'
  Int ->
  MergePullRequestBySquashResponse
mergePullRequestBySquashResponse pResponseStatus_ =
  MergePullRequestBySquashResponse'
    { _mprbsrsPullRequest = Nothing,
      _mprbsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mprbsrsPullRequest :: Lens' MergePullRequestBySquashResponse (Maybe PullRequest)
mprbsrsPullRequest = lens _mprbsrsPullRequest (\s a -> s {_mprbsrsPullRequest = a})

-- | -- | The response status code.
mprbsrsResponseStatus :: Lens' MergePullRequestBySquashResponse Int
mprbsrsResponseStatus = lens _mprbsrsResponseStatus (\s a -> s {_mprbsrsResponseStatus = a})

instance NFData MergePullRequestBySquashResponse
