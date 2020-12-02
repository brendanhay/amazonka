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
-- Module      : Network.AWS.CodeCommit.MergePullRequestByThreeWay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the three-way merge strategy. If the merge is successful, it closes the pull request.
module Network.AWS.CodeCommit.MergePullRequestByThreeWay
  ( -- * Creating a Request
    mergePullRequestByThreeWay,
    MergePullRequestByThreeWay,

    -- * Request Lenses
    mprbtwEmail,
    mprbtwAuthorName,
    mprbtwConflictDetailLevel,
    mprbtwCommitMessage,
    mprbtwConflictResolution,
    mprbtwConflictResolutionStrategy,
    mprbtwKeepEmptyFolders,
    mprbtwSourceCommitId,
    mprbtwPullRequestId,
    mprbtwRepositoryName,

    -- * Destructuring the Response
    mergePullRequestByThreeWayResponse,
    MergePullRequestByThreeWayResponse,

    -- * Response Lenses
    mprbtwrsPullRequest,
    mprbtwrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'mergePullRequestByThreeWay' smart constructor.
data MergePullRequestByThreeWay = MergePullRequestByThreeWay'
  { _mprbtwEmail ::
      !(Maybe Text),
    _mprbtwAuthorName :: !(Maybe Text),
    _mprbtwConflictDetailLevel ::
      !(Maybe ConflictDetailLevelTypeEnum),
    _mprbtwCommitMessage :: !(Maybe Text),
    _mprbtwConflictResolution ::
      !(Maybe ConflictResolution),
    _mprbtwConflictResolutionStrategy ::
      !( Maybe
           ConflictResolutionStrategyTypeEnum
       ),
    _mprbtwKeepEmptyFolders ::
      !(Maybe Bool),
    _mprbtwSourceCommitId ::
      !(Maybe Text),
    _mprbtwPullRequestId :: !Text,
    _mprbtwRepositoryName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MergePullRequestByThreeWay' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mprbtwEmail' - The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- * 'mprbtwAuthorName' - The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- * 'mprbtwConflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- * 'mprbtwCommitMessage' - The commit message to include in the commit information for the merge.
--
-- * 'mprbtwConflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- * 'mprbtwConflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- * 'mprbtwKeepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
--
-- * 'mprbtwSourceCommitId' - The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
--
-- * 'mprbtwPullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- * 'mprbtwRepositoryName' - The name of the repository where the pull request was created.
mergePullRequestByThreeWay ::
  -- | 'mprbtwPullRequestId'
  Text ->
  -- | 'mprbtwRepositoryName'
  Text ->
  MergePullRequestByThreeWay
mergePullRequestByThreeWay pPullRequestId_ pRepositoryName_ =
  MergePullRequestByThreeWay'
    { _mprbtwEmail = Nothing,
      _mprbtwAuthorName = Nothing,
      _mprbtwConflictDetailLevel = Nothing,
      _mprbtwCommitMessage = Nothing,
      _mprbtwConflictResolution = Nothing,
      _mprbtwConflictResolutionStrategy = Nothing,
      _mprbtwKeepEmptyFolders = Nothing,
      _mprbtwSourceCommitId = Nothing,
      _mprbtwPullRequestId = pPullRequestId_,
      _mprbtwRepositoryName = pRepositoryName_
    }

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
mprbtwEmail :: Lens' MergePullRequestByThreeWay (Maybe Text)
mprbtwEmail = lens _mprbtwEmail (\s a -> s {_mprbtwEmail = a})

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
mprbtwAuthorName :: Lens' MergePullRequestByThreeWay (Maybe Text)
mprbtwAuthorName = lens _mprbtwAuthorName (\s a -> s {_mprbtwAuthorName = a})

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
mprbtwConflictDetailLevel :: Lens' MergePullRequestByThreeWay (Maybe ConflictDetailLevelTypeEnum)
mprbtwConflictDetailLevel = lens _mprbtwConflictDetailLevel (\s a -> s {_mprbtwConflictDetailLevel = a})

-- | The commit message to include in the commit information for the merge.
mprbtwCommitMessage :: Lens' MergePullRequestByThreeWay (Maybe Text)
mprbtwCommitMessage = lens _mprbtwCommitMessage (\s a -> s {_mprbtwCommitMessage = a})

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
mprbtwConflictResolution :: Lens' MergePullRequestByThreeWay (Maybe ConflictResolution)
mprbtwConflictResolution = lens _mprbtwConflictResolution (\s a -> s {_mprbtwConflictResolution = a})

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
mprbtwConflictResolutionStrategy :: Lens' MergePullRequestByThreeWay (Maybe ConflictResolutionStrategyTypeEnum)
mprbtwConflictResolutionStrategy = lens _mprbtwConflictResolutionStrategy (\s a -> s {_mprbtwConflictResolutionStrategy = a})

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
mprbtwKeepEmptyFolders :: Lens' MergePullRequestByThreeWay (Maybe Bool)
mprbtwKeepEmptyFolders = lens _mprbtwKeepEmptyFolders (\s a -> s {_mprbtwKeepEmptyFolders = a})

-- | The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
mprbtwSourceCommitId :: Lens' MergePullRequestByThreeWay (Maybe Text)
mprbtwSourceCommitId = lens _mprbtwSourceCommitId (\s a -> s {_mprbtwSourceCommitId = a})

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
mprbtwPullRequestId :: Lens' MergePullRequestByThreeWay Text
mprbtwPullRequestId = lens _mprbtwPullRequestId (\s a -> s {_mprbtwPullRequestId = a})

-- | The name of the repository where the pull request was created.
mprbtwRepositoryName :: Lens' MergePullRequestByThreeWay Text
mprbtwRepositoryName = lens _mprbtwRepositoryName (\s a -> s {_mprbtwRepositoryName = a})

instance AWSRequest MergePullRequestByThreeWay where
  type
    Rs MergePullRequestByThreeWay =
      MergePullRequestByThreeWayResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          MergePullRequestByThreeWayResponse'
            <$> (x .?> "pullRequest") <*> (pure (fromEnum s))
      )

instance Hashable MergePullRequestByThreeWay

instance NFData MergePullRequestByThreeWay

instance ToHeaders MergePullRequestByThreeWay where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.MergePullRequestByThreeWay" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON MergePullRequestByThreeWay where
  toJSON MergePullRequestByThreeWay' {..} =
    object
      ( catMaybes
          [ ("email" .=) <$> _mprbtwEmail,
            ("authorName" .=) <$> _mprbtwAuthorName,
            ("conflictDetailLevel" .=) <$> _mprbtwConflictDetailLevel,
            ("commitMessage" .=) <$> _mprbtwCommitMessage,
            ("conflictResolution" .=) <$> _mprbtwConflictResolution,
            ("conflictResolutionStrategy" .=)
              <$> _mprbtwConflictResolutionStrategy,
            ("keepEmptyFolders" .=) <$> _mprbtwKeepEmptyFolders,
            ("sourceCommitId" .=) <$> _mprbtwSourceCommitId,
            Just ("pullRequestId" .= _mprbtwPullRequestId),
            Just ("repositoryName" .= _mprbtwRepositoryName)
          ]
      )

instance ToPath MergePullRequestByThreeWay where
  toPath = const "/"

instance ToQuery MergePullRequestByThreeWay where
  toQuery = const mempty

-- | /See:/ 'mergePullRequestByThreeWayResponse' smart constructor.
data MergePullRequestByThreeWayResponse = MergePullRequestByThreeWayResponse'
  { _mprbtwrsPullRequest ::
      !(Maybe PullRequest),
    _mprbtwrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MergePullRequestByThreeWayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mprbtwrsPullRequest' - Undocumented member.
--
-- * 'mprbtwrsResponseStatus' - -- | The response status code.
mergePullRequestByThreeWayResponse ::
  -- | 'mprbtwrsResponseStatus'
  Int ->
  MergePullRequestByThreeWayResponse
mergePullRequestByThreeWayResponse pResponseStatus_ =
  MergePullRequestByThreeWayResponse'
    { _mprbtwrsPullRequest =
        Nothing,
      _mprbtwrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mprbtwrsPullRequest :: Lens' MergePullRequestByThreeWayResponse (Maybe PullRequest)
mprbtwrsPullRequest = lens _mprbtwrsPullRequest (\s a -> s {_mprbtwrsPullRequest = a})

-- | -- | The response status code.
mprbtwrsResponseStatus :: Lens' MergePullRequestByThreeWayResponse Int
mprbtwrsResponseStatus = lens _mprbtwrsResponseStatus (\s a -> s {_mprbtwrsResponseStatus = a})

instance NFData MergePullRequestByThreeWayResponse
