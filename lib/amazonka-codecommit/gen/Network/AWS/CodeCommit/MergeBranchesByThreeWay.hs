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
-- Module      : Network.AWS.CodeCommit.MergeBranchesByThreeWay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two specified branches using the three-way merge strategy.
module Network.AWS.CodeCommit.MergeBranchesByThreeWay
  ( -- * Creating a Request
    mergeBranchesByThreeWay,
    MergeBranchesByThreeWay,

    -- * Request Lenses
    mbbtwEmail,
    mbbtwAuthorName,
    mbbtwTargetBranch,
    mbbtwConflictDetailLevel,
    mbbtwCommitMessage,
    mbbtwConflictResolution,
    mbbtwConflictResolutionStrategy,
    mbbtwKeepEmptyFolders,
    mbbtwRepositoryName,
    mbbtwSourceCommitSpecifier,
    mbbtwDestinationCommitSpecifier,

    -- * Destructuring the Response
    mergeBranchesByThreeWayResponse,
    MergeBranchesByThreeWayResponse,

    -- * Response Lenses
    mbbtwrsCommitId,
    mbbtwrsTreeId,
    mbbtwrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'mergeBranchesByThreeWay' smart constructor.
data MergeBranchesByThreeWay = MergeBranchesByThreeWay'
  { _mbbtwEmail ::
      !(Maybe Text),
    _mbbtwAuthorName :: !(Maybe Text),
    _mbbtwTargetBranch :: !(Maybe Text),
    _mbbtwConflictDetailLevel ::
      !(Maybe ConflictDetailLevelTypeEnum),
    _mbbtwCommitMessage :: !(Maybe Text),
    _mbbtwConflictResolution ::
      !(Maybe ConflictResolution),
    _mbbtwConflictResolutionStrategy ::
      !(Maybe ConflictResolutionStrategyTypeEnum),
    _mbbtwKeepEmptyFolders :: !(Maybe Bool),
    _mbbtwRepositoryName :: !Text,
    _mbbtwSourceCommitSpecifier :: !Text,
    _mbbtwDestinationCommitSpecifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MergeBranchesByThreeWay' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mbbtwEmail' - The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- * 'mbbtwAuthorName' - The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- * 'mbbtwTargetBranch' - The branch where the merge is applied.
--
-- * 'mbbtwConflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- * 'mbbtwCommitMessage' - The commit message to include in the commit information for the merge.
--
-- * 'mbbtwConflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- * 'mbbtwConflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- * 'mbbtwKeepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
--
-- * 'mbbtwRepositoryName' - The name of the repository where you want to merge two branches.
--
-- * 'mbbtwSourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- * 'mbbtwDestinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mergeBranchesByThreeWay ::
  -- | 'mbbtwRepositoryName'
  Text ->
  -- | 'mbbtwSourceCommitSpecifier'
  Text ->
  -- | 'mbbtwDestinationCommitSpecifier'
  Text ->
  MergeBranchesByThreeWay
mergeBranchesByThreeWay
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    MergeBranchesByThreeWay'
      { _mbbtwEmail = Nothing,
        _mbbtwAuthorName = Nothing,
        _mbbtwTargetBranch = Nothing,
        _mbbtwConflictDetailLevel = Nothing,
        _mbbtwCommitMessage = Nothing,
        _mbbtwConflictResolution = Nothing,
        _mbbtwConflictResolutionStrategy = Nothing,
        _mbbtwKeepEmptyFolders = Nothing,
        _mbbtwRepositoryName = pRepositoryName_,
        _mbbtwSourceCommitSpecifier = pSourceCommitSpecifier_,
        _mbbtwDestinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
mbbtwEmail :: Lens' MergeBranchesByThreeWay (Maybe Text)
mbbtwEmail = lens _mbbtwEmail (\s a -> s {_mbbtwEmail = a})

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
mbbtwAuthorName :: Lens' MergeBranchesByThreeWay (Maybe Text)
mbbtwAuthorName = lens _mbbtwAuthorName (\s a -> s {_mbbtwAuthorName = a})

-- | The branch where the merge is applied.
mbbtwTargetBranch :: Lens' MergeBranchesByThreeWay (Maybe Text)
mbbtwTargetBranch = lens _mbbtwTargetBranch (\s a -> s {_mbbtwTargetBranch = a})

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
mbbtwConflictDetailLevel :: Lens' MergeBranchesByThreeWay (Maybe ConflictDetailLevelTypeEnum)
mbbtwConflictDetailLevel = lens _mbbtwConflictDetailLevel (\s a -> s {_mbbtwConflictDetailLevel = a})

-- | The commit message to include in the commit information for the merge.
mbbtwCommitMessage :: Lens' MergeBranchesByThreeWay (Maybe Text)
mbbtwCommitMessage = lens _mbbtwCommitMessage (\s a -> s {_mbbtwCommitMessage = a})

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
mbbtwConflictResolution :: Lens' MergeBranchesByThreeWay (Maybe ConflictResolution)
mbbtwConflictResolution = lens _mbbtwConflictResolution (\s a -> s {_mbbtwConflictResolution = a})

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
mbbtwConflictResolutionStrategy :: Lens' MergeBranchesByThreeWay (Maybe ConflictResolutionStrategyTypeEnum)
mbbtwConflictResolutionStrategy = lens _mbbtwConflictResolutionStrategy (\s a -> s {_mbbtwConflictResolutionStrategy = a})

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
mbbtwKeepEmptyFolders :: Lens' MergeBranchesByThreeWay (Maybe Bool)
mbbtwKeepEmptyFolders = lens _mbbtwKeepEmptyFolders (\s a -> s {_mbbtwKeepEmptyFolders = a})

-- | The name of the repository where you want to merge two branches.
mbbtwRepositoryName :: Lens' MergeBranchesByThreeWay Text
mbbtwRepositoryName = lens _mbbtwRepositoryName (\s a -> s {_mbbtwRepositoryName = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mbbtwSourceCommitSpecifier :: Lens' MergeBranchesByThreeWay Text
mbbtwSourceCommitSpecifier = lens _mbbtwSourceCommitSpecifier (\s a -> s {_mbbtwSourceCommitSpecifier = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mbbtwDestinationCommitSpecifier :: Lens' MergeBranchesByThreeWay Text
mbbtwDestinationCommitSpecifier = lens _mbbtwDestinationCommitSpecifier (\s a -> s {_mbbtwDestinationCommitSpecifier = a})

instance AWSRequest MergeBranchesByThreeWay where
  type Rs MergeBranchesByThreeWay = MergeBranchesByThreeWayResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          MergeBranchesByThreeWayResponse'
            <$> (x .?> "commitId") <*> (x .?> "treeId") <*> (pure (fromEnum s))
      )

instance Hashable MergeBranchesByThreeWay

instance NFData MergeBranchesByThreeWay

instance ToHeaders MergeBranchesByThreeWay where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.MergeBranchesByThreeWay" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON MergeBranchesByThreeWay where
  toJSON MergeBranchesByThreeWay' {..} =
    object
      ( catMaybes
          [ ("email" .=) <$> _mbbtwEmail,
            ("authorName" .=) <$> _mbbtwAuthorName,
            ("targetBranch" .=) <$> _mbbtwTargetBranch,
            ("conflictDetailLevel" .=) <$> _mbbtwConflictDetailLevel,
            ("commitMessage" .=) <$> _mbbtwCommitMessage,
            ("conflictResolution" .=) <$> _mbbtwConflictResolution,
            ("conflictResolutionStrategy" .=)
              <$> _mbbtwConflictResolutionStrategy,
            ("keepEmptyFolders" .=) <$> _mbbtwKeepEmptyFolders,
            Just ("repositoryName" .= _mbbtwRepositoryName),
            Just ("sourceCommitSpecifier" .= _mbbtwSourceCommitSpecifier),
            Just
              ( "destinationCommitSpecifier"
                  .= _mbbtwDestinationCommitSpecifier
              )
          ]
      )

instance ToPath MergeBranchesByThreeWay where
  toPath = const "/"

instance ToQuery MergeBranchesByThreeWay where
  toQuery = const mempty

-- | /See:/ 'mergeBranchesByThreeWayResponse' smart constructor.
data MergeBranchesByThreeWayResponse = MergeBranchesByThreeWayResponse'
  { _mbbtwrsCommitId ::
      !(Maybe Text),
    _mbbtwrsTreeId ::
      !(Maybe Text),
    _mbbtwrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MergeBranchesByThreeWayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mbbtwrsCommitId' - The commit ID of the merge in the destination or target branch.
--
-- * 'mbbtwrsTreeId' - The tree ID of the merge in the destination or target branch.
--
-- * 'mbbtwrsResponseStatus' - -- | The response status code.
mergeBranchesByThreeWayResponse ::
  -- | 'mbbtwrsResponseStatus'
  Int ->
  MergeBranchesByThreeWayResponse
mergeBranchesByThreeWayResponse pResponseStatus_ =
  MergeBranchesByThreeWayResponse'
    { _mbbtwrsCommitId = Nothing,
      _mbbtwrsTreeId = Nothing,
      _mbbtwrsResponseStatus = pResponseStatus_
    }

-- | The commit ID of the merge in the destination or target branch.
mbbtwrsCommitId :: Lens' MergeBranchesByThreeWayResponse (Maybe Text)
mbbtwrsCommitId = lens _mbbtwrsCommitId (\s a -> s {_mbbtwrsCommitId = a})

-- | The tree ID of the merge in the destination or target branch.
mbbtwrsTreeId :: Lens' MergeBranchesByThreeWayResponse (Maybe Text)
mbbtwrsTreeId = lens _mbbtwrsTreeId (\s a -> s {_mbbtwrsTreeId = a})

-- | -- | The response status code.
mbbtwrsResponseStatus :: Lens' MergeBranchesByThreeWayResponse Int
mbbtwrsResponseStatus = lens _mbbtwrsResponseStatus (\s a -> s {_mbbtwrsResponseStatus = a})

instance NFData MergeBranchesByThreeWayResponse
