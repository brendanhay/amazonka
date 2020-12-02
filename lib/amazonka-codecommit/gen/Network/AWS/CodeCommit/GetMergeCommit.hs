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
-- Module      : Network.AWS.CodeCommit.GetMergeCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified merge commit.
module Network.AWS.CodeCommit.GetMergeCommit
  ( -- * Creating a Request
    getMergeCommit,
    GetMergeCommit,

    -- * Request Lenses
    gmcConflictDetailLevel,
    gmcConflictResolutionStrategy,
    gmcRepositoryName,
    gmcSourceCommitSpecifier,
    gmcDestinationCommitSpecifier,

    -- * Destructuring the Response
    getMergeCommitResponse,
    GetMergeCommitResponse,

    -- * Response Lenses
    gmcrsMergedCommitId,
    gmcrsDestinationCommitId,
    gmcrsBaseCommitId,
    gmcrsSourceCommitId,
    gmcrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMergeCommit' smart constructor.
data GetMergeCommit = GetMergeCommit'
  { _gmcConflictDetailLevel ::
      !(Maybe ConflictDetailLevelTypeEnum),
    _gmcConflictResolutionStrategy ::
      !(Maybe ConflictResolutionStrategyTypeEnum),
    _gmcRepositoryName :: !Text,
    _gmcSourceCommitSpecifier :: !Text,
    _gmcDestinationCommitSpecifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMergeCommit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmcConflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- * 'gmcConflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- * 'gmcRepositoryName' - The name of the repository that contains the merge commit about which you want to get information.
--
-- * 'gmcSourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- * 'gmcDestinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
getMergeCommit ::
  -- | 'gmcRepositoryName'
  Text ->
  -- | 'gmcSourceCommitSpecifier'
  Text ->
  -- | 'gmcDestinationCommitSpecifier'
  Text ->
  GetMergeCommit
getMergeCommit
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    GetMergeCommit'
      { _gmcConflictDetailLevel = Nothing,
        _gmcConflictResolutionStrategy = Nothing,
        _gmcRepositoryName = pRepositoryName_,
        _gmcSourceCommitSpecifier = pSourceCommitSpecifier_,
        _gmcDestinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
gmcConflictDetailLevel :: Lens' GetMergeCommit (Maybe ConflictDetailLevelTypeEnum)
gmcConflictDetailLevel = lens _gmcConflictDetailLevel (\s a -> s {_gmcConflictDetailLevel = a})

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
gmcConflictResolutionStrategy :: Lens' GetMergeCommit (Maybe ConflictResolutionStrategyTypeEnum)
gmcConflictResolutionStrategy = lens _gmcConflictResolutionStrategy (\s a -> s {_gmcConflictResolutionStrategy = a})

-- | The name of the repository that contains the merge commit about which you want to get information.
gmcRepositoryName :: Lens' GetMergeCommit Text
gmcRepositoryName = lens _gmcRepositoryName (\s a -> s {_gmcRepositoryName = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
gmcSourceCommitSpecifier :: Lens' GetMergeCommit Text
gmcSourceCommitSpecifier = lens _gmcSourceCommitSpecifier (\s a -> s {_gmcSourceCommitSpecifier = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
gmcDestinationCommitSpecifier :: Lens' GetMergeCommit Text
gmcDestinationCommitSpecifier = lens _gmcDestinationCommitSpecifier (\s a -> s {_gmcDestinationCommitSpecifier = a})

instance AWSRequest GetMergeCommit where
  type Rs GetMergeCommit = GetMergeCommitResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          GetMergeCommitResponse'
            <$> (x .?> "mergedCommitId")
            <*> (x .?> "destinationCommitId")
            <*> (x .?> "baseCommitId")
            <*> (x .?> "sourceCommitId")
            <*> (pure (fromEnum s))
      )

instance Hashable GetMergeCommit

instance NFData GetMergeCommit

instance ToHeaders GetMergeCommit where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.GetMergeCommit" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMergeCommit where
  toJSON GetMergeCommit' {..} =
    object
      ( catMaybes
          [ ("conflictDetailLevel" .=) <$> _gmcConflictDetailLevel,
            ("conflictResolutionStrategy" .=)
              <$> _gmcConflictResolutionStrategy,
            Just ("repositoryName" .= _gmcRepositoryName),
            Just ("sourceCommitSpecifier" .= _gmcSourceCommitSpecifier),
            Just
              ("destinationCommitSpecifier" .= _gmcDestinationCommitSpecifier)
          ]
      )

instance ToPath GetMergeCommit where
  toPath = const "/"

instance ToQuery GetMergeCommit where
  toQuery = const mempty

-- | /See:/ 'getMergeCommitResponse' smart constructor.
data GetMergeCommitResponse = GetMergeCommitResponse'
  { _gmcrsMergedCommitId ::
      !(Maybe Text),
    _gmcrsDestinationCommitId :: !(Maybe Text),
    _gmcrsBaseCommitId :: !(Maybe Text),
    _gmcrsSourceCommitId :: !(Maybe Text),
    _gmcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMergeCommitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmcrsMergedCommitId' - The commit ID for the merge commit created when the source branch was merged into the destination branch. If the fast-forward merge strategy was used, there is no merge commit.
--
-- * 'gmcrsDestinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- * 'gmcrsBaseCommitId' - The commit ID of the merge base.
--
-- * 'gmcrsSourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- * 'gmcrsResponseStatus' - -- | The response status code.
getMergeCommitResponse ::
  -- | 'gmcrsResponseStatus'
  Int ->
  GetMergeCommitResponse
getMergeCommitResponse pResponseStatus_ =
  GetMergeCommitResponse'
    { _gmcrsMergedCommitId = Nothing,
      _gmcrsDestinationCommitId = Nothing,
      _gmcrsBaseCommitId = Nothing,
      _gmcrsSourceCommitId = Nothing,
      _gmcrsResponseStatus = pResponseStatus_
    }

-- | The commit ID for the merge commit created when the source branch was merged into the destination branch. If the fast-forward merge strategy was used, there is no merge commit.
gmcrsMergedCommitId :: Lens' GetMergeCommitResponse (Maybe Text)
gmcrsMergedCommitId = lens _gmcrsMergedCommitId (\s a -> s {_gmcrsMergedCommitId = a})

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
gmcrsDestinationCommitId :: Lens' GetMergeCommitResponse (Maybe Text)
gmcrsDestinationCommitId = lens _gmcrsDestinationCommitId (\s a -> s {_gmcrsDestinationCommitId = a})

-- | The commit ID of the merge base.
gmcrsBaseCommitId :: Lens' GetMergeCommitResponse (Maybe Text)
gmcrsBaseCommitId = lens _gmcrsBaseCommitId (\s a -> s {_gmcrsBaseCommitId = a})

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
gmcrsSourceCommitId :: Lens' GetMergeCommitResponse (Maybe Text)
gmcrsSourceCommitId = lens _gmcrsSourceCommitId (\s a -> s {_gmcrsSourceCommitId = a})

-- | -- | The response status code.
gmcrsResponseStatus :: Lens' GetMergeCommitResponse Int
gmcrsResponseStatus = lens _gmcrsResponseStatus (\s a -> s {_gmcrsResponseStatus = a})

instance NFData GetMergeCommitResponse
