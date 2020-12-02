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
-- Module      : Network.AWS.CodeCommit.DescribeMergeConflicts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more merge conflicts in the attempted merge of two commit specifiers using the squash or three-way merge strategy. If the merge option for the attempted merge is specified as FAST_FORWARD_MERGE, an exception is thrown.
module Network.AWS.CodeCommit.DescribeMergeConflicts
  ( -- * Creating a Request
    describeMergeConflicts,
    DescribeMergeConflicts,

    -- * Request Lenses
    dmcConflictDetailLevel,
    dmcNextToken,
    dmcMaxMergeHunks,
    dmcConflictResolutionStrategy,
    dmcRepositoryName,
    dmcDestinationCommitSpecifier,
    dmcSourceCommitSpecifier,
    dmcMergeOption,
    dmcFilePath,

    -- * Destructuring the Response
    describeMergeConflictsResponse,
    DescribeMergeConflictsResponse,

    -- * Response Lenses
    dmcrsBaseCommitId,
    dmcrsNextToken,
    dmcrsResponseStatus,
    dmcrsConflictMetadata,
    dmcrsMergeHunks,
    dmcrsDestinationCommitId,
    dmcrsSourceCommitId,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeMergeConflicts' smart constructor.
data DescribeMergeConflicts = DescribeMergeConflicts'
  { _dmcConflictDetailLevel ::
      !(Maybe ConflictDetailLevelTypeEnum),
    _dmcNextToken :: !(Maybe Text),
    _dmcMaxMergeHunks :: !(Maybe Int),
    _dmcConflictResolutionStrategy ::
      !(Maybe ConflictResolutionStrategyTypeEnum),
    _dmcRepositoryName :: !Text,
    _dmcDestinationCommitSpecifier :: !Text,
    _dmcSourceCommitSpecifier :: !Text,
    _dmcMergeOption :: !MergeOptionTypeEnum,
    _dmcFilePath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMergeConflicts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmcConflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- * 'dmcNextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- * 'dmcMaxMergeHunks' - The maximum number of merge hunks to include in the output.
--
-- * 'dmcConflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- * 'dmcRepositoryName' - The name of the repository where you want to get information about a merge conflict.
--
-- * 'dmcDestinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- * 'dmcSourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- * 'dmcMergeOption' - The merge option or strategy you want to use to merge the code.
--
-- * 'dmcFilePath' - The path of the target files used to describe the conflicts.
describeMergeConflicts ::
  -- | 'dmcRepositoryName'
  Text ->
  -- | 'dmcDestinationCommitSpecifier'
  Text ->
  -- | 'dmcSourceCommitSpecifier'
  Text ->
  -- | 'dmcMergeOption'
  MergeOptionTypeEnum ->
  -- | 'dmcFilePath'
  Text ->
  DescribeMergeConflicts
describeMergeConflicts
  pRepositoryName_
  pDestinationCommitSpecifier_
  pSourceCommitSpecifier_
  pMergeOption_
  pFilePath_ =
    DescribeMergeConflicts'
      { _dmcConflictDetailLevel = Nothing,
        _dmcNextToken = Nothing,
        _dmcMaxMergeHunks = Nothing,
        _dmcConflictResolutionStrategy = Nothing,
        _dmcRepositoryName = pRepositoryName_,
        _dmcDestinationCommitSpecifier = pDestinationCommitSpecifier_,
        _dmcSourceCommitSpecifier = pSourceCommitSpecifier_,
        _dmcMergeOption = pMergeOption_,
        _dmcFilePath = pFilePath_
      }

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
dmcConflictDetailLevel :: Lens' DescribeMergeConflicts (Maybe ConflictDetailLevelTypeEnum)
dmcConflictDetailLevel = lens _dmcConflictDetailLevel (\s a -> s {_dmcConflictDetailLevel = a})

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
dmcNextToken :: Lens' DescribeMergeConflicts (Maybe Text)
dmcNextToken = lens _dmcNextToken (\s a -> s {_dmcNextToken = a})

-- | The maximum number of merge hunks to include in the output.
dmcMaxMergeHunks :: Lens' DescribeMergeConflicts (Maybe Int)
dmcMaxMergeHunks = lens _dmcMaxMergeHunks (\s a -> s {_dmcMaxMergeHunks = a})

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
dmcConflictResolutionStrategy :: Lens' DescribeMergeConflicts (Maybe ConflictResolutionStrategyTypeEnum)
dmcConflictResolutionStrategy = lens _dmcConflictResolutionStrategy (\s a -> s {_dmcConflictResolutionStrategy = a})

-- | The name of the repository where you want to get information about a merge conflict.
dmcRepositoryName :: Lens' DescribeMergeConflicts Text
dmcRepositoryName = lens _dmcRepositoryName (\s a -> s {_dmcRepositoryName = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
dmcDestinationCommitSpecifier :: Lens' DescribeMergeConflicts Text
dmcDestinationCommitSpecifier = lens _dmcDestinationCommitSpecifier (\s a -> s {_dmcDestinationCommitSpecifier = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
dmcSourceCommitSpecifier :: Lens' DescribeMergeConflicts Text
dmcSourceCommitSpecifier = lens _dmcSourceCommitSpecifier (\s a -> s {_dmcSourceCommitSpecifier = a})

-- | The merge option or strategy you want to use to merge the code.
dmcMergeOption :: Lens' DescribeMergeConflicts MergeOptionTypeEnum
dmcMergeOption = lens _dmcMergeOption (\s a -> s {_dmcMergeOption = a})

-- | The path of the target files used to describe the conflicts.
dmcFilePath :: Lens' DescribeMergeConflicts Text
dmcFilePath = lens _dmcFilePath (\s a -> s {_dmcFilePath = a})

instance AWSRequest DescribeMergeConflicts where
  type Rs DescribeMergeConflicts = DescribeMergeConflictsResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          DescribeMergeConflictsResponse'
            <$> (x .?> "baseCommitId")
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
            <*> (x .:> "conflictMetadata")
            <*> (x .?> "mergeHunks" .!@ mempty)
            <*> (x .:> "destinationCommitId")
            <*> (x .:> "sourceCommitId")
      )

instance Hashable DescribeMergeConflicts

instance NFData DescribeMergeConflicts

instance ToHeaders DescribeMergeConflicts where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.DescribeMergeConflicts" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeMergeConflicts where
  toJSON DescribeMergeConflicts' {..} =
    object
      ( catMaybes
          [ ("conflictDetailLevel" .=) <$> _dmcConflictDetailLevel,
            ("nextToken" .=) <$> _dmcNextToken,
            ("maxMergeHunks" .=) <$> _dmcMaxMergeHunks,
            ("conflictResolutionStrategy" .=)
              <$> _dmcConflictResolutionStrategy,
            Just ("repositoryName" .= _dmcRepositoryName),
            Just
              ("destinationCommitSpecifier" .= _dmcDestinationCommitSpecifier),
            Just ("sourceCommitSpecifier" .= _dmcSourceCommitSpecifier),
            Just ("mergeOption" .= _dmcMergeOption),
            Just ("filePath" .= _dmcFilePath)
          ]
      )

instance ToPath DescribeMergeConflicts where
  toPath = const "/"

instance ToQuery DescribeMergeConflicts where
  toQuery = const mempty

-- | /See:/ 'describeMergeConflictsResponse' smart constructor.
data DescribeMergeConflictsResponse = DescribeMergeConflictsResponse'
  { _dmcrsBaseCommitId ::
      !(Maybe Text),
    _dmcrsNextToken ::
      !(Maybe Text),
    _dmcrsResponseStatus :: !Int,
    _dmcrsConflictMetadata ::
      !ConflictMetadata,
    _dmcrsMergeHunks ::
      ![MergeHunk],
    _dmcrsDestinationCommitId ::
      !Text,
    _dmcrsSourceCommitId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMergeConflictsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmcrsBaseCommitId' - The commit ID of the merge base.
--
-- * 'dmcrsNextToken' - An enumeration token that can be used in a request to return the next batch of the results.
--
-- * 'dmcrsResponseStatus' - -- | The response status code.
--
-- * 'dmcrsConflictMetadata' - Contains metadata about the conflicts found in the merge.
--
-- * 'dmcrsMergeHunks' - A list of merge hunks of the differences between the files or lines.
--
-- * 'dmcrsDestinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- * 'dmcrsSourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
describeMergeConflictsResponse ::
  -- | 'dmcrsResponseStatus'
  Int ->
  -- | 'dmcrsConflictMetadata'
  ConflictMetadata ->
  -- | 'dmcrsDestinationCommitId'
  Text ->
  -- | 'dmcrsSourceCommitId'
  Text ->
  DescribeMergeConflictsResponse
describeMergeConflictsResponse
  pResponseStatus_
  pConflictMetadata_
  pDestinationCommitId_
  pSourceCommitId_ =
    DescribeMergeConflictsResponse'
      { _dmcrsBaseCommitId = Nothing,
        _dmcrsNextToken = Nothing,
        _dmcrsResponseStatus = pResponseStatus_,
        _dmcrsConflictMetadata = pConflictMetadata_,
        _dmcrsMergeHunks = mempty,
        _dmcrsDestinationCommitId = pDestinationCommitId_,
        _dmcrsSourceCommitId = pSourceCommitId_
      }

-- | The commit ID of the merge base.
dmcrsBaseCommitId :: Lens' DescribeMergeConflictsResponse (Maybe Text)
dmcrsBaseCommitId = lens _dmcrsBaseCommitId (\s a -> s {_dmcrsBaseCommitId = a})

-- | An enumeration token that can be used in a request to return the next batch of the results.
dmcrsNextToken :: Lens' DescribeMergeConflictsResponse (Maybe Text)
dmcrsNextToken = lens _dmcrsNextToken (\s a -> s {_dmcrsNextToken = a})

-- | -- | The response status code.
dmcrsResponseStatus :: Lens' DescribeMergeConflictsResponse Int
dmcrsResponseStatus = lens _dmcrsResponseStatus (\s a -> s {_dmcrsResponseStatus = a})

-- | Contains metadata about the conflicts found in the merge.
dmcrsConflictMetadata :: Lens' DescribeMergeConflictsResponse ConflictMetadata
dmcrsConflictMetadata = lens _dmcrsConflictMetadata (\s a -> s {_dmcrsConflictMetadata = a})

-- | A list of merge hunks of the differences between the files or lines.
dmcrsMergeHunks :: Lens' DescribeMergeConflictsResponse [MergeHunk]
dmcrsMergeHunks = lens _dmcrsMergeHunks (\s a -> s {_dmcrsMergeHunks = a}) . _Coerce

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
dmcrsDestinationCommitId :: Lens' DescribeMergeConflictsResponse Text
dmcrsDestinationCommitId = lens _dmcrsDestinationCommitId (\s a -> s {_dmcrsDestinationCommitId = a})

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
dmcrsSourceCommitId :: Lens' DescribeMergeConflictsResponse Text
dmcrsSourceCommitId = lens _dmcrsSourceCommitId (\s a -> s {_dmcrsSourceCommitId = a})

instance NFData DescribeMergeConflictsResponse
