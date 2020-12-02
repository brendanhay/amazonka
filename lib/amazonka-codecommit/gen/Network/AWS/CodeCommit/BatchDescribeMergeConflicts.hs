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
-- Module      : Network.AWS.CodeCommit.BatchDescribeMergeConflicts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more merge conflicts in the attempted merge of two commit specifiers using the squash or three-way merge strategy.
module Network.AWS.CodeCommit.BatchDescribeMergeConflicts
  ( -- * Creating a Request
    batchDescribeMergeConflicts,
    BatchDescribeMergeConflicts,

    -- * Request Lenses
    bdmcFilePaths,
    bdmcConflictDetailLevel,
    bdmcNextToken,
    bdmcMaxConflictFiles,
    bdmcMaxMergeHunks,
    bdmcConflictResolutionStrategy,
    bdmcRepositoryName,
    bdmcDestinationCommitSpecifier,
    bdmcSourceCommitSpecifier,
    bdmcMergeOption,

    -- * Destructuring the Response
    batchDescribeMergeConflictsResponse,
    BatchDescribeMergeConflictsResponse,

    -- * Response Lenses
    bdmcrsBaseCommitId,
    bdmcrsNextToken,
    bdmcrsErrors,
    bdmcrsResponseStatus,
    bdmcrsConflicts,
    bdmcrsDestinationCommitId,
    bdmcrsSourceCommitId,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDescribeMergeConflicts' smart constructor.
data BatchDescribeMergeConflicts = BatchDescribeMergeConflicts'
  { _bdmcFilePaths ::
      !(Maybe [Text]),
    _bdmcConflictDetailLevel ::
      !( Maybe
           ConflictDetailLevelTypeEnum
       ),
    _bdmcNextToken :: !(Maybe Text),
    _bdmcMaxConflictFiles ::
      !(Maybe Int),
    _bdmcMaxMergeHunks :: !(Maybe Int),
    _bdmcConflictResolutionStrategy ::
      !( Maybe
           ConflictResolutionStrategyTypeEnum
       ),
    _bdmcRepositoryName :: !Text,
    _bdmcDestinationCommitSpecifier ::
      !Text,
    _bdmcSourceCommitSpecifier :: !Text,
    _bdmcMergeOption ::
      !MergeOptionTypeEnum
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDescribeMergeConflicts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmcFilePaths' - The path of the target files used to describe the conflicts. If not specified, the default is all conflict files.
--
-- * 'bdmcConflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- * 'bdmcNextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- * 'bdmcMaxConflictFiles' - The maximum number of files to include in the output.
--
-- * 'bdmcMaxMergeHunks' - The maximum number of merge hunks to include in the output.
--
-- * 'bdmcConflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- * 'bdmcRepositoryName' - The name of the repository that contains the merge conflicts you want to review.
--
-- * 'bdmcDestinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- * 'bdmcSourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- * 'bdmcMergeOption' - The merge option or strategy you want to use to merge the code.
batchDescribeMergeConflicts ::
  -- | 'bdmcRepositoryName'
  Text ->
  -- | 'bdmcDestinationCommitSpecifier'
  Text ->
  -- | 'bdmcSourceCommitSpecifier'
  Text ->
  -- | 'bdmcMergeOption'
  MergeOptionTypeEnum ->
  BatchDescribeMergeConflicts
batchDescribeMergeConflicts
  pRepositoryName_
  pDestinationCommitSpecifier_
  pSourceCommitSpecifier_
  pMergeOption_ =
    BatchDescribeMergeConflicts'
      { _bdmcFilePaths = Nothing,
        _bdmcConflictDetailLevel = Nothing,
        _bdmcNextToken = Nothing,
        _bdmcMaxConflictFiles = Nothing,
        _bdmcMaxMergeHunks = Nothing,
        _bdmcConflictResolutionStrategy = Nothing,
        _bdmcRepositoryName = pRepositoryName_,
        _bdmcDestinationCommitSpecifier = pDestinationCommitSpecifier_,
        _bdmcSourceCommitSpecifier = pSourceCommitSpecifier_,
        _bdmcMergeOption = pMergeOption_
      }

-- | The path of the target files used to describe the conflicts. If not specified, the default is all conflict files.
bdmcFilePaths :: Lens' BatchDescribeMergeConflicts [Text]
bdmcFilePaths = lens _bdmcFilePaths (\s a -> s {_bdmcFilePaths = a}) . _Default . _Coerce

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
bdmcConflictDetailLevel :: Lens' BatchDescribeMergeConflicts (Maybe ConflictDetailLevelTypeEnum)
bdmcConflictDetailLevel = lens _bdmcConflictDetailLevel (\s a -> s {_bdmcConflictDetailLevel = a})

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
bdmcNextToken :: Lens' BatchDescribeMergeConflicts (Maybe Text)
bdmcNextToken = lens _bdmcNextToken (\s a -> s {_bdmcNextToken = a})

-- | The maximum number of files to include in the output.
bdmcMaxConflictFiles :: Lens' BatchDescribeMergeConflicts (Maybe Int)
bdmcMaxConflictFiles = lens _bdmcMaxConflictFiles (\s a -> s {_bdmcMaxConflictFiles = a})

-- | The maximum number of merge hunks to include in the output.
bdmcMaxMergeHunks :: Lens' BatchDescribeMergeConflicts (Maybe Int)
bdmcMaxMergeHunks = lens _bdmcMaxMergeHunks (\s a -> s {_bdmcMaxMergeHunks = a})

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
bdmcConflictResolutionStrategy :: Lens' BatchDescribeMergeConflicts (Maybe ConflictResolutionStrategyTypeEnum)
bdmcConflictResolutionStrategy = lens _bdmcConflictResolutionStrategy (\s a -> s {_bdmcConflictResolutionStrategy = a})

-- | The name of the repository that contains the merge conflicts you want to review.
bdmcRepositoryName :: Lens' BatchDescribeMergeConflicts Text
bdmcRepositoryName = lens _bdmcRepositoryName (\s a -> s {_bdmcRepositoryName = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
bdmcDestinationCommitSpecifier :: Lens' BatchDescribeMergeConflicts Text
bdmcDestinationCommitSpecifier = lens _bdmcDestinationCommitSpecifier (\s a -> s {_bdmcDestinationCommitSpecifier = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
bdmcSourceCommitSpecifier :: Lens' BatchDescribeMergeConflicts Text
bdmcSourceCommitSpecifier = lens _bdmcSourceCommitSpecifier (\s a -> s {_bdmcSourceCommitSpecifier = a})

-- | The merge option or strategy you want to use to merge the code.
bdmcMergeOption :: Lens' BatchDescribeMergeConflicts MergeOptionTypeEnum
bdmcMergeOption = lens _bdmcMergeOption (\s a -> s {_bdmcMergeOption = a})

instance AWSRequest BatchDescribeMergeConflicts where
  type
    Rs BatchDescribeMergeConflicts =
      BatchDescribeMergeConflictsResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          BatchDescribeMergeConflictsResponse'
            <$> (x .?> "baseCommitId")
            <*> (x .?> "nextToken")
            <*> (x .?> "errors" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .?> "conflicts" .!@ mempty)
            <*> (x .:> "destinationCommitId")
            <*> (x .:> "sourceCommitId")
      )

instance Hashable BatchDescribeMergeConflicts

instance NFData BatchDescribeMergeConflicts

instance ToHeaders BatchDescribeMergeConflicts where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.BatchDescribeMergeConflicts" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchDescribeMergeConflicts where
  toJSON BatchDescribeMergeConflicts' {..} =
    object
      ( catMaybes
          [ ("filePaths" .=) <$> _bdmcFilePaths,
            ("conflictDetailLevel" .=) <$> _bdmcConflictDetailLevel,
            ("nextToken" .=) <$> _bdmcNextToken,
            ("maxConflictFiles" .=) <$> _bdmcMaxConflictFiles,
            ("maxMergeHunks" .=) <$> _bdmcMaxMergeHunks,
            ("conflictResolutionStrategy" .=)
              <$> _bdmcConflictResolutionStrategy,
            Just ("repositoryName" .= _bdmcRepositoryName),
            Just
              ("destinationCommitSpecifier" .= _bdmcDestinationCommitSpecifier),
            Just ("sourceCommitSpecifier" .= _bdmcSourceCommitSpecifier),
            Just ("mergeOption" .= _bdmcMergeOption)
          ]
      )

instance ToPath BatchDescribeMergeConflicts where
  toPath = const "/"

instance ToQuery BatchDescribeMergeConflicts where
  toQuery = const mempty

-- | /See:/ 'batchDescribeMergeConflictsResponse' smart constructor.
data BatchDescribeMergeConflictsResponse = BatchDescribeMergeConflictsResponse'
  { _bdmcrsBaseCommitId ::
      !(Maybe Text),
    _bdmcrsNextToken ::
      !(Maybe Text),
    _bdmcrsErrors ::
      !( Maybe
           [BatchDescribeMergeConflictsError]
       ),
    _bdmcrsResponseStatus ::
      !Int,
    _bdmcrsConflicts ::
      ![Conflict],
    _bdmcrsDestinationCommitId ::
      !Text,
    _bdmcrsSourceCommitId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDescribeMergeConflictsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmcrsBaseCommitId' - The commit ID of the merge base.
--
-- * 'bdmcrsNextToken' - An enumeration token that can be used in a request to return the next batch of the results.
--
-- * 'bdmcrsErrors' - A list of any errors returned while describing the merge conflicts for each file.
--
-- * 'bdmcrsResponseStatus' - -- | The response status code.
--
-- * 'bdmcrsConflicts' - A list of conflicts for each file, including the conflict metadata and the hunks of the differences between the files.
--
-- * 'bdmcrsDestinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- * 'bdmcrsSourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
batchDescribeMergeConflictsResponse ::
  -- | 'bdmcrsResponseStatus'
  Int ->
  -- | 'bdmcrsDestinationCommitId'
  Text ->
  -- | 'bdmcrsSourceCommitId'
  Text ->
  BatchDescribeMergeConflictsResponse
batchDescribeMergeConflictsResponse
  pResponseStatus_
  pDestinationCommitId_
  pSourceCommitId_ =
    BatchDescribeMergeConflictsResponse'
      { _bdmcrsBaseCommitId =
          Nothing,
        _bdmcrsNextToken = Nothing,
        _bdmcrsErrors = Nothing,
        _bdmcrsResponseStatus = pResponseStatus_,
        _bdmcrsConflicts = mempty,
        _bdmcrsDestinationCommitId = pDestinationCommitId_,
        _bdmcrsSourceCommitId = pSourceCommitId_
      }

-- | The commit ID of the merge base.
bdmcrsBaseCommitId :: Lens' BatchDescribeMergeConflictsResponse (Maybe Text)
bdmcrsBaseCommitId = lens _bdmcrsBaseCommitId (\s a -> s {_bdmcrsBaseCommitId = a})

-- | An enumeration token that can be used in a request to return the next batch of the results.
bdmcrsNextToken :: Lens' BatchDescribeMergeConflictsResponse (Maybe Text)
bdmcrsNextToken = lens _bdmcrsNextToken (\s a -> s {_bdmcrsNextToken = a})

-- | A list of any errors returned while describing the merge conflicts for each file.
bdmcrsErrors :: Lens' BatchDescribeMergeConflictsResponse [BatchDescribeMergeConflictsError]
bdmcrsErrors = lens _bdmcrsErrors (\s a -> s {_bdmcrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bdmcrsResponseStatus :: Lens' BatchDescribeMergeConflictsResponse Int
bdmcrsResponseStatus = lens _bdmcrsResponseStatus (\s a -> s {_bdmcrsResponseStatus = a})

-- | A list of conflicts for each file, including the conflict metadata and the hunks of the differences between the files.
bdmcrsConflicts :: Lens' BatchDescribeMergeConflictsResponse [Conflict]
bdmcrsConflicts = lens _bdmcrsConflicts (\s a -> s {_bdmcrsConflicts = a}) . _Coerce

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
bdmcrsDestinationCommitId :: Lens' BatchDescribeMergeConflictsResponse Text
bdmcrsDestinationCommitId = lens _bdmcrsDestinationCommitId (\s a -> s {_bdmcrsDestinationCommitId = a})

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
bdmcrsSourceCommitId :: Lens' BatchDescribeMergeConflictsResponse Text
bdmcrsSourceCommitId = lens _bdmcrsSourceCommitId (\s a -> s {_bdmcrsSourceCommitId = a})

instance NFData BatchDescribeMergeConflictsResponse
