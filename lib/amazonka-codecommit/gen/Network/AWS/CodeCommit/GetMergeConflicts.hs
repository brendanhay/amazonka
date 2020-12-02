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
-- Module      : Network.AWS.CodeCommit.GetMergeConflicts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about merge conflicts between the before and after commit IDs for a pull request in a repository.
module Network.AWS.CodeCommit.GetMergeConflicts
  ( -- * Creating a Request
    getMergeConflicts,
    GetMergeConflicts,

    -- * Request Lenses
    gmcsConflictDetailLevel,
    gmcsNextToken,
    gmcsMaxConflictFiles,
    gmcsConflictResolutionStrategy,
    gmcsRepositoryName,
    gmcsDestinationCommitSpecifier,
    gmcsSourceCommitSpecifier,
    gmcsMergeOption,

    -- * Destructuring the Response
    getMergeConflictsResponse,
    GetMergeConflictsResponse,

    -- * Response Lenses
    gmcsrsBaseCommitId,
    gmcsrsNextToken,
    gmcsrsResponseStatus,
    gmcsrsMergeable,
    gmcsrsDestinationCommitId,
    gmcsrsSourceCommitId,
    gmcsrsConflictMetadataList,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMergeConflicts' smart constructor.
data GetMergeConflicts = GetMergeConflicts'
  { _gmcsConflictDetailLevel ::
      !(Maybe ConflictDetailLevelTypeEnum),
    _gmcsNextToken :: !(Maybe Text),
    _gmcsMaxConflictFiles :: !(Maybe Int),
    _gmcsConflictResolutionStrategy ::
      !(Maybe ConflictResolutionStrategyTypeEnum),
    _gmcsRepositoryName :: !Text,
    _gmcsDestinationCommitSpecifier :: !Text,
    _gmcsSourceCommitSpecifier :: !Text,
    _gmcsMergeOption :: !MergeOptionTypeEnum
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMergeConflicts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmcsConflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- * 'gmcsNextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- * 'gmcsMaxConflictFiles' - The maximum number of files to include in the output.
--
-- * 'gmcsConflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- * 'gmcsRepositoryName' - The name of the repository where the pull request was created.
--
-- * 'gmcsDestinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- * 'gmcsSourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- * 'gmcsMergeOption' - The merge option or strategy you want to use to merge the code.
getMergeConflicts ::
  -- | 'gmcsRepositoryName'
  Text ->
  -- | 'gmcsDestinationCommitSpecifier'
  Text ->
  -- | 'gmcsSourceCommitSpecifier'
  Text ->
  -- | 'gmcsMergeOption'
  MergeOptionTypeEnum ->
  GetMergeConflicts
getMergeConflicts
  pRepositoryName_
  pDestinationCommitSpecifier_
  pSourceCommitSpecifier_
  pMergeOption_ =
    GetMergeConflicts'
      { _gmcsConflictDetailLevel = Nothing,
        _gmcsNextToken = Nothing,
        _gmcsMaxConflictFiles = Nothing,
        _gmcsConflictResolutionStrategy = Nothing,
        _gmcsRepositoryName = pRepositoryName_,
        _gmcsDestinationCommitSpecifier = pDestinationCommitSpecifier_,
        _gmcsSourceCommitSpecifier = pSourceCommitSpecifier_,
        _gmcsMergeOption = pMergeOption_
      }

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
gmcsConflictDetailLevel :: Lens' GetMergeConflicts (Maybe ConflictDetailLevelTypeEnum)
gmcsConflictDetailLevel = lens _gmcsConflictDetailLevel (\s a -> s {_gmcsConflictDetailLevel = a})

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
gmcsNextToken :: Lens' GetMergeConflicts (Maybe Text)
gmcsNextToken = lens _gmcsNextToken (\s a -> s {_gmcsNextToken = a})

-- | The maximum number of files to include in the output.
gmcsMaxConflictFiles :: Lens' GetMergeConflicts (Maybe Int)
gmcsMaxConflictFiles = lens _gmcsMaxConflictFiles (\s a -> s {_gmcsMaxConflictFiles = a})

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
gmcsConflictResolutionStrategy :: Lens' GetMergeConflicts (Maybe ConflictResolutionStrategyTypeEnum)
gmcsConflictResolutionStrategy = lens _gmcsConflictResolutionStrategy (\s a -> s {_gmcsConflictResolutionStrategy = a})

-- | The name of the repository where the pull request was created.
gmcsRepositoryName :: Lens' GetMergeConflicts Text
gmcsRepositoryName = lens _gmcsRepositoryName (\s a -> s {_gmcsRepositoryName = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
gmcsDestinationCommitSpecifier :: Lens' GetMergeConflicts Text
gmcsDestinationCommitSpecifier = lens _gmcsDestinationCommitSpecifier (\s a -> s {_gmcsDestinationCommitSpecifier = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
gmcsSourceCommitSpecifier :: Lens' GetMergeConflicts Text
gmcsSourceCommitSpecifier = lens _gmcsSourceCommitSpecifier (\s a -> s {_gmcsSourceCommitSpecifier = a})

-- | The merge option or strategy you want to use to merge the code.
gmcsMergeOption :: Lens' GetMergeConflicts MergeOptionTypeEnum
gmcsMergeOption = lens _gmcsMergeOption (\s a -> s {_gmcsMergeOption = a})

instance AWSRequest GetMergeConflicts where
  type Rs GetMergeConflicts = GetMergeConflictsResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          GetMergeConflictsResponse'
            <$> (x .?> "baseCommitId")
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
            <*> (x .:> "mergeable")
            <*> (x .:> "destinationCommitId")
            <*> (x .:> "sourceCommitId")
            <*> (x .?> "conflictMetadataList" .!@ mempty)
      )

instance Hashable GetMergeConflicts

instance NFData GetMergeConflicts

instance ToHeaders GetMergeConflicts where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.GetMergeConflicts" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMergeConflicts where
  toJSON GetMergeConflicts' {..} =
    object
      ( catMaybes
          [ ("conflictDetailLevel" .=) <$> _gmcsConflictDetailLevel,
            ("nextToken" .=) <$> _gmcsNextToken,
            ("maxConflictFiles" .=) <$> _gmcsMaxConflictFiles,
            ("conflictResolutionStrategy" .=)
              <$> _gmcsConflictResolutionStrategy,
            Just ("repositoryName" .= _gmcsRepositoryName),
            Just
              ("destinationCommitSpecifier" .= _gmcsDestinationCommitSpecifier),
            Just ("sourceCommitSpecifier" .= _gmcsSourceCommitSpecifier),
            Just ("mergeOption" .= _gmcsMergeOption)
          ]
      )

instance ToPath GetMergeConflicts where
  toPath = const "/"

instance ToQuery GetMergeConflicts where
  toQuery = const mempty

-- | /See:/ 'getMergeConflictsResponse' smart constructor.
data GetMergeConflictsResponse = GetMergeConflictsResponse'
  { _gmcsrsBaseCommitId ::
      !(Maybe Text),
    _gmcsrsNextToken :: !(Maybe Text),
    _gmcsrsResponseStatus :: !Int,
    _gmcsrsMergeable :: !Bool,
    _gmcsrsDestinationCommitId :: !Text,
    _gmcsrsSourceCommitId :: !Text,
    _gmcsrsConflictMetadataList ::
      ![ConflictMetadata]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMergeConflictsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmcsrsBaseCommitId' - The commit ID of the merge base.
--
-- * 'gmcsrsNextToken' - An enumeration token that can be used in a request to return the next batch of the results.
--
-- * 'gmcsrsResponseStatus' - -- | The response status code.
--
-- * 'gmcsrsMergeable' - A Boolean value that indicates whether the code is mergeable by the specified merge option.
--
-- * 'gmcsrsDestinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- * 'gmcsrsSourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- * 'gmcsrsConflictMetadataList' - A list of metadata for any conflicting files. If the specified merge strategy is FAST_FORWARD_MERGE, this list is always empty.
getMergeConflictsResponse ::
  -- | 'gmcsrsResponseStatus'
  Int ->
  -- | 'gmcsrsMergeable'
  Bool ->
  -- | 'gmcsrsDestinationCommitId'
  Text ->
  -- | 'gmcsrsSourceCommitId'
  Text ->
  GetMergeConflictsResponse
getMergeConflictsResponse
  pResponseStatus_
  pMergeable_
  pDestinationCommitId_
  pSourceCommitId_ =
    GetMergeConflictsResponse'
      { _gmcsrsBaseCommitId = Nothing,
        _gmcsrsNextToken = Nothing,
        _gmcsrsResponseStatus = pResponseStatus_,
        _gmcsrsMergeable = pMergeable_,
        _gmcsrsDestinationCommitId = pDestinationCommitId_,
        _gmcsrsSourceCommitId = pSourceCommitId_,
        _gmcsrsConflictMetadataList = mempty
      }

-- | The commit ID of the merge base.
gmcsrsBaseCommitId :: Lens' GetMergeConflictsResponse (Maybe Text)
gmcsrsBaseCommitId = lens _gmcsrsBaseCommitId (\s a -> s {_gmcsrsBaseCommitId = a})

-- | An enumeration token that can be used in a request to return the next batch of the results.
gmcsrsNextToken :: Lens' GetMergeConflictsResponse (Maybe Text)
gmcsrsNextToken = lens _gmcsrsNextToken (\s a -> s {_gmcsrsNextToken = a})

-- | -- | The response status code.
gmcsrsResponseStatus :: Lens' GetMergeConflictsResponse Int
gmcsrsResponseStatus = lens _gmcsrsResponseStatus (\s a -> s {_gmcsrsResponseStatus = a})

-- | A Boolean value that indicates whether the code is mergeable by the specified merge option.
gmcsrsMergeable :: Lens' GetMergeConflictsResponse Bool
gmcsrsMergeable = lens _gmcsrsMergeable (\s a -> s {_gmcsrsMergeable = a})

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
gmcsrsDestinationCommitId :: Lens' GetMergeConflictsResponse Text
gmcsrsDestinationCommitId = lens _gmcsrsDestinationCommitId (\s a -> s {_gmcsrsDestinationCommitId = a})

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
gmcsrsSourceCommitId :: Lens' GetMergeConflictsResponse Text
gmcsrsSourceCommitId = lens _gmcsrsSourceCommitId (\s a -> s {_gmcsrsSourceCommitId = a})

-- | A list of metadata for any conflicting files. If the specified merge strategy is FAST_FORWARD_MERGE, this list is always empty.
gmcsrsConflictMetadataList :: Lens' GetMergeConflictsResponse [ConflictMetadata]
gmcsrsConflictMetadataList = lens _gmcsrsConflictMetadataList (\s a -> s {_gmcsrsConflictMetadataList = a}) . _Coerce

instance NFData GetMergeConflictsResponse
