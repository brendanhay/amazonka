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
-- Module      : Network.AWS.CodeCommit.GetMergeOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the merge options available for merging two specified branches. For details about why a merge option is not available, use GetMergeConflicts or DescribeMergeConflicts.
module Network.AWS.CodeCommit.GetMergeOptions
  ( -- * Creating a Request
    getMergeOptions,
    GetMergeOptions,

    -- * Request Lenses
    gmoConflictDetailLevel,
    gmoConflictResolutionStrategy,
    gmoRepositoryName,
    gmoSourceCommitSpecifier,
    gmoDestinationCommitSpecifier,

    -- * Destructuring the Response
    getMergeOptionsResponse,
    GetMergeOptionsResponse,

    -- * Response Lenses
    gmorsResponseStatus,
    gmorsMergeOptions,
    gmorsSourceCommitId,
    gmorsDestinationCommitId,
    gmorsBaseCommitId,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMergeOptions' smart constructor.
data GetMergeOptions = GetMergeOptions'
  { _gmoConflictDetailLevel ::
      !(Maybe ConflictDetailLevelTypeEnum),
    _gmoConflictResolutionStrategy ::
      !(Maybe ConflictResolutionStrategyTypeEnum),
    _gmoRepositoryName :: !Text,
    _gmoSourceCommitSpecifier :: !Text,
    _gmoDestinationCommitSpecifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMergeOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmoConflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- * 'gmoConflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- * 'gmoRepositoryName' - The name of the repository that contains the commits about which you want to get merge options.
--
-- * 'gmoSourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- * 'gmoDestinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
getMergeOptions ::
  -- | 'gmoRepositoryName'
  Text ->
  -- | 'gmoSourceCommitSpecifier'
  Text ->
  -- | 'gmoDestinationCommitSpecifier'
  Text ->
  GetMergeOptions
getMergeOptions
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    GetMergeOptions'
      { _gmoConflictDetailLevel = Nothing,
        _gmoConflictResolutionStrategy = Nothing,
        _gmoRepositoryName = pRepositoryName_,
        _gmoSourceCommitSpecifier = pSourceCommitSpecifier_,
        _gmoDestinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
gmoConflictDetailLevel :: Lens' GetMergeOptions (Maybe ConflictDetailLevelTypeEnum)
gmoConflictDetailLevel = lens _gmoConflictDetailLevel (\s a -> s {_gmoConflictDetailLevel = a})

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
gmoConflictResolutionStrategy :: Lens' GetMergeOptions (Maybe ConflictResolutionStrategyTypeEnum)
gmoConflictResolutionStrategy = lens _gmoConflictResolutionStrategy (\s a -> s {_gmoConflictResolutionStrategy = a})

-- | The name of the repository that contains the commits about which you want to get merge options.
gmoRepositoryName :: Lens' GetMergeOptions Text
gmoRepositoryName = lens _gmoRepositoryName (\s a -> s {_gmoRepositoryName = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
gmoSourceCommitSpecifier :: Lens' GetMergeOptions Text
gmoSourceCommitSpecifier = lens _gmoSourceCommitSpecifier (\s a -> s {_gmoSourceCommitSpecifier = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
gmoDestinationCommitSpecifier :: Lens' GetMergeOptions Text
gmoDestinationCommitSpecifier = lens _gmoDestinationCommitSpecifier (\s a -> s {_gmoDestinationCommitSpecifier = a})

instance AWSRequest GetMergeOptions where
  type Rs GetMergeOptions = GetMergeOptionsResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          GetMergeOptionsResponse'
            <$> (pure (fromEnum s))
            <*> (x .?> "mergeOptions" .!@ mempty)
            <*> (x .:> "sourceCommitId")
            <*> (x .:> "destinationCommitId")
            <*> (x .:> "baseCommitId")
      )

instance Hashable GetMergeOptions

instance NFData GetMergeOptions

instance ToHeaders GetMergeOptions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.GetMergeOptions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMergeOptions where
  toJSON GetMergeOptions' {..} =
    object
      ( catMaybes
          [ ("conflictDetailLevel" .=) <$> _gmoConflictDetailLevel,
            ("conflictResolutionStrategy" .=)
              <$> _gmoConflictResolutionStrategy,
            Just ("repositoryName" .= _gmoRepositoryName),
            Just ("sourceCommitSpecifier" .= _gmoSourceCommitSpecifier),
            Just
              ("destinationCommitSpecifier" .= _gmoDestinationCommitSpecifier)
          ]
      )

instance ToPath GetMergeOptions where
  toPath = const "/"

instance ToQuery GetMergeOptions where
  toQuery = const mempty

-- | /See:/ 'getMergeOptionsResponse' smart constructor.
data GetMergeOptionsResponse = GetMergeOptionsResponse'
  { _gmorsResponseStatus ::
      !Int,
    _gmorsMergeOptions ::
      ![MergeOptionTypeEnum],
    _gmorsSourceCommitId :: !Text,
    _gmorsDestinationCommitId :: !Text,
    _gmorsBaseCommitId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMergeOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmorsResponseStatus' - -- | The response status code.
--
-- * 'gmorsMergeOptions' - The merge option or strategy used to merge the code.
--
-- * 'gmorsSourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- * 'gmorsDestinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- * 'gmorsBaseCommitId' - The commit ID of the merge base.
getMergeOptionsResponse ::
  -- | 'gmorsResponseStatus'
  Int ->
  -- | 'gmorsSourceCommitId'
  Text ->
  -- | 'gmorsDestinationCommitId'
  Text ->
  -- | 'gmorsBaseCommitId'
  Text ->
  GetMergeOptionsResponse
getMergeOptionsResponse
  pResponseStatus_
  pSourceCommitId_
  pDestinationCommitId_
  pBaseCommitId_ =
    GetMergeOptionsResponse'
      { _gmorsResponseStatus = pResponseStatus_,
        _gmorsMergeOptions = mempty,
        _gmorsSourceCommitId = pSourceCommitId_,
        _gmorsDestinationCommitId = pDestinationCommitId_,
        _gmorsBaseCommitId = pBaseCommitId_
      }

-- | -- | The response status code.
gmorsResponseStatus :: Lens' GetMergeOptionsResponse Int
gmorsResponseStatus = lens _gmorsResponseStatus (\s a -> s {_gmorsResponseStatus = a})

-- | The merge option or strategy used to merge the code.
gmorsMergeOptions :: Lens' GetMergeOptionsResponse [MergeOptionTypeEnum]
gmorsMergeOptions = lens _gmorsMergeOptions (\s a -> s {_gmorsMergeOptions = a}) . _Coerce

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
gmorsSourceCommitId :: Lens' GetMergeOptionsResponse Text
gmorsSourceCommitId = lens _gmorsSourceCommitId (\s a -> s {_gmorsSourceCommitId = a})

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
gmorsDestinationCommitId :: Lens' GetMergeOptionsResponse Text
gmorsDestinationCommitId = lens _gmorsDestinationCommitId (\s a -> s {_gmorsDestinationCommitId = a})

-- | The commit ID of the merge base.
gmorsBaseCommitId :: Lens' GetMergeOptionsResponse Text
gmorsBaseCommitId = lens _gmorsBaseCommitId (\s a -> s {_gmorsBaseCommitId = a})

instance NFData GetMergeOptionsResponse
