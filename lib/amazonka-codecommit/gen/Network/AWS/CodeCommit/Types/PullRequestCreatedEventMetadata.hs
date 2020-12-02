{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Metadata about the pull request that is used when comparing the pull request source with its destination.
--
--
--
-- /See:/ 'pullRequestCreatedEventMetadata' smart constructor.
data PullRequestCreatedEventMetadata = PullRequestCreatedEventMetadata'
  { _prcemDestinationCommitId ::
      !(Maybe Text),
    _prcemMergeBase ::
      !(Maybe Text),
    _prcemRepositoryName ::
      !(Maybe Text),
    _prcemSourceCommitId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PullRequestCreatedEventMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prcemDestinationCommitId' - The commit ID of the tip of the branch specified as the destination branch when the pull request was created.
--
-- * 'prcemMergeBase' - The commit ID of the most recent commit that the source branch and the destination branch have in common.
--
-- * 'prcemRepositoryName' - The name of the repository where the pull request was created.
--
-- * 'prcemSourceCommitId' - The commit ID on the source branch used when the pull request was created.
pullRequestCreatedEventMetadata ::
  PullRequestCreatedEventMetadata
pullRequestCreatedEventMetadata =
  PullRequestCreatedEventMetadata'
    { _prcemDestinationCommitId =
        Nothing,
      _prcemMergeBase = Nothing,
      _prcemRepositoryName = Nothing,
      _prcemSourceCommitId = Nothing
    }

-- | The commit ID of the tip of the branch specified as the destination branch when the pull request was created.
prcemDestinationCommitId :: Lens' PullRequestCreatedEventMetadata (Maybe Text)
prcemDestinationCommitId = lens _prcemDestinationCommitId (\s a -> s {_prcemDestinationCommitId = a})

-- | The commit ID of the most recent commit that the source branch and the destination branch have in common.
prcemMergeBase :: Lens' PullRequestCreatedEventMetadata (Maybe Text)
prcemMergeBase = lens _prcemMergeBase (\s a -> s {_prcemMergeBase = a})

-- | The name of the repository where the pull request was created.
prcemRepositoryName :: Lens' PullRequestCreatedEventMetadata (Maybe Text)
prcemRepositoryName = lens _prcemRepositoryName (\s a -> s {_prcemRepositoryName = a})

-- | The commit ID on the source branch used when the pull request was created.
prcemSourceCommitId :: Lens' PullRequestCreatedEventMetadata (Maybe Text)
prcemSourceCommitId = lens _prcemSourceCommitId (\s a -> s {_prcemSourceCommitId = a})

instance FromJSON PullRequestCreatedEventMetadata where
  parseJSON =
    withObject
      "PullRequestCreatedEventMetadata"
      ( \x ->
          PullRequestCreatedEventMetadata'
            <$> (x .:? "destinationCommitId")
            <*> (x .:? "mergeBase")
            <*> (x .:? "repositoryName")
            <*> (x .:? "sourceCommitId")
      )

instance Hashable PullRequestCreatedEventMetadata

instance NFData PullRequestCreatedEventMetadata
