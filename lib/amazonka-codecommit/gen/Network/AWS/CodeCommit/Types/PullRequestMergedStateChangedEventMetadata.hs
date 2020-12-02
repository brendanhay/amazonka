{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata where

import Network.AWS.CodeCommit.Types.MergeMetadata
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about the change in the merge state for a pull request event.
--
--
--
-- /See:/ 'pullRequestMergedStateChangedEventMetadata' smart constructor.
data PullRequestMergedStateChangedEventMetadata = PullRequestMergedStateChangedEventMetadata'
  { _prmscemDestinationReference ::
      !( Maybe
           Text
       ),
    _prmscemMergeMetadata ::
      !( Maybe
           MergeMetadata
       ),
    _prmscemRepositoryName ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'PullRequestMergedStateChangedEventMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prmscemDestinationReference' - The name of the branch that the pull request is merged into.
--
-- * 'prmscemMergeMetadata' - Information about the merge state change event.
--
-- * 'prmscemRepositoryName' - The name of the repository where the pull request was created.
pullRequestMergedStateChangedEventMetadata ::
  PullRequestMergedStateChangedEventMetadata
pullRequestMergedStateChangedEventMetadata =
  PullRequestMergedStateChangedEventMetadata'
    { _prmscemDestinationReference =
        Nothing,
      _prmscemMergeMetadata = Nothing,
      _prmscemRepositoryName = Nothing
    }

-- | The name of the branch that the pull request is merged into.
prmscemDestinationReference :: Lens' PullRequestMergedStateChangedEventMetadata (Maybe Text)
prmscemDestinationReference = lens _prmscemDestinationReference (\s a -> s {_prmscemDestinationReference = a})

-- | Information about the merge state change event.
prmscemMergeMetadata :: Lens' PullRequestMergedStateChangedEventMetadata (Maybe MergeMetadata)
prmscemMergeMetadata = lens _prmscemMergeMetadata (\s a -> s {_prmscemMergeMetadata = a})

-- | The name of the repository where the pull request was created.
prmscemRepositoryName :: Lens' PullRequestMergedStateChangedEventMetadata (Maybe Text)
prmscemRepositoryName = lens _prmscemRepositoryName (\s a -> s {_prmscemRepositoryName = a})

instance FromJSON PullRequestMergedStateChangedEventMetadata where
  parseJSON =
    withObject
      "PullRequestMergedStateChangedEventMetadata"
      ( \x ->
          PullRequestMergedStateChangedEventMetadata'
            <$> (x .:? "destinationReference")
            <*> (x .:? "mergeMetadata")
            <*> (x .:? "repositoryName")
      )

instance Hashable PullRequestMergedStateChangedEventMetadata

instance NFData PullRequestMergedStateChangedEventMetadata
