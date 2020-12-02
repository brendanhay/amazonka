{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata where

import Network.AWS.CodeCommit.Types.PullRequestStatusEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a change to the status of a pull request.
--
--
--
-- /See:/ 'pullRequestStatusChangedEventMetadata' smart constructor.
newtype PullRequestStatusChangedEventMetadata = PullRequestStatusChangedEventMetadata'
  { _prscemPullRequestStatus ::
      Maybe
        PullRequestStatusEnum
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PullRequestStatusChangedEventMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prscemPullRequestStatus' - The changed status of the pull request.
pullRequestStatusChangedEventMetadata ::
  PullRequestStatusChangedEventMetadata
pullRequestStatusChangedEventMetadata =
  PullRequestStatusChangedEventMetadata'
    { _prscemPullRequestStatus =
        Nothing
    }

-- | The changed status of the pull request.
prscemPullRequestStatus :: Lens' PullRequestStatusChangedEventMetadata (Maybe PullRequestStatusEnum)
prscemPullRequestStatus = lens _prscemPullRequestStatus (\s a -> s {_prscemPullRequestStatus = a})

instance FromJSON PullRequestStatusChangedEventMetadata where
  parseJSON =
    withObject
      "PullRequestStatusChangedEventMetadata"
      ( \x ->
          PullRequestStatusChangedEventMetadata'
            <$> (x .:? "pullRequestStatus")
      )

instance Hashable PullRequestStatusChangedEventMetadata

instance NFData PullRequestStatusChangedEventMetadata
