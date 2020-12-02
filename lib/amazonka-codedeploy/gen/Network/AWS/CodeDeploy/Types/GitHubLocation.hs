{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GitHubLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GitHubLocation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the location of application artifacts stored in GitHub.
--
--
--
-- /See:/ 'gitHubLocation' smart constructor.
data GitHubLocation = GitHubLocation'
  { _ghlCommitId ::
      !(Maybe Text),
    _ghlRepository :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GitHubLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghlCommitId' - The SHA1 commit ID of the GitHub commit that represents the bundled artifacts for the application revision.
--
-- * 'ghlRepository' - The GitHub account and repository pair that stores a reference to the commit that represents the bundled artifacts for the application revision.  Specified as account/repository.
gitHubLocation ::
  GitHubLocation
gitHubLocation =
  GitHubLocation' {_ghlCommitId = Nothing, _ghlRepository = Nothing}

-- | The SHA1 commit ID of the GitHub commit that represents the bundled artifacts for the application revision.
ghlCommitId :: Lens' GitHubLocation (Maybe Text)
ghlCommitId = lens _ghlCommitId (\s a -> s {_ghlCommitId = a})

-- | The GitHub account and repository pair that stores a reference to the commit that represents the bundled artifacts for the application revision.  Specified as account/repository.
ghlRepository :: Lens' GitHubLocation (Maybe Text)
ghlRepository = lens _ghlRepository (\s a -> s {_ghlRepository = a})

instance FromJSON GitHubLocation where
  parseJSON =
    withObject
      "GitHubLocation"
      ( \x ->
          GitHubLocation' <$> (x .:? "commitId") <*> (x .:? "repository")
      )

instance Hashable GitHubLocation

instance NFData GitHubLocation

instance ToJSON GitHubLocation where
  toJSON GitHubLocation' {..} =
    object
      ( catMaybes
          [ ("commitId" .=) <$> _ghlCommitId,
            ("repository" .=) <$> _ghlRepository
          ]
      )
