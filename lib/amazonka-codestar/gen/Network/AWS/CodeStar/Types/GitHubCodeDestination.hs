{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.GitHubCodeDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.GitHubCodeDestination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
--
--
-- /See:/ 'gitHubCodeDestination' smart constructor.
data GitHubCodeDestination = GitHubCodeDestination'
  { _ghcdDescription ::
      !(Maybe Text),
    _ghcdName :: !Text,
    _ghcdType :: !Text,
    _ghcdOwner :: !Text,
    _ghcdPrivateRepository :: !Bool,
    _ghcdIssuesEnabled :: !Bool,
    _ghcdToken :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GitHubCodeDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghcdDescription' - Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
--
-- * 'ghcdName' - Name of the GitHub repository to be created in AWS CodeStar.
--
-- * 'ghcdType' - The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
--
-- * 'ghcdOwner' - The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
--
-- * 'ghcdPrivateRepository' - Whether the GitHub repository is to be a private repository.
--
-- * 'ghcdIssuesEnabled' - Whether to enable issues for the GitHub repository.
--
-- * 'ghcdToken' - The GitHub user's personal access token for the GitHub repository.
gitHubCodeDestination ::
  -- | 'ghcdName'
  Text ->
  -- | 'ghcdType'
  Text ->
  -- | 'ghcdOwner'
  Text ->
  -- | 'ghcdPrivateRepository'
  Bool ->
  -- | 'ghcdIssuesEnabled'
  Bool ->
  -- | 'ghcdToken'
  Text ->
  GitHubCodeDestination
gitHubCodeDestination
  pName_
  pType_
  pOwner_
  pPrivateRepository_
  pIssuesEnabled_
  pToken_ =
    GitHubCodeDestination'
      { _ghcdDescription = Nothing,
        _ghcdName = pName_,
        _ghcdType = pType_,
        _ghcdOwner = pOwner_,
        _ghcdPrivateRepository = pPrivateRepository_,
        _ghcdIssuesEnabled = pIssuesEnabled_,
        _ghcdToken = _Sensitive # pToken_
      }

-- | Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
ghcdDescription :: Lens' GitHubCodeDestination (Maybe Text)
ghcdDescription = lens _ghcdDescription (\s a -> s {_ghcdDescription = a})

-- | Name of the GitHub repository to be created in AWS CodeStar.
ghcdName :: Lens' GitHubCodeDestination Text
ghcdName = lens _ghcdName (\s a -> s {_ghcdName = a})

-- | The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
ghcdType :: Lens' GitHubCodeDestination Text
ghcdType = lens _ghcdType (\s a -> s {_ghcdType = a})

-- | The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
ghcdOwner :: Lens' GitHubCodeDestination Text
ghcdOwner = lens _ghcdOwner (\s a -> s {_ghcdOwner = a})

-- | Whether the GitHub repository is to be a private repository.
ghcdPrivateRepository :: Lens' GitHubCodeDestination Bool
ghcdPrivateRepository = lens _ghcdPrivateRepository (\s a -> s {_ghcdPrivateRepository = a})

-- | Whether to enable issues for the GitHub repository.
ghcdIssuesEnabled :: Lens' GitHubCodeDestination Bool
ghcdIssuesEnabled = lens _ghcdIssuesEnabled (\s a -> s {_ghcdIssuesEnabled = a})

-- | The GitHub user's personal access token for the GitHub repository.
ghcdToken :: Lens' GitHubCodeDestination Text
ghcdToken = lens _ghcdToken (\s a -> s {_ghcdToken = a}) . _Sensitive

instance Hashable GitHubCodeDestination

instance NFData GitHubCodeDestination

instance ToJSON GitHubCodeDestination where
  toJSON GitHubCodeDestination' {..} =
    object
      ( catMaybes
          [ ("description" .=) <$> _ghcdDescription,
            Just ("name" .= _ghcdName),
            Just ("type" .= _ghcdType),
            Just ("owner" .= _ghcdOwner),
            Just ("privateRepository" .= _ghcdPrivateRepository),
            Just ("issuesEnabled" .= _ghcdIssuesEnabled),
            Just ("token" .= _ghcdToken)
          ]
      )
