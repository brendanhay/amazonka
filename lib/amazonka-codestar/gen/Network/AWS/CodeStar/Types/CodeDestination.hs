{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.CodeDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.CodeDestination where

import Network.AWS.CodeStar.Types.CodeCommitCodeDestination
import Network.AWS.CodeStar.Types.GitHubCodeDestination
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
--
--
--
-- /See:/ 'codeDestination' smart constructor.
data CodeDestination = CodeDestination'
  { _cdCodeCommit ::
      !(Maybe CodeCommitCodeDestination),
    _cdGitHub :: !(Maybe GitHubCodeDestination)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdCodeCommit' - Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- * 'cdGitHub' - Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
codeDestination ::
  CodeDestination
codeDestination =
  CodeDestination' {_cdCodeCommit = Nothing, _cdGitHub = Nothing}

-- | Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
cdCodeCommit :: Lens' CodeDestination (Maybe CodeCommitCodeDestination)
cdCodeCommit = lens _cdCodeCommit (\s a -> s {_cdCodeCommit = a})

-- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
cdGitHub :: Lens' CodeDestination (Maybe GitHubCodeDestination)
cdGitHub = lens _cdGitHub (\s a -> s {_cdGitHub = a})

instance Hashable CodeDestination

instance NFData CodeDestination

instance ToJSON CodeDestination where
  toJSON CodeDestination' {..} =
    object
      ( catMaybes
          [("codeCommit" .=) <$> _cdCodeCommit, ("gitHub" .=) <$> _cdGitHub]
      )
