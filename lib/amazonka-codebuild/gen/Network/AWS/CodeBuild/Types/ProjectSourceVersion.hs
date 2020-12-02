{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectSourceVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectSourceVersion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A source identifier and its corresponding version.
--
--
--
-- /See:/ 'projectSourceVersion' smart constructor.
data ProjectSourceVersion = ProjectSourceVersion'
  { _psvSourceIdentifier ::
      !Text,
    _psvSourceVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectSourceVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psvSourceIdentifier' - An identifier for a source in the build project.
--
-- * 'psvSourceVersion' - The source version for the corresponding source identifier. If specified, must be one of:     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
projectSourceVersion ::
  -- | 'psvSourceIdentifier'
  Text ->
  -- | 'psvSourceVersion'
  Text ->
  ProjectSourceVersion
projectSourceVersion pSourceIdentifier_ pSourceVersion_ =
  ProjectSourceVersion'
    { _psvSourceIdentifier = pSourceIdentifier_,
      _psvSourceVersion = pSourceVersion_
    }

-- | An identifier for a source in the build project.
psvSourceIdentifier :: Lens' ProjectSourceVersion Text
psvSourceIdentifier = lens _psvSourceIdentifier (\s a -> s {_psvSourceIdentifier = a})

-- | The source version for the corresponding source identifier. If specified, must be one of:     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
psvSourceVersion :: Lens' ProjectSourceVersion Text
psvSourceVersion = lens _psvSourceVersion (\s a -> s {_psvSourceVersion = a})

instance FromJSON ProjectSourceVersion where
  parseJSON =
    withObject
      "ProjectSourceVersion"
      ( \x ->
          ProjectSourceVersion'
            <$> (x .: "sourceIdentifier") <*> (x .: "sourceVersion")
      )

instance Hashable ProjectSourceVersion

instance NFData ProjectSourceVersion

instance ToJSON ProjectSourceVersion where
  toJSON ProjectSourceVersion' {..} =
    object
      ( catMaybes
          [ Just ("sourceIdentifier" .= _psvSourceIdentifier),
            Just ("sourceVersion" .= _psvSourceVersion)
          ]
      )
