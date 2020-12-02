{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.StartBuild
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts running a build.
--
--
module Network.AWS.CodeBuild.StartBuild
    (
    -- * Creating a Request
      startBuild
    , StartBuild
    -- * Request Lenses
    , sbEnvironmentVariablesOverride
    , sbTimeoutInMinutesOverride
    , sbGitCloneDepthOverride
    , sbSourceVersion
    , sbBuildspecOverride
    , sbArtifactsOverride
    , sbProjectName

    -- * Destructuring the Response
    , startBuildResponse
    , StartBuildResponse
    -- * Response Lenses
    , srsBuild
    , srsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startBuild' smart constructor.
data StartBuild = StartBuild'
  { _sbEnvironmentVariablesOverride :: !(Maybe [EnvironmentVariable])
  , _sbTimeoutInMinutesOverride     :: !(Maybe Nat)
  , _sbGitCloneDepthOverride        :: !(Maybe Nat)
  , _sbSourceVersion                :: !(Maybe Text)
  , _sbBuildspecOverride            :: !(Maybe Text)
  , _sbArtifactsOverride            :: !(Maybe ProjectArtifacts)
  , _sbProjectName                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbEnvironmentVariablesOverride' - A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
--
-- * 'sbTimeoutInMinutesOverride' - The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
--
-- * 'sbGitCloneDepthOverride' - The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
--
-- * 'sbSourceVersion' - A version of the build input to be built, for this build only. If not specified, the latest version will be used. If specified, must be one of:     * For AWS CodeCommit: the commit ID to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID will be used. If not specified, the default branch's HEAD commit ID will be used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID will be used. If not specified, the default branch's HEAD commit ID will be used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object representing the build input ZIP file to use.
--
-- * 'sbBuildspecOverride' - A build spec declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- * 'sbArtifactsOverride' - Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
--
-- * 'sbProjectName' - The name of the AWS CodeBuild build project to start running a build.
startBuild
    :: Text -- ^ 'sbProjectName'
    -> StartBuild
startBuild pProjectName_ =
  StartBuild'
    { _sbEnvironmentVariablesOverride = Nothing
    , _sbTimeoutInMinutesOverride = Nothing
    , _sbGitCloneDepthOverride = Nothing
    , _sbSourceVersion = Nothing
    , _sbBuildspecOverride = Nothing
    , _sbArtifactsOverride = Nothing
    , _sbProjectName = pProjectName_
    }


-- | A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
sbEnvironmentVariablesOverride :: Lens' StartBuild [EnvironmentVariable]
sbEnvironmentVariablesOverride = lens _sbEnvironmentVariablesOverride (\ s a -> s{_sbEnvironmentVariablesOverride = a}) . _Default . _Coerce

-- | The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
sbTimeoutInMinutesOverride :: Lens' StartBuild (Maybe Natural)
sbTimeoutInMinutesOverride = lens _sbTimeoutInMinutesOverride (\ s a -> s{_sbTimeoutInMinutesOverride = a}) . mapping _Nat

-- | The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
sbGitCloneDepthOverride :: Lens' StartBuild (Maybe Natural)
sbGitCloneDepthOverride = lens _sbGitCloneDepthOverride (\ s a -> s{_sbGitCloneDepthOverride = a}) . mapping _Nat

-- | A version of the build input to be built, for this build only. If not specified, the latest version will be used. If specified, must be one of:     * For AWS CodeCommit: the commit ID to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID will be used. If not specified, the default branch's HEAD commit ID will be used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID will be used. If not specified, the default branch's HEAD commit ID will be used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object representing the build input ZIP file to use.
sbSourceVersion :: Lens' StartBuild (Maybe Text)
sbSourceVersion = lens _sbSourceVersion (\ s a -> s{_sbSourceVersion = a})

-- | A build spec declaration that overrides, for this build only, the latest one already defined in the build project.
sbBuildspecOverride :: Lens' StartBuild (Maybe Text)
sbBuildspecOverride = lens _sbBuildspecOverride (\ s a -> s{_sbBuildspecOverride = a})

-- | Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
sbArtifactsOverride :: Lens' StartBuild (Maybe ProjectArtifacts)
sbArtifactsOverride = lens _sbArtifactsOverride (\ s a -> s{_sbArtifactsOverride = a})

-- | The name of the AWS CodeBuild build project to start running a build.
sbProjectName :: Lens' StartBuild Text
sbProjectName = lens _sbProjectName (\ s a -> s{_sbProjectName = a})

instance AWSRequest StartBuild where
        type Rs StartBuild = StartBuildResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 StartBuildResponse' <$>
                   (x .?> "build") <*> (pure (fromEnum s)))

instance Hashable StartBuild where

instance NFData StartBuild where

instance ToHeaders StartBuild where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.StartBuild" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartBuild where
        toJSON StartBuild'{..}
          = object
              (catMaybes
                 [("environmentVariablesOverride" .=) <$>
                    _sbEnvironmentVariablesOverride,
                  ("timeoutInMinutesOverride" .=) <$>
                    _sbTimeoutInMinutesOverride,
                  ("gitCloneDepthOverride" .=) <$>
                    _sbGitCloneDepthOverride,
                  ("sourceVersion" .=) <$> _sbSourceVersion,
                  ("buildspecOverride" .=) <$> _sbBuildspecOverride,
                  ("artifactsOverride" .=) <$> _sbArtifactsOverride,
                  Just ("projectName" .= _sbProjectName)])

instance ToPath StartBuild where
        toPath = const "/"

instance ToQuery StartBuild where
        toQuery = const mempty

-- | /See:/ 'startBuildResponse' smart constructor.
data StartBuildResponse = StartBuildResponse'
  { _srsBuild          :: !(Maybe Build)
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartBuildResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsBuild' - Information about the build to be run.
--
-- * 'srsResponseStatus' - -- | The response status code.
startBuildResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartBuildResponse
startBuildResponse pResponseStatus_ =
  StartBuildResponse'
    {_srsBuild = Nothing, _srsResponseStatus = pResponseStatus_}


-- | Information about the build to be run.
srsBuild :: Lens' StartBuildResponse (Maybe Build)
srsBuild = lens _srsBuild (\ s a -> s{_srsBuild = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StartBuildResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StartBuildResponse where
