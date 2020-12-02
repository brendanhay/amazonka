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
-- Module      : Network.AWS.CodeBuild.CreateProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a build project.
--
--
module Network.AWS.CodeBuild.CreateProject
    (
    -- * Creating a Request
      createProject
    , CreateProject
    -- * Request Lenses
    , cpBadgeEnabled
    , cpCache
    , cpVpcConfig
    , cpEncryptionKey
    , cpDescription
    , cpServiceRole
    , cpTags
    , cpTimeoutInMinutes
    , cpName
    , cpSource
    , cpArtifacts
    , cpEnvironment

    -- * Destructuring the Response
    , createProjectResponse
    , CreateProjectResponse
    -- * Response Lenses
    , cprsProject
    , cprsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProject' smart constructor.
data CreateProject = CreateProject'
  { _cpBadgeEnabled     :: !(Maybe Bool)
  , _cpCache            :: !(Maybe ProjectCache)
  , _cpVpcConfig        :: !(Maybe VPCConfig)
  , _cpEncryptionKey    :: !(Maybe Text)
  , _cpDescription      :: !(Maybe Text)
  , _cpServiceRole      :: !(Maybe Text)
  , _cpTags             :: !(Maybe [Tag])
  , _cpTimeoutInMinutes :: !(Maybe Nat)
  , _cpName             :: !Text
  , _cpSource           :: !ProjectSource
  , _cpArtifacts        :: !ProjectArtifacts
  , _cpEnvironment      :: !ProjectEnvironment
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpBadgeEnabled' - Set this to true to generate a publicly-accessible URL for your project's build badge.
--
-- * 'cpCache' - Stores recently used information so that it can be quickly accessed at a later time.
--
-- * 'cpVpcConfig' - VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
--
-- * 'cpEncryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. You can specify either the CMK's Amazon Resource Name (ARN) or, if available, the CMK's alias (using the format @alias//alias-name/ @ ).
--
-- * 'cpDescription' - A description that makes the build project easy to identify.
--
-- * 'cpServiceRole' - The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- * 'cpTags' - A set of tags for this build project. These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- * 'cpTimeoutInMinutes' - How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any build that has not been marked as completed. The default is 60 minutes.
--
-- * 'cpName' - The name of the build project.
--
-- * 'cpSource' - Information about the build input source code for the build project.
--
-- * 'cpArtifacts' - Information about the build output artifacts for the build project.
--
-- * 'cpEnvironment' - Information about the build environment for the build project.
createProject
    :: Text -- ^ 'cpName'
    -> ProjectSource -- ^ 'cpSource'
    -> ProjectArtifacts -- ^ 'cpArtifacts'
    -> ProjectEnvironment -- ^ 'cpEnvironment'
    -> CreateProject
createProject pName_ pSource_ pArtifacts_ pEnvironment_ =
  CreateProject'
    { _cpBadgeEnabled = Nothing
    , _cpCache = Nothing
    , _cpVpcConfig = Nothing
    , _cpEncryptionKey = Nothing
    , _cpDescription = Nothing
    , _cpServiceRole = Nothing
    , _cpTags = Nothing
    , _cpTimeoutInMinutes = Nothing
    , _cpName = pName_
    , _cpSource = pSource_
    , _cpArtifacts = pArtifacts_
    , _cpEnvironment = pEnvironment_
    }


-- | Set this to true to generate a publicly-accessible URL for your project's build badge.
cpBadgeEnabled :: Lens' CreateProject (Maybe Bool)
cpBadgeEnabled = lens _cpBadgeEnabled (\ s a -> s{_cpBadgeEnabled = a})

-- | Stores recently used information so that it can be quickly accessed at a later time.
cpCache :: Lens' CreateProject (Maybe ProjectCache)
cpCache = lens _cpCache (\ s a -> s{_cpCache = a})

-- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
cpVpcConfig :: Lens' CreateProject (Maybe VPCConfig)
cpVpcConfig = lens _cpVpcConfig (\ s a -> s{_cpVpcConfig = a})

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. You can specify either the CMK's Amazon Resource Name (ARN) or, if available, the CMK's alias (using the format @alias//alias-name/ @ ).
cpEncryptionKey :: Lens' CreateProject (Maybe Text)
cpEncryptionKey = lens _cpEncryptionKey (\ s a -> s{_cpEncryptionKey = a})

-- | A description that makes the build project easy to identify.
cpDescription :: Lens' CreateProject (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
cpServiceRole :: Lens' CreateProject (Maybe Text)
cpServiceRole = lens _cpServiceRole (\ s a -> s{_cpServiceRole = a})

-- | A set of tags for this build project. These tags are available for use by AWS services that support AWS CodeBuild build project tags.
cpTags :: Lens' CreateProject [Tag]
cpTags = lens _cpTags (\ s a -> s{_cpTags = a}) . _Default . _Coerce

-- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any build that has not been marked as completed. The default is 60 minutes.
cpTimeoutInMinutes :: Lens' CreateProject (Maybe Natural)
cpTimeoutInMinutes = lens _cpTimeoutInMinutes (\ s a -> s{_cpTimeoutInMinutes = a}) . mapping _Nat

-- | The name of the build project.
cpName :: Lens' CreateProject Text
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | Information about the build input source code for the build project.
cpSource :: Lens' CreateProject ProjectSource
cpSource = lens _cpSource (\ s a -> s{_cpSource = a})

-- | Information about the build output artifacts for the build project.
cpArtifacts :: Lens' CreateProject ProjectArtifacts
cpArtifacts = lens _cpArtifacts (\ s a -> s{_cpArtifacts = a})

-- | Information about the build environment for the build project.
cpEnvironment :: Lens' CreateProject ProjectEnvironment
cpEnvironment = lens _cpEnvironment (\ s a -> s{_cpEnvironment = a})

instance AWSRequest CreateProject where
        type Rs CreateProject = CreateProjectResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 CreateProjectResponse' <$>
                   (x .?> "project") <*> (pure (fromEnum s)))

instance Hashable CreateProject where

instance NFData CreateProject where

instance ToHeaders CreateProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.CreateProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateProject where
        toJSON CreateProject'{..}
          = object
              (catMaybes
                 [("badgeEnabled" .=) <$> _cpBadgeEnabled,
                  ("cache" .=) <$> _cpCache,
                  ("vpcConfig" .=) <$> _cpVpcConfig,
                  ("encryptionKey" .=) <$> _cpEncryptionKey,
                  ("description" .=) <$> _cpDescription,
                  ("serviceRole" .=) <$> _cpServiceRole,
                  ("tags" .=) <$> _cpTags,
                  ("timeoutInMinutes" .=) <$> _cpTimeoutInMinutes,
                  Just ("name" .= _cpName),
                  Just ("source" .= _cpSource),
                  Just ("artifacts" .= _cpArtifacts),
                  Just ("environment" .= _cpEnvironment)])

instance ToPath CreateProject where
        toPath = const "/"

instance ToQuery CreateProject where
        toQuery = const mempty

-- | /See:/ 'createProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { _cprsProject        :: !(Maybe Project)
  , _cprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsProject' - Information about the build project that was created.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createProjectResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreateProjectResponse
createProjectResponse pResponseStatus_ =
  CreateProjectResponse'
    {_cprsProject = Nothing, _cprsResponseStatus = pResponseStatus_}


-- | Information about the build project that was created.
cprsProject :: Lens' CreateProjectResponse (Maybe Project)
cprsProject = lens _cprsProject (\ s a -> s{_cprsProject = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreateProjectResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreateProjectResponse where
