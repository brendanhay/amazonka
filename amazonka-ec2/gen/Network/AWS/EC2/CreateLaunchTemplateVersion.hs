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
-- Module      : Network.AWS.EC2.CreateLaunchTemplateVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version for a launch template. You can specify an existing version of launch template from which to base the new version.
--
--
-- Launch template versions are numbered in the order in which they are created. You cannot specify, change, or replace the numbering of launch template versions.
--
module Network.AWS.EC2.CreateLaunchTemplateVersion
    (
    -- * Creating a Request
      createLaunchTemplateVersion
    , CreateLaunchTemplateVersion
    -- * Request Lenses
    , cltvLaunchTemplateName
    , cltvClientToken
    , cltvLaunchTemplateId
    , cltvVersionDescription
    , cltvSourceVersion
    , cltvDryRun
    , cltvLaunchTemplateData

    -- * Destructuring the Response
    , createLaunchTemplateVersionResponse
    , CreateLaunchTemplateVersionResponse
    -- * Response Lenses
    , cltvrsLaunchTemplateVersion
    , cltvrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLaunchTemplateVersion' smart constructor.
data CreateLaunchTemplateVersion = CreateLaunchTemplateVersion'
  { _cltvLaunchTemplateName :: !(Maybe Text)
  , _cltvClientToken        :: !(Maybe Text)
  , _cltvLaunchTemplateId   :: !(Maybe Text)
  , _cltvVersionDescription :: !(Maybe Text)
  , _cltvSourceVersion      :: !(Maybe Text)
  , _cltvDryRun             :: !(Maybe Bool)
  , _cltvLaunchTemplateData :: !RequestLaunchTemplateData
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLaunchTemplateVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cltvLaunchTemplateName' - The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- * 'cltvClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'cltvLaunchTemplateId' - The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- * 'cltvVersionDescription' - A description for the version of the launch template.
--
-- * 'cltvSourceVersion' - The version number of the launch template version on which to base the new version. The new version inherits the same launch parameters as the source version, except for parameters that you specify in LaunchTemplateData.
--
-- * 'cltvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cltvLaunchTemplateData' - The information for the launch template.
createLaunchTemplateVersion
    :: RequestLaunchTemplateData -- ^ 'cltvLaunchTemplateData'
    -> CreateLaunchTemplateVersion
createLaunchTemplateVersion pLaunchTemplateData_ =
  CreateLaunchTemplateVersion'
    { _cltvLaunchTemplateName = Nothing
    , _cltvClientToken = Nothing
    , _cltvLaunchTemplateId = Nothing
    , _cltvVersionDescription = Nothing
    , _cltvSourceVersion = Nothing
    , _cltvDryRun = Nothing
    , _cltvLaunchTemplateData = pLaunchTemplateData_
    }


-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
cltvLaunchTemplateName :: Lens' CreateLaunchTemplateVersion (Maybe Text)
cltvLaunchTemplateName = lens _cltvLaunchTemplateName (\ s a -> s{_cltvLaunchTemplateName = a})

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
cltvClientToken :: Lens' CreateLaunchTemplateVersion (Maybe Text)
cltvClientToken = lens _cltvClientToken (\ s a -> s{_cltvClientToken = a})

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
cltvLaunchTemplateId :: Lens' CreateLaunchTemplateVersion (Maybe Text)
cltvLaunchTemplateId = lens _cltvLaunchTemplateId (\ s a -> s{_cltvLaunchTemplateId = a})

-- | A description for the version of the launch template.
cltvVersionDescription :: Lens' CreateLaunchTemplateVersion (Maybe Text)
cltvVersionDescription = lens _cltvVersionDescription (\ s a -> s{_cltvVersionDescription = a})

-- | The version number of the launch template version on which to base the new version. The new version inherits the same launch parameters as the source version, except for parameters that you specify in LaunchTemplateData.
cltvSourceVersion :: Lens' CreateLaunchTemplateVersion (Maybe Text)
cltvSourceVersion = lens _cltvSourceVersion (\ s a -> s{_cltvSourceVersion = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cltvDryRun :: Lens' CreateLaunchTemplateVersion (Maybe Bool)
cltvDryRun = lens _cltvDryRun (\ s a -> s{_cltvDryRun = a})

-- | The information for the launch template.
cltvLaunchTemplateData :: Lens' CreateLaunchTemplateVersion RequestLaunchTemplateData
cltvLaunchTemplateData = lens _cltvLaunchTemplateData (\ s a -> s{_cltvLaunchTemplateData = a})

instance AWSRequest CreateLaunchTemplateVersion where
        type Rs CreateLaunchTemplateVersion =
             CreateLaunchTemplateVersionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateLaunchTemplateVersionResponse' <$>
                   (x .@? "launchTemplateVersion") <*>
                     (pure (fromEnum s)))

instance Hashable CreateLaunchTemplateVersion where

instance NFData CreateLaunchTemplateVersion where

instance ToHeaders CreateLaunchTemplateVersion where
        toHeaders = const mempty

instance ToPath CreateLaunchTemplateVersion where
        toPath = const "/"

instance ToQuery CreateLaunchTemplateVersion where
        toQuery CreateLaunchTemplateVersion'{..}
          = mconcat
              ["Action" =:
                 ("CreateLaunchTemplateVersion" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "LaunchTemplateName" =: _cltvLaunchTemplateName,
               "ClientToken" =: _cltvClientToken,
               "LaunchTemplateId" =: _cltvLaunchTemplateId,
               "VersionDescription" =: _cltvVersionDescription,
               "SourceVersion" =: _cltvSourceVersion,
               "DryRun" =: _cltvDryRun,
               "LaunchTemplateData" =: _cltvLaunchTemplateData]

-- | /See:/ 'createLaunchTemplateVersionResponse' smart constructor.
data CreateLaunchTemplateVersionResponse = CreateLaunchTemplateVersionResponse'
  { _cltvrsLaunchTemplateVersion :: !(Maybe LaunchTemplateVersion)
  , _cltvrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLaunchTemplateVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cltvrsLaunchTemplateVersion' - Information about the launch template version.
--
-- * 'cltvrsResponseStatus' - -- | The response status code.
createLaunchTemplateVersionResponse
    :: Int -- ^ 'cltvrsResponseStatus'
    -> CreateLaunchTemplateVersionResponse
createLaunchTemplateVersionResponse pResponseStatus_ =
  CreateLaunchTemplateVersionResponse'
    { _cltvrsLaunchTemplateVersion = Nothing
    , _cltvrsResponseStatus = pResponseStatus_
    }


-- | Information about the launch template version.
cltvrsLaunchTemplateVersion :: Lens' CreateLaunchTemplateVersionResponse (Maybe LaunchTemplateVersion)
cltvrsLaunchTemplateVersion = lens _cltvrsLaunchTemplateVersion (\ s a -> s{_cltvrsLaunchTemplateVersion = a})

-- | -- | The response status code.
cltvrsResponseStatus :: Lens' CreateLaunchTemplateVersionResponse Int
cltvrsResponseStatus = lens _cltvrsResponseStatus (\ s a -> s{_cltvrsResponseStatus = a})

instance NFData CreateLaunchTemplateVersionResponse
         where
