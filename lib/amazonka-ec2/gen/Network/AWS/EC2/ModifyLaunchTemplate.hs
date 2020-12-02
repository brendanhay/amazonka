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
-- Module      : Network.AWS.EC2.ModifyLaunchTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a launch template. You can specify which version of the launch template to set as the default version. When launching an instance, the default version applies when a launch template version is not specified.
--
--
module Network.AWS.EC2.ModifyLaunchTemplate
    (
    -- * Creating a Request
      modifyLaunchTemplate
    , ModifyLaunchTemplate
    -- * Request Lenses
    , mltLaunchTemplateName
    , mltClientToken
    , mltLaunchTemplateId
    , mltDefaultVersion
    , mltDryRun

    -- * Destructuring the Response
    , modifyLaunchTemplateResponse
    , ModifyLaunchTemplateResponse
    -- * Response Lenses
    , mltrsLaunchTemplate
    , mltrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyLaunchTemplate' smart constructor.
data ModifyLaunchTemplate = ModifyLaunchTemplate'
  { _mltLaunchTemplateName :: !(Maybe Text)
  , _mltClientToken        :: !(Maybe Text)
  , _mltLaunchTemplateId   :: !(Maybe Text)
  , _mltDefaultVersion     :: !(Maybe Text)
  , _mltDryRun             :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyLaunchTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mltLaunchTemplateName' - The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- * 'mltClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'mltLaunchTemplateId' - The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- * 'mltDefaultVersion' - The version number of the launch template to set as the default version.
--
-- * 'mltDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
modifyLaunchTemplate
    :: ModifyLaunchTemplate
modifyLaunchTemplate =
  ModifyLaunchTemplate'
    { _mltLaunchTemplateName = Nothing
    , _mltClientToken = Nothing
    , _mltLaunchTemplateId = Nothing
    , _mltDefaultVersion = Nothing
    , _mltDryRun = Nothing
    }


-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
mltLaunchTemplateName :: Lens' ModifyLaunchTemplate (Maybe Text)
mltLaunchTemplateName = lens _mltLaunchTemplateName (\ s a -> s{_mltLaunchTemplateName = a})

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
mltClientToken :: Lens' ModifyLaunchTemplate (Maybe Text)
mltClientToken = lens _mltClientToken (\ s a -> s{_mltClientToken = a})

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
mltLaunchTemplateId :: Lens' ModifyLaunchTemplate (Maybe Text)
mltLaunchTemplateId = lens _mltLaunchTemplateId (\ s a -> s{_mltLaunchTemplateId = a})

-- | The version number of the launch template to set as the default version.
mltDefaultVersion :: Lens' ModifyLaunchTemplate (Maybe Text)
mltDefaultVersion = lens _mltDefaultVersion (\ s a -> s{_mltDefaultVersion = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mltDryRun :: Lens' ModifyLaunchTemplate (Maybe Bool)
mltDryRun = lens _mltDryRun (\ s a -> s{_mltDryRun = a})

instance AWSRequest ModifyLaunchTemplate where
        type Rs ModifyLaunchTemplate =
             ModifyLaunchTemplateResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyLaunchTemplateResponse' <$>
                   (x .@? "launchTemplate") <*> (pure (fromEnum s)))

instance Hashable ModifyLaunchTemplate where

instance NFData ModifyLaunchTemplate where

instance ToHeaders ModifyLaunchTemplate where
        toHeaders = const mempty

instance ToPath ModifyLaunchTemplate where
        toPath = const "/"

instance ToQuery ModifyLaunchTemplate where
        toQuery ModifyLaunchTemplate'{..}
          = mconcat
              ["Action" =: ("ModifyLaunchTemplate" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "LaunchTemplateName" =: _mltLaunchTemplateName,
               "ClientToken" =: _mltClientToken,
               "LaunchTemplateId" =: _mltLaunchTemplateId,
               "SetDefaultVersion" =: _mltDefaultVersion,
               "DryRun" =: _mltDryRun]

-- | /See:/ 'modifyLaunchTemplateResponse' smart constructor.
data ModifyLaunchTemplateResponse = ModifyLaunchTemplateResponse'
  { _mltrsLaunchTemplate :: !(Maybe LaunchTemplate)
  , _mltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyLaunchTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mltrsLaunchTemplate' - Information about the launch template.
--
-- * 'mltrsResponseStatus' - -- | The response status code.
modifyLaunchTemplateResponse
    :: Int -- ^ 'mltrsResponseStatus'
    -> ModifyLaunchTemplateResponse
modifyLaunchTemplateResponse pResponseStatus_ =
  ModifyLaunchTemplateResponse'
    {_mltrsLaunchTemplate = Nothing, _mltrsResponseStatus = pResponseStatus_}


-- | Information about the launch template.
mltrsLaunchTemplate :: Lens' ModifyLaunchTemplateResponse (Maybe LaunchTemplate)
mltrsLaunchTemplate = lens _mltrsLaunchTemplate (\ s a -> s{_mltrsLaunchTemplate = a})

-- | -- | The response status code.
mltrsResponseStatus :: Lens' ModifyLaunchTemplateResponse Int
mltrsResponseStatus = lens _mltrsResponseStatus (\ s a -> s{_mltrsResponseStatus = a})

instance NFData ModifyLaunchTemplateResponse where
