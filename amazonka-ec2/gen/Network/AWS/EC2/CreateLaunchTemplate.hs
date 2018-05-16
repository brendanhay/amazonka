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
-- Module      : Network.AWS.EC2.CreateLaunchTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch template. A launch template contains the parameters to launch an instance. When you launch an instance using 'RunInstances' , you can specify a launch template instead of providing the launch parameters in the request.
--
--
module Network.AWS.EC2.CreateLaunchTemplate
    (
    -- * Creating a Request
      createLaunchTemplate
    , CreateLaunchTemplate
    -- * Request Lenses
    , cltClientToken
    , cltVersionDescription
    , cltDryRun
    , cltLaunchTemplateName
    , cltLaunchTemplateData

    -- * Destructuring the Response
    , createLaunchTemplateResponse
    , CreateLaunchTemplateResponse
    -- * Response Lenses
    , cltrsLaunchTemplate
    , cltrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLaunchTemplate' smart constructor.
data CreateLaunchTemplate = CreateLaunchTemplate'
  { _cltClientToken        :: !(Maybe Text)
  , _cltVersionDescription :: !(Maybe Text)
  , _cltDryRun             :: !(Maybe Bool)
  , _cltLaunchTemplateName :: !Text
  , _cltLaunchTemplateData :: !RequestLaunchTemplateData
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLaunchTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cltClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'cltVersionDescription' - A description for the first version of the launch template.
--
-- * 'cltDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cltLaunchTemplateName' - A name for the launch template.
--
-- * 'cltLaunchTemplateData' - The information for the launch template.
createLaunchTemplate
    :: Text -- ^ 'cltLaunchTemplateName'
    -> RequestLaunchTemplateData -- ^ 'cltLaunchTemplateData'
    -> CreateLaunchTemplate
createLaunchTemplate pLaunchTemplateName_ pLaunchTemplateData_ =
  CreateLaunchTemplate'
    { _cltClientToken = Nothing
    , _cltVersionDescription = Nothing
    , _cltDryRun = Nothing
    , _cltLaunchTemplateName = pLaunchTemplateName_
    , _cltLaunchTemplateData = pLaunchTemplateData_
    }


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
cltClientToken :: Lens' CreateLaunchTemplate (Maybe Text)
cltClientToken = lens _cltClientToken (\ s a -> s{_cltClientToken = a})

-- | A description for the first version of the launch template.
cltVersionDescription :: Lens' CreateLaunchTemplate (Maybe Text)
cltVersionDescription = lens _cltVersionDescription (\ s a -> s{_cltVersionDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cltDryRun :: Lens' CreateLaunchTemplate (Maybe Bool)
cltDryRun = lens _cltDryRun (\ s a -> s{_cltDryRun = a})

-- | A name for the launch template.
cltLaunchTemplateName :: Lens' CreateLaunchTemplate Text
cltLaunchTemplateName = lens _cltLaunchTemplateName (\ s a -> s{_cltLaunchTemplateName = a})

-- | The information for the launch template.
cltLaunchTemplateData :: Lens' CreateLaunchTemplate RequestLaunchTemplateData
cltLaunchTemplateData = lens _cltLaunchTemplateData (\ s a -> s{_cltLaunchTemplateData = a})

instance AWSRequest CreateLaunchTemplate where
        type Rs CreateLaunchTemplate =
             CreateLaunchTemplateResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateLaunchTemplateResponse' <$>
                   (x .@? "launchTemplate") <*> (pure (fromEnum s)))

instance Hashable CreateLaunchTemplate where

instance NFData CreateLaunchTemplate where

instance ToHeaders CreateLaunchTemplate where
        toHeaders = const mempty

instance ToPath CreateLaunchTemplate where
        toPath = const "/"

instance ToQuery CreateLaunchTemplate where
        toQuery CreateLaunchTemplate'{..}
          = mconcat
              ["Action" =: ("CreateLaunchTemplate" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _cltClientToken,
               "VersionDescription" =: _cltVersionDescription,
               "DryRun" =: _cltDryRun,
               "LaunchTemplateName" =: _cltLaunchTemplateName,
               "LaunchTemplateData" =: _cltLaunchTemplateData]

-- | /See:/ 'createLaunchTemplateResponse' smart constructor.
data CreateLaunchTemplateResponse = CreateLaunchTemplateResponse'
  { _cltrsLaunchTemplate :: !(Maybe LaunchTemplate)
  , _cltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLaunchTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cltrsLaunchTemplate' - Information about the launch template.
--
-- * 'cltrsResponseStatus' - -- | The response status code.
createLaunchTemplateResponse
    :: Int -- ^ 'cltrsResponseStatus'
    -> CreateLaunchTemplateResponse
createLaunchTemplateResponse pResponseStatus_ =
  CreateLaunchTemplateResponse'
    {_cltrsLaunchTemplate = Nothing, _cltrsResponseStatus = pResponseStatus_}


-- | Information about the launch template.
cltrsLaunchTemplate :: Lens' CreateLaunchTemplateResponse (Maybe LaunchTemplate)
cltrsLaunchTemplate = lens _cltrsLaunchTemplate (\ s a -> s{_cltrsLaunchTemplate = a})

-- | -- | The response status code.
cltrsResponseStatus :: Lens' CreateLaunchTemplateResponse Int
cltrsResponseStatus = lens _cltrsResponseStatus (\ s a -> s{_cltrsResponseStatus = a})

instance NFData CreateLaunchTemplateResponse where
