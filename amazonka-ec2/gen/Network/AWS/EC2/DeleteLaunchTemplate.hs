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
-- Module      : Network.AWS.EC2.DeleteLaunchTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a launch template. Deleting a launch template deletes all of its versions.
--
--
module Network.AWS.EC2.DeleteLaunchTemplate
    (
    -- * Creating a Request
      deleteLaunchTemplate
    , DeleteLaunchTemplate
    -- * Request Lenses
    , dltLaunchTemplateName
    , dltLaunchTemplateId
    , dltDryRun

    -- * Destructuring the Response
    , deleteLaunchTemplateResponse
    , DeleteLaunchTemplateResponse
    -- * Response Lenses
    , dltrsLaunchTemplate
    , dltrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLaunchTemplate' smart constructor.
data DeleteLaunchTemplate = DeleteLaunchTemplate'
  { _dltLaunchTemplateName :: !(Maybe Text)
  , _dltLaunchTemplateId   :: !(Maybe Text)
  , _dltDryRun             :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLaunchTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltLaunchTemplateName' - The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- * 'dltLaunchTemplateId' - The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- * 'dltDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
deleteLaunchTemplate
    :: DeleteLaunchTemplate
deleteLaunchTemplate =
  DeleteLaunchTemplate'
    { _dltLaunchTemplateName = Nothing
    , _dltLaunchTemplateId = Nothing
    , _dltDryRun = Nothing
    }


-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
dltLaunchTemplateName :: Lens' DeleteLaunchTemplate (Maybe Text)
dltLaunchTemplateName = lens _dltLaunchTemplateName (\ s a -> s{_dltLaunchTemplateName = a})

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
dltLaunchTemplateId :: Lens' DeleteLaunchTemplate (Maybe Text)
dltLaunchTemplateId = lens _dltLaunchTemplateId (\ s a -> s{_dltLaunchTemplateId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dltDryRun :: Lens' DeleteLaunchTemplate (Maybe Bool)
dltDryRun = lens _dltDryRun (\ s a -> s{_dltDryRun = a})

instance AWSRequest DeleteLaunchTemplate where
        type Rs DeleteLaunchTemplate =
             DeleteLaunchTemplateResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteLaunchTemplateResponse' <$>
                   (x .@? "launchTemplate") <*> (pure (fromEnum s)))

instance Hashable DeleteLaunchTemplate where

instance NFData DeleteLaunchTemplate where

instance ToHeaders DeleteLaunchTemplate where
        toHeaders = const mempty

instance ToPath DeleteLaunchTemplate where
        toPath = const "/"

instance ToQuery DeleteLaunchTemplate where
        toQuery DeleteLaunchTemplate'{..}
          = mconcat
              ["Action" =: ("DeleteLaunchTemplate" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "LaunchTemplateName" =: _dltLaunchTemplateName,
               "LaunchTemplateId" =: _dltLaunchTemplateId,
               "DryRun" =: _dltDryRun]

-- | /See:/ 'deleteLaunchTemplateResponse' smart constructor.
data DeleteLaunchTemplateResponse = DeleteLaunchTemplateResponse'
  { _dltrsLaunchTemplate :: !(Maybe LaunchTemplate)
  , _dltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLaunchTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltrsLaunchTemplate' - Information about the launch template.
--
-- * 'dltrsResponseStatus' - -- | The response status code.
deleteLaunchTemplateResponse
    :: Int -- ^ 'dltrsResponseStatus'
    -> DeleteLaunchTemplateResponse
deleteLaunchTemplateResponse pResponseStatus_ =
  DeleteLaunchTemplateResponse'
    {_dltrsLaunchTemplate = Nothing, _dltrsResponseStatus = pResponseStatus_}


-- | Information about the launch template.
dltrsLaunchTemplate :: Lens' DeleteLaunchTemplateResponse (Maybe LaunchTemplate)
dltrsLaunchTemplate = lens _dltrsLaunchTemplate (\ s a -> s{_dltrsLaunchTemplate = a})

-- | -- | The response status code.
dltrsResponseStatus :: Lens' DeleteLaunchTemplateResponse Int
dltrsResponseStatus = lens _dltrsResponseStatus (\ s a -> s{_dltrsResponseStatus = a})

instance NFData DeleteLaunchTemplateResponse where
