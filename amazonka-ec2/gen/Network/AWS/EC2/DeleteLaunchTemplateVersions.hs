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
-- Module      : Network.AWS.EC2.DeleteLaunchTemplateVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more versions of a launch template. You cannot delete the default version of a launch template; you must first assign a different version as the default. If the default version is the only version for the launch template, you must delete the entire launch template using 'DeleteLaunchTemplate' .
--
--
module Network.AWS.EC2.DeleteLaunchTemplateVersions
    (
    -- * Creating a Request
      deleteLaunchTemplateVersions
    , DeleteLaunchTemplateVersions
    -- * Request Lenses
    , dltvLaunchTemplateName
    , dltvLaunchTemplateId
    , dltvDryRun
    , dltvVersions

    -- * Destructuring the Response
    , deleteLaunchTemplateVersionsResponse
    , DeleteLaunchTemplateVersionsResponse
    -- * Response Lenses
    , dltvsrsSuccessfullyDeletedLaunchTemplateVersions
    , dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions
    , dltvsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLaunchTemplateVersions' smart constructor.
data DeleteLaunchTemplateVersions = DeleteLaunchTemplateVersions'
  { _dltvLaunchTemplateName :: !(Maybe Text)
  , _dltvLaunchTemplateId   :: !(Maybe Text)
  , _dltvDryRun             :: !(Maybe Bool)
  , _dltvVersions           :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLaunchTemplateVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltvLaunchTemplateName' - The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- * 'dltvLaunchTemplateId' - The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- * 'dltvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dltvVersions' - The version numbers of one or more launch template versions to delete.
deleteLaunchTemplateVersions
    :: DeleteLaunchTemplateVersions
deleteLaunchTemplateVersions =
  DeleteLaunchTemplateVersions'
    { _dltvLaunchTemplateName = Nothing
    , _dltvLaunchTemplateId = Nothing
    , _dltvDryRun = Nothing
    , _dltvVersions = mempty
    }


-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
dltvLaunchTemplateName :: Lens' DeleteLaunchTemplateVersions (Maybe Text)
dltvLaunchTemplateName = lens _dltvLaunchTemplateName (\ s a -> s{_dltvLaunchTemplateName = a})

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
dltvLaunchTemplateId :: Lens' DeleteLaunchTemplateVersions (Maybe Text)
dltvLaunchTemplateId = lens _dltvLaunchTemplateId (\ s a -> s{_dltvLaunchTemplateId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dltvDryRun :: Lens' DeleteLaunchTemplateVersions (Maybe Bool)
dltvDryRun = lens _dltvDryRun (\ s a -> s{_dltvDryRun = a})

-- | The version numbers of one or more launch template versions to delete.
dltvVersions :: Lens' DeleteLaunchTemplateVersions [Text]
dltvVersions = lens _dltvVersions (\ s a -> s{_dltvVersions = a}) . _Coerce

instance AWSRequest DeleteLaunchTemplateVersions
         where
        type Rs DeleteLaunchTemplateVersions =
             DeleteLaunchTemplateVersionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteLaunchTemplateVersionsResponse' <$>
                   (x .@? "successfullyDeletedLaunchTemplateVersionSet"
                      .!@ mempty
                      >>= may (parseXMLList "item"))
                     <*>
                     (x .@?
                        "unsuccessfullyDeletedLaunchTemplateVersionSet"
                        .!@ mempty
                        >>= may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DeleteLaunchTemplateVersions where

instance NFData DeleteLaunchTemplateVersions where

instance ToHeaders DeleteLaunchTemplateVersions where
        toHeaders = const mempty

instance ToPath DeleteLaunchTemplateVersions where
        toPath = const "/"

instance ToQuery DeleteLaunchTemplateVersions where
        toQuery DeleteLaunchTemplateVersions'{..}
          = mconcat
              ["Action" =:
                 ("DeleteLaunchTemplateVersions" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "LaunchTemplateName" =: _dltvLaunchTemplateName,
               "LaunchTemplateId" =: _dltvLaunchTemplateId,
               "DryRun" =: _dltvDryRun,
               toQueryList "LaunchTemplateVersion" _dltvVersions]

-- | /See:/ 'deleteLaunchTemplateVersionsResponse' smart constructor.
data DeleteLaunchTemplateVersionsResponse = DeleteLaunchTemplateVersionsResponse'
  { _dltvsrsSuccessfullyDeletedLaunchTemplateVersions :: !(Maybe [DeleteLaunchTemplateVersionsResponseSuccessItem])
  , _dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions :: !(Maybe [DeleteLaunchTemplateVersionsResponseErrorItem])
  , _dltvsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLaunchTemplateVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltvsrsSuccessfullyDeletedLaunchTemplateVersions' - Information about the launch template versions that were successfully deleted.
--
-- * 'dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions' - Information about the launch template versions that could not be deleted.
--
-- * 'dltvsrsResponseStatus' - -- | The response status code.
deleteLaunchTemplateVersionsResponse
    :: Int -- ^ 'dltvsrsResponseStatus'
    -> DeleteLaunchTemplateVersionsResponse
deleteLaunchTemplateVersionsResponse pResponseStatus_ =
  DeleteLaunchTemplateVersionsResponse'
    { _dltvsrsSuccessfullyDeletedLaunchTemplateVersions = Nothing
    , _dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions = Nothing
    , _dltvsrsResponseStatus = pResponseStatus_
    }


-- | Information about the launch template versions that were successfully deleted.
dltvsrsSuccessfullyDeletedLaunchTemplateVersions :: Lens' DeleteLaunchTemplateVersionsResponse [DeleteLaunchTemplateVersionsResponseSuccessItem]
dltvsrsSuccessfullyDeletedLaunchTemplateVersions = lens _dltvsrsSuccessfullyDeletedLaunchTemplateVersions (\ s a -> s{_dltvsrsSuccessfullyDeletedLaunchTemplateVersions = a}) . _Default . _Coerce

-- | Information about the launch template versions that could not be deleted.
dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions :: Lens' DeleteLaunchTemplateVersionsResponse [DeleteLaunchTemplateVersionsResponseErrorItem]
dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions = lens _dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions (\ s a -> s{_dltvsrsUnsuccessfullyDeletedLaunchTemplateVersions = a}) . _Default . _Coerce

-- | -- | The response status code.
dltvsrsResponseStatus :: Lens' DeleteLaunchTemplateVersionsResponse Int
dltvsrsResponseStatus = lens _dltvsrsResponseStatus (\ s a -> s{_dltvsrsResponseStatus = a})

instance NFData DeleteLaunchTemplateVersionsResponse
         where
