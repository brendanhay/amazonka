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
-- Module      : Network.AWS.EC2.DeleteVPCEndpointConnectionNotifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more VPC endpoint connection notifications.
--
--
module Network.AWS.EC2.DeleteVPCEndpointConnectionNotifications
    (
    -- * Creating a Request
      deleteVPCEndpointConnectionNotifications
    , DeleteVPCEndpointConnectionNotifications
    -- * Request Lenses
    , dvecnDryRun
    , dvecnConnectionNotificationIds

    -- * Destructuring the Response
    , deleteVPCEndpointConnectionNotificationsResponse
    , DeleteVPCEndpointConnectionNotificationsResponse
    -- * Response Lenses
    , dvecnrsUnsuccessful
    , dvecnrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteVPCEndpointConnectionNotifications' smart constructor.
data DeleteVPCEndpointConnectionNotifications = DeleteVPCEndpointConnectionNotifications'
  { _dvecnDryRun                    :: !(Maybe Bool)
  , _dvecnConnectionNotificationIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCEndpointConnectionNotifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvecnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvecnConnectionNotificationIds' - One or more notification IDs.
deleteVPCEndpointConnectionNotifications
    :: DeleteVPCEndpointConnectionNotifications
deleteVPCEndpointConnectionNotifications =
  DeleteVPCEndpointConnectionNotifications'
    {_dvecnDryRun = Nothing, _dvecnConnectionNotificationIds = mempty}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvecnDryRun :: Lens' DeleteVPCEndpointConnectionNotifications (Maybe Bool)
dvecnDryRun = lens _dvecnDryRun (\ s a -> s{_dvecnDryRun = a})

-- | One or more notification IDs.
dvecnConnectionNotificationIds :: Lens' DeleteVPCEndpointConnectionNotifications [Text]
dvecnConnectionNotificationIds = lens _dvecnConnectionNotificationIds (\ s a -> s{_dvecnConnectionNotificationIds = a}) . _Coerce

instance AWSRequest
           DeleteVPCEndpointConnectionNotifications
         where
        type Rs DeleteVPCEndpointConnectionNotifications =
             DeleteVPCEndpointConnectionNotificationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteVPCEndpointConnectionNotificationsResponse' <$>
                   (x .@? "unsuccessful" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable
           DeleteVPCEndpointConnectionNotifications
         where

instance NFData
           DeleteVPCEndpointConnectionNotifications
         where

instance ToHeaders
           DeleteVPCEndpointConnectionNotifications
         where
        toHeaders = const mempty

instance ToPath
           DeleteVPCEndpointConnectionNotifications
         where
        toPath = const "/"

instance ToQuery
           DeleteVPCEndpointConnectionNotifications
         where
        toQuery DeleteVPCEndpointConnectionNotifications'{..}
          = mconcat
              ["Action" =:
                 ("DeleteVpcEndpointConnectionNotifications" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dvecnDryRun,
               toQueryList "ConnectionNotificationId"
                 _dvecnConnectionNotificationIds]

-- | /See:/ 'deleteVPCEndpointConnectionNotificationsResponse' smart constructor.
data DeleteVPCEndpointConnectionNotificationsResponse = DeleteVPCEndpointConnectionNotificationsResponse'
  { _dvecnrsUnsuccessful   :: !(Maybe [UnsuccessfulItem])
  , _dvecnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCEndpointConnectionNotificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvecnrsUnsuccessful' - Information about the notifications that could not be deleted successfully.
--
-- * 'dvecnrsResponseStatus' - -- | The response status code.
deleteVPCEndpointConnectionNotificationsResponse
    :: Int -- ^ 'dvecnrsResponseStatus'
    -> DeleteVPCEndpointConnectionNotificationsResponse
deleteVPCEndpointConnectionNotificationsResponse pResponseStatus_ =
  DeleteVPCEndpointConnectionNotificationsResponse'
    {_dvecnrsUnsuccessful = Nothing, _dvecnrsResponseStatus = pResponseStatus_}


-- | Information about the notifications that could not be deleted successfully.
dvecnrsUnsuccessful :: Lens' DeleteVPCEndpointConnectionNotificationsResponse [UnsuccessfulItem]
dvecnrsUnsuccessful = lens _dvecnrsUnsuccessful (\ s a -> s{_dvecnrsUnsuccessful = a}) . _Default . _Coerce

-- | -- | The response status code.
dvecnrsResponseStatus :: Lens' DeleteVPCEndpointConnectionNotificationsResponse Int
dvecnrsResponseStatus = lens _dvecnrsResponseStatus (\ s a -> s{_dvecnrsResponseStatus = a})

instance NFData
           DeleteVPCEndpointConnectionNotificationsResponse
         where
