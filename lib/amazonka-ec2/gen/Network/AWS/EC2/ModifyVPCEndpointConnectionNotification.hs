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
-- Module      : Network.AWS.EC2.ModifyVPCEndpointConnectionNotification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a connection notification for VPC endpoint or VPC endpoint service. You can change the SNS topic for the notification, or the events for which to be notified.
--
--
module Network.AWS.EC2.ModifyVPCEndpointConnectionNotification
    (
    -- * Creating a Request
      modifyVPCEndpointConnectionNotification
    , ModifyVPCEndpointConnectionNotification
    -- * Request Lenses
    , mvecnConnectionEvents
    , mvecnConnectionNotificationARN
    , mvecnDryRun
    , mvecnConnectionNotificationId

    -- * Destructuring the Response
    , modifyVPCEndpointConnectionNotificationResponse
    , ModifyVPCEndpointConnectionNotificationResponse
    -- * Response Lenses
    , mvecnrsReturnValue
    , mvecnrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyVPCEndpointConnectionNotification' smart constructor.
data ModifyVPCEndpointConnectionNotification = ModifyVPCEndpointConnectionNotification'
  { _mvecnConnectionEvents          :: !(Maybe [Text])
  , _mvecnConnectionNotificationARN :: !(Maybe Text)
  , _mvecnDryRun                    :: !(Maybe Bool)
  , _mvecnConnectionNotificationId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCEndpointConnectionNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvecnConnectionEvents' - One or more events for the endpoint. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
--
-- * 'mvecnConnectionNotificationARN' - The ARN for the SNS topic for the notification.
--
-- * 'mvecnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mvecnConnectionNotificationId' - The ID of the notification.
modifyVPCEndpointConnectionNotification
    :: Text -- ^ 'mvecnConnectionNotificationId'
    -> ModifyVPCEndpointConnectionNotification
modifyVPCEndpointConnectionNotification pConnectionNotificationId_ =
  ModifyVPCEndpointConnectionNotification'
    { _mvecnConnectionEvents = Nothing
    , _mvecnConnectionNotificationARN = Nothing
    , _mvecnDryRun = Nothing
    , _mvecnConnectionNotificationId = pConnectionNotificationId_
    }


-- | One or more events for the endpoint. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
mvecnConnectionEvents :: Lens' ModifyVPCEndpointConnectionNotification [Text]
mvecnConnectionEvents = lens _mvecnConnectionEvents (\ s a -> s{_mvecnConnectionEvents = a}) . _Default . _Coerce

-- | The ARN for the SNS topic for the notification.
mvecnConnectionNotificationARN :: Lens' ModifyVPCEndpointConnectionNotification (Maybe Text)
mvecnConnectionNotificationARN = lens _mvecnConnectionNotificationARN (\ s a -> s{_mvecnConnectionNotificationARN = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mvecnDryRun :: Lens' ModifyVPCEndpointConnectionNotification (Maybe Bool)
mvecnDryRun = lens _mvecnDryRun (\ s a -> s{_mvecnDryRun = a})

-- | The ID of the notification.
mvecnConnectionNotificationId :: Lens' ModifyVPCEndpointConnectionNotification Text
mvecnConnectionNotificationId = lens _mvecnConnectionNotificationId (\ s a -> s{_mvecnConnectionNotificationId = a})

instance AWSRequest
           ModifyVPCEndpointConnectionNotification
         where
        type Rs ModifyVPCEndpointConnectionNotification =
             ModifyVPCEndpointConnectionNotificationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyVPCEndpointConnectionNotificationResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable
           ModifyVPCEndpointConnectionNotification
         where

instance NFData
           ModifyVPCEndpointConnectionNotification
         where

instance ToHeaders
           ModifyVPCEndpointConnectionNotification
         where
        toHeaders = const mempty

instance ToPath
           ModifyVPCEndpointConnectionNotification
         where
        toPath = const "/"

instance ToQuery
           ModifyVPCEndpointConnectionNotification
         where
        toQuery ModifyVPCEndpointConnectionNotification'{..}
          = mconcat
              ["Action" =:
                 ("ModifyVpcEndpointConnectionNotification" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "ConnectionEvents" <$>
                    _mvecnConnectionEvents),
               "ConnectionNotificationArn" =:
                 _mvecnConnectionNotificationARN,
               "DryRun" =: _mvecnDryRun,
               "ConnectionNotificationId" =:
                 _mvecnConnectionNotificationId]

-- | /See:/ 'modifyVPCEndpointConnectionNotificationResponse' smart constructor.
data ModifyVPCEndpointConnectionNotificationResponse = ModifyVPCEndpointConnectionNotificationResponse'
  { _mvecnrsReturnValue    :: !(Maybe Bool)
  , _mvecnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCEndpointConnectionNotificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvecnrsReturnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'mvecnrsResponseStatus' - -- | The response status code.
modifyVPCEndpointConnectionNotificationResponse
    :: Int -- ^ 'mvecnrsResponseStatus'
    -> ModifyVPCEndpointConnectionNotificationResponse
modifyVPCEndpointConnectionNotificationResponse pResponseStatus_ =
  ModifyVPCEndpointConnectionNotificationResponse'
    {_mvecnrsReturnValue = Nothing, _mvecnrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
mvecnrsReturnValue :: Lens' ModifyVPCEndpointConnectionNotificationResponse (Maybe Bool)
mvecnrsReturnValue = lens _mvecnrsReturnValue (\ s a -> s{_mvecnrsReturnValue = a})

-- | -- | The response status code.
mvecnrsResponseStatus :: Lens' ModifyVPCEndpointConnectionNotificationResponse Int
mvecnrsResponseStatus = lens _mvecnrsResponseStatus (\ s a -> s{_mvecnrsResponseStatus = a})

instance NFData
           ModifyVPCEndpointConnectionNotificationResponse
         where
