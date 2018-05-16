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
-- Module      : Network.AWS.EC2.RejectVPCEndpointConnections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects one or more VPC endpoint connection requests to your VPC endpoint service.
--
--
module Network.AWS.EC2.RejectVPCEndpointConnections
    (
    -- * Creating a Request
      rejectVPCEndpointConnections
    , RejectVPCEndpointConnections
    -- * Request Lenses
    , rvecDryRun
    , rvecServiceId
    , rvecVPCEndpointIds

    -- * Destructuring the Response
    , rejectVPCEndpointConnectionsResponse
    , RejectVPCEndpointConnectionsResponse
    -- * Response Lenses
    , rvecrsUnsuccessful
    , rvecrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rejectVPCEndpointConnections' smart constructor.
data RejectVPCEndpointConnections = RejectVPCEndpointConnections'
  { _rvecDryRun         :: !(Maybe Bool)
  , _rvecServiceId      :: !Text
  , _rvecVPCEndpointIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectVPCEndpointConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rvecDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rvecServiceId' - The ID of the service.
--
-- * 'rvecVPCEndpointIds' - The IDs of one or more VPC endpoints.
rejectVPCEndpointConnections
    :: Text -- ^ 'rvecServiceId'
    -> RejectVPCEndpointConnections
rejectVPCEndpointConnections pServiceId_ =
  RejectVPCEndpointConnections'
    { _rvecDryRun = Nothing
    , _rvecServiceId = pServiceId_
    , _rvecVPCEndpointIds = mempty
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rvecDryRun :: Lens' RejectVPCEndpointConnections (Maybe Bool)
rvecDryRun = lens _rvecDryRun (\ s a -> s{_rvecDryRun = a})

-- | The ID of the service.
rvecServiceId :: Lens' RejectVPCEndpointConnections Text
rvecServiceId = lens _rvecServiceId (\ s a -> s{_rvecServiceId = a})

-- | The IDs of one or more VPC endpoints.
rvecVPCEndpointIds :: Lens' RejectVPCEndpointConnections [Text]
rvecVPCEndpointIds = lens _rvecVPCEndpointIds (\ s a -> s{_rvecVPCEndpointIds = a}) . _Coerce

instance AWSRequest RejectVPCEndpointConnections
         where
        type Rs RejectVPCEndpointConnections =
             RejectVPCEndpointConnectionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 RejectVPCEndpointConnectionsResponse' <$>
                   (x .@? "unsuccessful" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable RejectVPCEndpointConnections where

instance NFData RejectVPCEndpointConnections where

instance ToHeaders RejectVPCEndpointConnections where
        toHeaders = const mempty

instance ToPath RejectVPCEndpointConnections where
        toPath = const "/"

instance ToQuery RejectVPCEndpointConnections where
        toQuery RejectVPCEndpointConnections'{..}
          = mconcat
              ["Action" =:
                 ("RejectVpcEndpointConnections" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _rvecDryRun,
               "ServiceId" =: _rvecServiceId,
               toQueryList "VpcEndpointId" _rvecVPCEndpointIds]

-- | /See:/ 'rejectVPCEndpointConnectionsResponse' smart constructor.
data RejectVPCEndpointConnectionsResponse = RejectVPCEndpointConnectionsResponse'
  { _rvecrsUnsuccessful   :: !(Maybe [UnsuccessfulItem])
  , _rvecrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectVPCEndpointConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rvecrsUnsuccessful' - Information about the endpoints that were not rejected, if applicable.
--
-- * 'rvecrsResponseStatus' - -- | The response status code.
rejectVPCEndpointConnectionsResponse
    :: Int -- ^ 'rvecrsResponseStatus'
    -> RejectVPCEndpointConnectionsResponse
rejectVPCEndpointConnectionsResponse pResponseStatus_ =
  RejectVPCEndpointConnectionsResponse'
    {_rvecrsUnsuccessful = Nothing, _rvecrsResponseStatus = pResponseStatus_}


-- | Information about the endpoints that were not rejected, if applicable.
rvecrsUnsuccessful :: Lens' RejectVPCEndpointConnectionsResponse [UnsuccessfulItem]
rvecrsUnsuccessful = lens _rvecrsUnsuccessful (\ s a -> s{_rvecrsUnsuccessful = a}) . _Default . _Coerce

-- | -- | The response status code.
rvecrsResponseStatus :: Lens' RejectVPCEndpointConnectionsResponse Int
rvecrsResponseStatus = lens _rvecrsResponseStatus (\ s a -> s{_rvecrsResponseStatus = a})

instance NFData RejectVPCEndpointConnectionsResponse
         where
