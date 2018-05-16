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
-- Module      : Network.AWS.EC2.AcceptVPCEndpointConnections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts one or more interface VPC endpoint connection requests to your VPC endpoint service.
--
--
module Network.AWS.EC2.AcceptVPCEndpointConnections
    (
    -- * Creating a Request
      acceptVPCEndpointConnections
    , AcceptVPCEndpointConnections
    -- * Request Lenses
    , avecDryRun
    , avecServiceId
    , avecVPCEndpointIds

    -- * Destructuring the Response
    , acceptVPCEndpointConnectionsResponse
    , AcceptVPCEndpointConnectionsResponse
    -- * Response Lenses
    , avecrsUnsuccessful
    , avecrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'acceptVPCEndpointConnections' smart constructor.
data AcceptVPCEndpointConnections = AcceptVPCEndpointConnections'
  { _avecDryRun         :: !(Maybe Bool)
  , _avecServiceId      :: !Text
  , _avecVPCEndpointIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptVPCEndpointConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avecDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'avecServiceId' - The ID of the endpoint service.
--
-- * 'avecVPCEndpointIds' - The IDs of one or more interface VPC endpoints.
acceptVPCEndpointConnections
    :: Text -- ^ 'avecServiceId'
    -> AcceptVPCEndpointConnections
acceptVPCEndpointConnections pServiceId_ =
  AcceptVPCEndpointConnections'
    { _avecDryRun = Nothing
    , _avecServiceId = pServiceId_
    , _avecVPCEndpointIds = mempty
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
avecDryRun :: Lens' AcceptVPCEndpointConnections (Maybe Bool)
avecDryRun = lens _avecDryRun (\ s a -> s{_avecDryRun = a})

-- | The ID of the endpoint service.
avecServiceId :: Lens' AcceptVPCEndpointConnections Text
avecServiceId = lens _avecServiceId (\ s a -> s{_avecServiceId = a})

-- | The IDs of one or more interface VPC endpoints.
avecVPCEndpointIds :: Lens' AcceptVPCEndpointConnections [Text]
avecVPCEndpointIds = lens _avecVPCEndpointIds (\ s a -> s{_avecVPCEndpointIds = a}) . _Coerce

instance AWSRequest AcceptVPCEndpointConnections
         where
        type Rs AcceptVPCEndpointConnections =
             AcceptVPCEndpointConnectionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AcceptVPCEndpointConnectionsResponse' <$>
                   (x .@? "unsuccessful" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable AcceptVPCEndpointConnections where

instance NFData AcceptVPCEndpointConnections where

instance ToHeaders AcceptVPCEndpointConnections where
        toHeaders = const mempty

instance ToPath AcceptVPCEndpointConnections where
        toPath = const "/"

instance ToQuery AcceptVPCEndpointConnections where
        toQuery AcceptVPCEndpointConnections'{..}
          = mconcat
              ["Action" =:
                 ("AcceptVpcEndpointConnections" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _avecDryRun,
               "ServiceId" =: _avecServiceId,
               toQueryList "VpcEndpointId" _avecVPCEndpointIds]

-- | /See:/ 'acceptVPCEndpointConnectionsResponse' smart constructor.
data AcceptVPCEndpointConnectionsResponse = AcceptVPCEndpointConnectionsResponse'
  { _avecrsUnsuccessful   :: !(Maybe [UnsuccessfulItem])
  , _avecrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptVPCEndpointConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avecrsUnsuccessful' - Information about the interface endpoints that were not accepted, if applicable.
--
-- * 'avecrsResponseStatus' - -- | The response status code.
acceptVPCEndpointConnectionsResponse
    :: Int -- ^ 'avecrsResponseStatus'
    -> AcceptVPCEndpointConnectionsResponse
acceptVPCEndpointConnectionsResponse pResponseStatus_ =
  AcceptVPCEndpointConnectionsResponse'
    {_avecrsUnsuccessful = Nothing, _avecrsResponseStatus = pResponseStatus_}


-- | Information about the interface endpoints that were not accepted, if applicable.
avecrsUnsuccessful :: Lens' AcceptVPCEndpointConnectionsResponse [UnsuccessfulItem]
avecrsUnsuccessful = lens _avecrsUnsuccessful (\ s a -> s{_avecrsUnsuccessful = a}) . _Default . _Coerce

-- | -- | The response status code.
avecrsResponseStatus :: Lens' AcceptVPCEndpointConnectionsResponse Int
avecrsResponseStatus = lens _avecrsResponseStatus (\ s a -> s{_avecrsResponseStatus = a})

instance NFData AcceptVPCEndpointConnectionsResponse
         where
