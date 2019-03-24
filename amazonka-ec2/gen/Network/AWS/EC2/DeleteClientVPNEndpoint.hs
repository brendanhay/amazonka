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
-- Module      : Network.AWS.EC2.DeleteClientVPNEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Client VPN endpoint. You must disassociate all target networks before you can delete a Client VPN endpoint.
--
--
module Network.AWS.EC2.DeleteClientVPNEndpoint
    (
    -- * Creating a Request
      deleteClientVPNEndpoint
    , DeleteClientVPNEndpoint
    -- * Request Lenses
    , dcvpneDryRun
    , dcvpneClientVPNEndpointId

    -- * Destructuring the Response
    , deleteClientVPNEndpointResponse
    , DeleteClientVPNEndpointResponse
    -- * Response Lenses
    , dcvpnersStatus
    , dcvpnersResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteClientVPNEndpoint' smart constructor.
data DeleteClientVPNEndpoint = DeleteClientVPNEndpoint'
  { _dcvpneDryRun              :: !(Maybe Bool)
  , _dcvpneClientVPNEndpointId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClientVPNEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvpneDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dcvpneClientVPNEndpointId' - The ID of the Client VPN to be deleted.
deleteClientVPNEndpoint
    :: Text -- ^ 'dcvpneClientVPNEndpointId'
    -> DeleteClientVPNEndpoint
deleteClientVPNEndpoint pClientVPNEndpointId_ =
  DeleteClientVPNEndpoint'
    { _dcvpneDryRun = Nothing
    , _dcvpneClientVPNEndpointId = pClientVPNEndpointId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dcvpneDryRun :: Lens' DeleteClientVPNEndpoint (Maybe Bool)
dcvpneDryRun = lens _dcvpneDryRun (\ s a -> s{_dcvpneDryRun = a})

-- | The ID of the Client VPN to be deleted.
dcvpneClientVPNEndpointId :: Lens' DeleteClientVPNEndpoint Text
dcvpneClientVPNEndpointId = lens _dcvpneClientVPNEndpointId (\ s a -> s{_dcvpneClientVPNEndpointId = a})

instance AWSRequest DeleteClientVPNEndpoint where
        type Rs DeleteClientVPNEndpoint =
             DeleteClientVPNEndpointResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteClientVPNEndpointResponse' <$>
                   (x .@? "status") <*> (pure (fromEnum s)))

instance Hashable DeleteClientVPNEndpoint where

instance NFData DeleteClientVPNEndpoint where

instance ToHeaders DeleteClientVPNEndpoint where
        toHeaders = const mempty

instance ToPath DeleteClientVPNEndpoint where
        toPath = const "/"

instance ToQuery DeleteClientVPNEndpoint where
        toQuery DeleteClientVPNEndpoint'{..}
          = mconcat
              ["Action" =:
                 ("DeleteClientVpnEndpoint" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dcvpneDryRun,
               "ClientVpnEndpointId" =: _dcvpneClientVPNEndpointId]

-- | /See:/ 'deleteClientVPNEndpointResponse' smart constructor.
data DeleteClientVPNEndpointResponse = DeleteClientVPNEndpointResponse'
  { _dcvpnersStatus         :: !(Maybe ClientVPNEndpointStatus)
  , _dcvpnersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClientVPNEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvpnersStatus' - The current state of the Client VPN endpoint.
--
-- * 'dcvpnersResponseStatus' - -- | The response status code.
deleteClientVPNEndpointResponse
    :: Int -- ^ 'dcvpnersResponseStatus'
    -> DeleteClientVPNEndpointResponse
deleteClientVPNEndpointResponse pResponseStatus_ =
  DeleteClientVPNEndpointResponse'
    {_dcvpnersStatus = Nothing, _dcvpnersResponseStatus = pResponseStatus_}


-- | The current state of the Client VPN endpoint.
dcvpnersStatus :: Lens' DeleteClientVPNEndpointResponse (Maybe ClientVPNEndpointStatus)
dcvpnersStatus = lens _dcvpnersStatus (\ s a -> s{_dcvpnersStatus = a})

-- | -- | The response status code.
dcvpnersResponseStatus :: Lens' DeleteClientVPNEndpointResponse Int
dcvpnersResponseStatus = lens _dcvpnersResponseStatus (\ s a -> s{_dcvpnersResponseStatus = a})

instance NFData DeleteClientVPNEndpointResponse where
