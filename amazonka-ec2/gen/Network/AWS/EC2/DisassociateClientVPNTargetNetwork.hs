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
-- Module      : Network.AWS.EC2.DisassociateClientVPNTargetNetwork
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a target network from the specified Client VPN endpoint. When you disassociate the last target network from a Client VPN, the following happens:
--
--
--     * The route that was automatically added for the VPC is deleted
--
--     * All active client connections are terminated
--
--     * New client connections are disallowed
--
--     * The Client VPN endpoint's status changes to @pending-associate@
--
--
--
module Network.AWS.EC2.DisassociateClientVPNTargetNetwork
    (
    -- * Creating a Request
      disassociateClientVPNTargetNetwork
    , DisassociateClientVPNTargetNetwork
    -- * Request Lenses
    , dcvpntnDryRun
    , dcvpntnClientVPNEndpointId
    , dcvpntnAssociationId

    -- * Destructuring the Response
    , disassociateClientVPNTargetNetworkResponse
    , DisassociateClientVPNTargetNetworkResponse
    -- * Response Lenses
    , dcvpntnrsAssociationId
    , dcvpntnrsStatus
    , dcvpntnrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateClientVPNTargetNetwork' smart constructor.
data DisassociateClientVPNTargetNetwork = DisassociateClientVPNTargetNetwork'
  { _dcvpntnDryRun              :: !(Maybe Bool)
  , _dcvpntnClientVPNEndpointId :: !Text
  , _dcvpntnAssociationId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateClientVPNTargetNetwork' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvpntnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dcvpntnClientVPNEndpointId' - The ID of the Client VPN endpoint from which to disassociate the target network.
--
-- * 'dcvpntnAssociationId' - The ID of the target network association.
disassociateClientVPNTargetNetwork
    :: Text -- ^ 'dcvpntnClientVPNEndpointId'
    -> Text -- ^ 'dcvpntnAssociationId'
    -> DisassociateClientVPNTargetNetwork
disassociateClientVPNTargetNetwork pClientVPNEndpointId_ pAssociationId_ =
  DisassociateClientVPNTargetNetwork'
    { _dcvpntnDryRun = Nothing
    , _dcvpntnClientVPNEndpointId = pClientVPNEndpointId_
    , _dcvpntnAssociationId = pAssociationId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dcvpntnDryRun :: Lens' DisassociateClientVPNTargetNetwork (Maybe Bool)
dcvpntnDryRun = lens _dcvpntnDryRun (\ s a -> s{_dcvpntnDryRun = a})

-- | The ID of the Client VPN endpoint from which to disassociate the target network.
dcvpntnClientVPNEndpointId :: Lens' DisassociateClientVPNTargetNetwork Text
dcvpntnClientVPNEndpointId = lens _dcvpntnClientVPNEndpointId (\ s a -> s{_dcvpntnClientVPNEndpointId = a})

-- | The ID of the target network association.
dcvpntnAssociationId :: Lens' DisassociateClientVPNTargetNetwork Text
dcvpntnAssociationId = lens _dcvpntnAssociationId (\ s a -> s{_dcvpntnAssociationId = a})

instance AWSRequest
           DisassociateClientVPNTargetNetwork
         where
        type Rs DisassociateClientVPNTargetNetwork =
             DisassociateClientVPNTargetNetworkResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisassociateClientVPNTargetNetworkResponse' <$>
                   (x .@? "associationId") <*> (x .@? "status") <*>
                     (pure (fromEnum s)))

instance Hashable DisassociateClientVPNTargetNetwork
         where

instance NFData DisassociateClientVPNTargetNetwork
         where

instance ToHeaders DisassociateClientVPNTargetNetwork
         where
        toHeaders = const mempty

instance ToPath DisassociateClientVPNTargetNetwork
         where
        toPath = const "/"

instance ToQuery DisassociateClientVPNTargetNetwork
         where
        toQuery DisassociateClientVPNTargetNetwork'{..}
          = mconcat
              ["Action" =:
                 ("DisassociateClientVpnTargetNetwork" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dcvpntnDryRun,
               "ClientVpnEndpointId" =: _dcvpntnClientVPNEndpointId,
               "AssociationId" =: _dcvpntnAssociationId]

-- | /See:/ 'disassociateClientVPNTargetNetworkResponse' smart constructor.
data DisassociateClientVPNTargetNetworkResponse = DisassociateClientVPNTargetNetworkResponse'
  { _dcvpntnrsAssociationId  :: !(Maybe Text)
  , _dcvpntnrsStatus         :: !(Maybe AssociationStatus)
  , _dcvpntnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateClientVPNTargetNetworkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvpntnrsAssociationId' - The ID of the target network association.
--
-- * 'dcvpntnrsStatus' - The current state of the target network association.
--
-- * 'dcvpntnrsResponseStatus' - -- | The response status code.
disassociateClientVPNTargetNetworkResponse
    :: Int -- ^ 'dcvpntnrsResponseStatus'
    -> DisassociateClientVPNTargetNetworkResponse
disassociateClientVPNTargetNetworkResponse pResponseStatus_ =
  DisassociateClientVPNTargetNetworkResponse'
    { _dcvpntnrsAssociationId = Nothing
    , _dcvpntnrsStatus = Nothing
    , _dcvpntnrsResponseStatus = pResponseStatus_
    }


-- | The ID of the target network association.
dcvpntnrsAssociationId :: Lens' DisassociateClientVPNTargetNetworkResponse (Maybe Text)
dcvpntnrsAssociationId = lens _dcvpntnrsAssociationId (\ s a -> s{_dcvpntnrsAssociationId = a})

-- | The current state of the target network association.
dcvpntnrsStatus :: Lens' DisassociateClientVPNTargetNetworkResponse (Maybe AssociationStatus)
dcvpntnrsStatus = lens _dcvpntnrsStatus (\ s a -> s{_dcvpntnrsStatus = a})

-- | -- | The response status code.
dcvpntnrsResponseStatus :: Lens' DisassociateClientVPNTargetNetworkResponse Int
dcvpntnrsResponseStatus = lens _dcvpntnrsResponseStatus (\ s a -> s{_dcvpntnrsResponseStatus = a})

instance NFData
           DisassociateClientVPNTargetNetworkResponse
         where
