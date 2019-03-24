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
-- Module      : Network.AWS.EC2.AssociateClientVPNTargetNetwork
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a target network with a Client VPN endpoint. A target network is a subnet in a VPC. You can associate multiple subnets from the same VPC with a Client VPN endpoint. You can associate only one subnet in each Availability Zone. We recommend that you associate at least two subnets to provide Availability Zone redundancy.
--
--
module Network.AWS.EC2.AssociateClientVPNTargetNetwork
    (
    -- * Creating a Request
      associateClientVPNTargetNetwork
    , AssociateClientVPNTargetNetwork
    -- * Request Lenses
    , acvtnDryRun
    , acvtnClientVPNEndpointId
    , acvtnSubnetId

    -- * Destructuring the Response
    , associateClientVPNTargetNetworkResponse
    , AssociateClientVPNTargetNetworkResponse
    -- * Response Lenses
    , acvtnrsAssociationId
    , acvtnrsStatus
    , acvtnrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateClientVPNTargetNetwork' smart constructor.
data AssociateClientVPNTargetNetwork = AssociateClientVPNTargetNetwork'
  { _acvtnDryRun              :: !(Maybe Bool)
  , _acvtnClientVPNEndpointId :: !Text
  , _acvtnSubnetId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateClientVPNTargetNetwork' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acvtnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'acvtnClientVPNEndpointId' - The ID of the Client VPN endpoint.
--
-- * 'acvtnSubnetId' - The ID of the subnet to associate with the Client VPN endpoint.
associateClientVPNTargetNetwork
    :: Text -- ^ 'acvtnClientVPNEndpointId'
    -> Text -- ^ 'acvtnSubnetId'
    -> AssociateClientVPNTargetNetwork
associateClientVPNTargetNetwork pClientVPNEndpointId_ pSubnetId_ =
  AssociateClientVPNTargetNetwork'
    { _acvtnDryRun = Nothing
    , _acvtnClientVPNEndpointId = pClientVPNEndpointId_
    , _acvtnSubnetId = pSubnetId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
acvtnDryRun :: Lens' AssociateClientVPNTargetNetwork (Maybe Bool)
acvtnDryRun = lens _acvtnDryRun (\ s a -> s{_acvtnDryRun = a})

-- | The ID of the Client VPN endpoint.
acvtnClientVPNEndpointId :: Lens' AssociateClientVPNTargetNetwork Text
acvtnClientVPNEndpointId = lens _acvtnClientVPNEndpointId (\ s a -> s{_acvtnClientVPNEndpointId = a})

-- | The ID of the subnet to associate with the Client VPN endpoint.
acvtnSubnetId :: Lens' AssociateClientVPNTargetNetwork Text
acvtnSubnetId = lens _acvtnSubnetId (\ s a -> s{_acvtnSubnetId = a})

instance AWSRequest AssociateClientVPNTargetNetwork
         where
        type Rs AssociateClientVPNTargetNetwork =
             AssociateClientVPNTargetNetworkResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AssociateClientVPNTargetNetworkResponse' <$>
                   (x .@? "associationId") <*> (x .@? "status") <*>
                     (pure (fromEnum s)))

instance Hashable AssociateClientVPNTargetNetwork
         where

instance NFData AssociateClientVPNTargetNetwork where

instance ToHeaders AssociateClientVPNTargetNetwork
         where
        toHeaders = const mempty

instance ToPath AssociateClientVPNTargetNetwork where
        toPath = const "/"

instance ToQuery AssociateClientVPNTargetNetwork
         where
        toQuery AssociateClientVPNTargetNetwork'{..}
          = mconcat
              ["Action" =:
                 ("AssociateClientVpnTargetNetwork" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _acvtnDryRun,
               "ClientVpnEndpointId" =: _acvtnClientVPNEndpointId,
               "SubnetId" =: _acvtnSubnetId]

-- | /See:/ 'associateClientVPNTargetNetworkResponse' smart constructor.
data AssociateClientVPNTargetNetworkResponse = AssociateClientVPNTargetNetworkResponse'
  { _acvtnrsAssociationId  :: !(Maybe Text)
  , _acvtnrsStatus         :: !(Maybe AssociationStatus)
  , _acvtnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateClientVPNTargetNetworkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acvtnrsAssociationId' - The unique ID of the target network association.
--
-- * 'acvtnrsStatus' - The current state of the target network association.
--
-- * 'acvtnrsResponseStatus' - -- | The response status code.
associateClientVPNTargetNetworkResponse
    :: Int -- ^ 'acvtnrsResponseStatus'
    -> AssociateClientVPNTargetNetworkResponse
associateClientVPNTargetNetworkResponse pResponseStatus_ =
  AssociateClientVPNTargetNetworkResponse'
    { _acvtnrsAssociationId = Nothing
    , _acvtnrsStatus = Nothing
    , _acvtnrsResponseStatus = pResponseStatus_
    }


-- | The unique ID of the target network association.
acvtnrsAssociationId :: Lens' AssociateClientVPNTargetNetworkResponse (Maybe Text)
acvtnrsAssociationId = lens _acvtnrsAssociationId (\ s a -> s{_acvtnrsAssociationId = a})

-- | The current state of the target network association.
acvtnrsStatus :: Lens' AssociateClientVPNTargetNetworkResponse (Maybe AssociationStatus)
acvtnrsStatus = lens _acvtnrsStatus (\ s a -> s{_acvtnrsStatus = a})

-- | -- | The response status code.
acvtnrsResponseStatus :: Lens' AssociateClientVPNTargetNetworkResponse Int
acvtnrsResponseStatus = lens _acvtnrsResponseStatus (\ s a -> s{_acvtnrsResponseStatus = a})

instance NFData
           AssociateClientVPNTargetNetworkResponse
         where
