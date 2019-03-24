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
-- Module      : Network.AWS.EC2.RevokeClientVPNIngress
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an ingress authorization rule from a Client VPN endpoint.
--
--
module Network.AWS.EC2.RevokeClientVPNIngress
    (
    -- * Creating a Request
      revokeClientVPNIngress
    , RevokeClientVPNIngress
    -- * Request Lenses
    , rcviAccessGroupId
    , rcviRevokeAllGroups
    , rcviDryRun
    , rcviClientVPNEndpointId
    , rcviTargetNetworkCidr

    -- * Destructuring the Response
    , revokeClientVPNIngressResponse
    , RevokeClientVPNIngressResponse
    -- * Response Lenses
    , rcvirsStatus
    , rcvirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'revokeClientVPNIngress' smart constructor.
data RevokeClientVPNIngress = RevokeClientVPNIngress'
  { _rcviAccessGroupId       :: !(Maybe Text)
  , _rcviRevokeAllGroups     :: !(Maybe Bool)
  , _rcviDryRun              :: !(Maybe Bool)
  , _rcviClientVPNEndpointId :: !Text
  , _rcviTargetNetworkCidr   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeClientVPNIngress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcviAccessGroupId' - The ID of the Active Directory group for which to revoke access.
--
-- * 'rcviRevokeAllGroups' - Indicates whether access should be revoked for all clients.
--
-- * 'rcviDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rcviClientVPNEndpointId' - The ID of the Client VPN endpoint with which the authorization rule is associated.
--
-- * 'rcviTargetNetworkCidr' - The IPv4 address range, in CIDR notation, of the network for which access is being removed.
revokeClientVPNIngress
    :: Text -- ^ 'rcviClientVPNEndpointId'
    -> Text -- ^ 'rcviTargetNetworkCidr'
    -> RevokeClientVPNIngress
revokeClientVPNIngress pClientVPNEndpointId_ pTargetNetworkCidr_ =
  RevokeClientVPNIngress'
    { _rcviAccessGroupId = Nothing
    , _rcviRevokeAllGroups = Nothing
    , _rcviDryRun = Nothing
    , _rcviClientVPNEndpointId = pClientVPNEndpointId_
    , _rcviTargetNetworkCidr = pTargetNetworkCidr_
    }


-- | The ID of the Active Directory group for which to revoke access.
rcviAccessGroupId :: Lens' RevokeClientVPNIngress (Maybe Text)
rcviAccessGroupId = lens _rcviAccessGroupId (\ s a -> s{_rcviAccessGroupId = a})

-- | Indicates whether access should be revoked for all clients.
rcviRevokeAllGroups :: Lens' RevokeClientVPNIngress (Maybe Bool)
rcviRevokeAllGroups = lens _rcviRevokeAllGroups (\ s a -> s{_rcviRevokeAllGroups = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rcviDryRun :: Lens' RevokeClientVPNIngress (Maybe Bool)
rcviDryRun = lens _rcviDryRun (\ s a -> s{_rcviDryRun = a})

-- | The ID of the Client VPN endpoint with which the authorization rule is associated.
rcviClientVPNEndpointId :: Lens' RevokeClientVPNIngress Text
rcviClientVPNEndpointId = lens _rcviClientVPNEndpointId (\ s a -> s{_rcviClientVPNEndpointId = a})

-- | The IPv4 address range, in CIDR notation, of the network for which access is being removed.
rcviTargetNetworkCidr :: Lens' RevokeClientVPNIngress Text
rcviTargetNetworkCidr = lens _rcviTargetNetworkCidr (\ s a -> s{_rcviTargetNetworkCidr = a})

instance AWSRequest RevokeClientVPNIngress where
        type Rs RevokeClientVPNIngress =
             RevokeClientVPNIngressResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 RevokeClientVPNIngressResponse' <$>
                   (x .@? "status") <*> (pure (fromEnum s)))

instance Hashable RevokeClientVPNIngress where

instance NFData RevokeClientVPNIngress where

instance ToHeaders RevokeClientVPNIngress where
        toHeaders = const mempty

instance ToPath RevokeClientVPNIngress where
        toPath = const "/"

instance ToQuery RevokeClientVPNIngress where
        toQuery RevokeClientVPNIngress'{..}
          = mconcat
              ["Action" =:
                 ("RevokeClientVpnIngress" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AccessGroupId" =: _rcviAccessGroupId,
               "RevokeAllGroups" =: _rcviRevokeAllGroups,
               "DryRun" =: _rcviDryRun,
               "ClientVpnEndpointId" =: _rcviClientVPNEndpointId,
               "TargetNetworkCidr" =: _rcviTargetNetworkCidr]

-- | /See:/ 'revokeClientVPNIngressResponse' smart constructor.
data RevokeClientVPNIngressResponse = RevokeClientVPNIngressResponse'
  { _rcvirsStatus         :: !(Maybe ClientVPNAuthorizationRuleStatus)
  , _rcvirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeClientVPNIngressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcvirsStatus' - The current state of the authorization rule.
--
-- * 'rcvirsResponseStatus' - -- | The response status code.
revokeClientVPNIngressResponse
    :: Int -- ^ 'rcvirsResponseStatus'
    -> RevokeClientVPNIngressResponse
revokeClientVPNIngressResponse pResponseStatus_ =
  RevokeClientVPNIngressResponse'
    {_rcvirsStatus = Nothing, _rcvirsResponseStatus = pResponseStatus_}


-- | The current state of the authorization rule.
rcvirsStatus :: Lens' RevokeClientVPNIngressResponse (Maybe ClientVPNAuthorizationRuleStatus)
rcvirsStatus = lens _rcvirsStatus (\ s a -> s{_rcvirsStatus = a})

-- | -- | The response status code.
rcvirsResponseStatus :: Lens' RevokeClientVPNIngressResponse Int
rcvirsResponseStatus = lens _rcvirsResponseStatus (\ s a -> s{_rcvirsResponseStatus = a})

instance NFData RevokeClientVPNIngressResponse where
