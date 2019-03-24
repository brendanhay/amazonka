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
-- Module      : Network.AWS.EC2.AuthorizeClientVPNIngress
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an ingress authorization rule to a Client VPN endpoint. Ingress authorization rules act as firewall rules that grant access to networks. You must configure ingress authorization rules to enable clients to access resources in AWS or on-premises networks.
--
--
module Network.AWS.EC2.AuthorizeClientVPNIngress
    (
    -- * Creating a Request
      authorizeClientVPNIngress
    , AuthorizeClientVPNIngress
    -- * Request Lenses
    , acviAccessGroupId
    , acviAuthorizeAllGroups
    , acviDescription
    , acviDryRun
    , acviClientVPNEndpointId
    , acviTargetNetworkCidr

    -- * Destructuring the Response
    , authorizeClientVPNIngressResponse
    , AuthorizeClientVPNIngressResponse
    -- * Response Lenses
    , acvirsStatus
    , acvirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'authorizeClientVPNIngress' smart constructor.
data AuthorizeClientVPNIngress = AuthorizeClientVPNIngress'
  { _acviAccessGroupId       :: !(Maybe Text)
  , _acviAuthorizeAllGroups  :: !(Maybe Bool)
  , _acviDescription         :: !(Maybe Text)
  , _acviDryRun              :: !(Maybe Bool)
  , _acviClientVPNEndpointId :: !Text
  , _acviTargetNetworkCidr   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeClientVPNIngress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acviAccessGroupId' - The ID of the Active Directory group to grant access.
--
-- * 'acviAuthorizeAllGroups' - Indicates whether to grant access to all clients. Use @true@ to grant all clients who successfully establish a VPN connection access to the network.
--
-- * 'acviDescription' - A brief description of the authorization rule.
--
-- * 'acviDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'acviClientVPNEndpointId' - The ID of the Client VPN endpoint.
--
-- * 'acviTargetNetworkCidr' - The IPv4 address range, in CIDR notation, of the network for which access is being authorized.
authorizeClientVPNIngress
    :: Text -- ^ 'acviClientVPNEndpointId'
    -> Text -- ^ 'acviTargetNetworkCidr'
    -> AuthorizeClientVPNIngress
authorizeClientVPNIngress pClientVPNEndpointId_ pTargetNetworkCidr_ =
  AuthorizeClientVPNIngress'
    { _acviAccessGroupId = Nothing
    , _acviAuthorizeAllGroups = Nothing
    , _acviDescription = Nothing
    , _acviDryRun = Nothing
    , _acviClientVPNEndpointId = pClientVPNEndpointId_
    , _acviTargetNetworkCidr = pTargetNetworkCidr_
    }


-- | The ID of the Active Directory group to grant access.
acviAccessGroupId :: Lens' AuthorizeClientVPNIngress (Maybe Text)
acviAccessGroupId = lens _acviAccessGroupId (\ s a -> s{_acviAccessGroupId = a})

-- | Indicates whether to grant access to all clients. Use @true@ to grant all clients who successfully establish a VPN connection access to the network.
acviAuthorizeAllGroups :: Lens' AuthorizeClientVPNIngress (Maybe Bool)
acviAuthorizeAllGroups = lens _acviAuthorizeAllGroups (\ s a -> s{_acviAuthorizeAllGroups = a})

-- | A brief description of the authorization rule.
acviDescription :: Lens' AuthorizeClientVPNIngress (Maybe Text)
acviDescription = lens _acviDescription (\ s a -> s{_acviDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
acviDryRun :: Lens' AuthorizeClientVPNIngress (Maybe Bool)
acviDryRun = lens _acviDryRun (\ s a -> s{_acviDryRun = a})

-- | The ID of the Client VPN endpoint.
acviClientVPNEndpointId :: Lens' AuthorizeClientVPNIngress Text
acviClientVPNEndpointId = lens _acviClientVPNEndpointId (\ s a -> s{_acviClientVPNEndpointId = a})

-- | The IPv4 address range, in CIDR notation, of the network for which access is being authorized.
acviTargetNetworkCidr :: Lens' AuthorizeClientVPNIngress Text
acviTargetNetworkCidr = lens _acviTargetNetworkCidr (\ s a -> s{_acviTargetNetworkCidr = a})

instance AWSRequest AuthorizeClientVPNIngress where
        type Rs AuthorizeClientVPNIngress =
             AuthorizeClientVPNIngressResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AuthorizeClientVPNIngressResponse' <$>
                   (x .@? "status") <*> (pure (fromEnum s)))

instance Hashable AuthorizeClientVPNIngress where

instance NFData AuthorizeClientVPNIngress where

instance ToHeaders AuthorizeClientVPNIngress where
        toHeaders = const mempty

instance ToPath AuthorizeClientVPNIngress where
        toPath = const "/"

instance ToQuery AuthorizeClientVPNIngress where
        toQuery AuthorizeClientVPNIngress'{..}
          = mconcat
              ["Action" =:
                 ("AuthorizeClientVpnIngress" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AccessGroupId" =: _acviAccessGroupId,
               "AuthorizeAllGroups" =: _acviAuthorizeAllGroups,
               "Description" =: _acviDescription,
               "DryRun" =: _acviDryRun,
               "ClientVpnEndpointId" =: _acviClientVPNEndpointId,
               "TargetNetworkCidr" =: _acviTargetNetworkCidr]

-- | /See:/ 'authorizeClientVPNIngressResponse' smart constructor.
data AuthorizeClientVPNIngressResponse = AuthorizeClientVPNIngressResponse'
  { _acvirsStatus         :: !(Maybe ClientVPNAuthorizationRuleStatus)
  , _acvirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeClientVPNIngressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acvirsStatus' - The current state of the authorization rule.
--
-- * 'acvirsResponseStatus' - -- | The response status code.
authorizeClientVPNIngressResponse
    :: Int -- ^ 'acvirsResponseStatus'
    -> AuthorizeClientVPNIngressResponse
authorizeClientVPNIngressResponse pResponseStatus_ =
  AuthorizeClientVPNIngressResponse'
    {_acvirsStatus = Nothing, _acvirsResponseStatus = pResponseStatus_}


-- | The current state of the authorization rule.
acvirsStatus :: Lens' AuthorizeClientVPNIngressResponse (Maybe ClientVPNAuthorizationRuleStatus)
acvirsStatus = lens _acvirsStatus (\ s a -> s{_acvirsStatus = a})

-- | -- | The response status code.
acvirsResponseStatus :: Lens' AuthorizeClientVPNIngressResponse Int
acvirsResponseStatus = lens _acvirsResponseStatus (\ s a -> s{_acvirsResponseStatus = a})

instance NFData AuthorizeClientVPNIngressResponse
         where
