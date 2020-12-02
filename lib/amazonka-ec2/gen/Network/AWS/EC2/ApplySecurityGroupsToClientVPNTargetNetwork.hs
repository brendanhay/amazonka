{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ApplySecurityGroupsToClientVPNTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a security group to the association between the target network and the Client VPN endpoint. This action replaces the existing security groups with the specified security groups.
module Network.AWS.EC2.ApplySecurityGroupsToClientVPNTargetNetwork
  ( -- * Creating a Request
    applySecurityGroupsToClientVPNTargetNetwork,
    ApplySecurityGroupsToClientVPNTargetNetwork,

    -- * Request Lenses
    asgtcvtnDryRun,
    asgtcvtnClientVPNEndpointId,
    asgtcvtnVPCId,
    asgtcvtnSecurityGroupIds,

    -- * Destructuring the Response
    applySecurityGroupsToClientVPNTargetNetworkResponse,
    ApplySecurityGroupsToClientVPNTargetNetworkResponse,

    -- * Response Lenses
    asgtcvtnrsSecurityGroupIds,
    asgtcvtnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'applySecurityGroupsToClientVPNTargetNetwork' smart constructor.
data ApplySecurityGroupsToClientVPNTargetNetwork = ApplySecurityGroupsToClientVPNTargetNetwork'
  { _asgtcvtnDryRun ::
      !( Maybe
           Bool
       ),
    _asgtcvtnClientVPNEndpointId ::
      !Text,
    _asgtcvtnVPCId ::
      !Text,
    _asgtcvtnSecurityGroupIds ::
      ![Text]
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ApplySecurityGroupsToClientVPNTargetNetwork' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgtcvtnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'asgtcvtnClientVPNEndpointId' - The ID of the Client VPN endpoint.
--
-- * 'asgtcvtnVPCId' - The ID of the VPC in which the associated target network is located.
--
-- * 'asgtcvtnSecurityGroupIds' - The IDs of the security groups to apply to the associated target network. Up to 5 security groups can be applied to an associated target network.
applySecurityGroupsToClientVPNTargetNetwork ::
  -- | 'asgtcvtnClientVPNEndpointId'
  Text ->
  -- | 'asgtcvtnVPCId'
  Text ->
  ApplySecurityGroupsToClientVPNTargetNetwork
applySecurityGroupsToClientVPNTargetNetwork
  pClientVPNEndpointId_
  pVPCId_ =
    ApplySecurityGroupsToClientVPNTargetNetwork'
      { _asgtcvtnDryRun =
          Nothing,
        _asgtcvtnClientVPNEndpointId =
          pClientVPNEndpointId_,
        _asgtcvtnVPCId = pVPCId_,
        _asgtcvtnSecurityGroupIds = mempty
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
asgtcvtnDryRun :: Lens' ApplySecurityGroupsToClientVPNTargetNetwork (Maybe Bool)
asgtcvtnDryRun = lens _asgtcvtnDryRun (\s a -> s {_asgtcvtnDryRun = a})

-- | The ID of the Client VPN endpoint.
asgtcvtnClientVPNEndpointId :: Lens' ApplySecurityGroupsToClientVPNTargetNetwork Text
asgtcvtnClientVPNEndpointId = lens _asgtcvtnClientVPNEndpointId (\s a -> s {_asgtcvtnClientVPNEndpointId = a})

-- | The ID of the VPC in which the associated target network is located.
asgtcvtnVPCId :: Lens' ApplySecurityGroupsToClientVPNTargetNetwork Text
asgtcvtnVPCId = lens _asgtcvtnVPCId (\s a -> s {_asgtcvtnVPCId = a})

-- | The IDs of the security groups to apply to the associated target network. Up to 5 security groups can be applied to an associated target network.
asgtcvtnSecurityGroupIds :: Lens' ApplySecurityGroupsToClientVPNTargetNetwork [Text]
asgtcvtnSecurityGroupIds = lens _asgtcvtnSecurityGroupIds (\s a -> s {_asgtcvtnSecurityGroupIds = a}) . _Coerce

instance AWSRequest ApplySecurityGroupsToClientVPNTargetNetwork where
  type
    Rs ApplySecurityGroupsToClientVPNTargetNetwork =
      ApplySecurityGroupsToClientVPNTargetNetworkResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ApplySecurityGroupsToClientVPNTargetNetworkResponse'
            <$> (x .@? "securityGroupIds" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable ApplySecurityGroupsToClientVPNTargetNetwork

instance NFData ApplySecurityGroupsToClientVPNTargetNetwork

instance ToHeaders ApplySecurityGroupsToClientVPNTargetNetwork where
  toHeaders = const mempty

instance ToPath ApplySecurityGroupsToClientVPNTargetNetwork where
  toPath = const "/"

instance ToQuery ApplySecurityGroupsToClientVPNTargetNetwork where
  toQuery ApplySecurityGroupsToClientVPNTargetNetwork' {..} =
    mconcat
      [ "Action"
          =: ("ApplySecurityGroupsToClientVpnTargetNetwork" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _asgtcvtnDryRun,
        "ClientVpnEndpointId" =: _asgtcvtnClientVPNEndpointId,
        "VpcId" =: _asgtcvtnVPCId,
        toQueryList "SecurityGroupId" _asgtcvtnSecurityGroupIds
      ]

-- | /See:/ 'applySecurityGroupsToClientVPNTargetNetworkResponse' smart constructor.
data ApplySecurityGroupsToClientVPNTargetNetworkResponse = ApplySecurityGroupsToClientVPNTargetNetworkResponse'
  { _asgtcvtnrsSecurityGroupIds ::
      !( Maybe
           [Text]
       ),
    _asgtcvtnrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ApplySecurityGroupsToClientVPNTargetNetworkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgtcvtnrsSecurityGroupIds' - The IDs of the applied security groups.
--
-- * 'asgtcvtnrsResponseStatus' - -- | The response status code.
applySecurityGroupsToClientVPNTargetNetworkResponse ::
  -- | 'asgtcvtnrsResponseStatus'
  Int ->
  ApplySecurityGroupsToClientVPNTargetNetworkResponse
applySecurityGroupsToClientVPNTargetNetworkResponse
  pResponseStatus_ =
    ApplySecurityGroupsToClientVPNTargetNetworkResponse'
      { _asgtcvtnrsSecurityGroupIds =
          Nothing,
        _asgtcvtnrsResponseStatus =
          pResponseStatus_
      }

-- | The IDs of the applied security groups.
asgtcvtnrsSecurityGroupIds :: Lens' ApplySecurityGroupsToClientVPNTargetNetworkResponse [Text]
asgtcvtnrsSecurityGroupIds = lens _asgtcvtnrsSecurityGroupIds (\s a -> s {_asgtcvtnrsSecurityGroupIds = a}) . _Default . _Coerce

-- | -- | The response status code.
asgtcvtnrsResponseStatus :: Lens' ApplySecurityGroupsToClientVPNTargetNetworkResponse Int
asgtcvtnrsResponseStatus = lens _asgtcvtnrsResponseStatus (\s a -> s {_asgtcvtnrsResponseStatus = a})

instance NFData ApplySecurityGroupsToClientVPNTargetNetworkResponse
