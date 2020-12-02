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
-- Module      : Network.AWS.EC2.ModifyVPNTunnelOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the options for a VPN tunnel in an AWS Site-to-Site VPN connection. You can modify multiple options for a tunnel in a single request, but you can only modify one tunnel at a time. For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPNTunnels.html Site-to-Site VPN Tunnel Options for Your Site-to-Site VPN Connection> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.ModifyVPNTunnelOptions
  ( -- * Creating a Request
    modifyVPNTunnelOptions,
    ModifyVPNTunnelOptions,

    -- * Request Lenses
    mvtoDryRun,
    mvtoVPNConnectionId,
    mvtoVPNTunnelOutsideIPAddress,
    mvtoTunnelOptions,

    -- * Destructuring the Response
    modifyVPNTunnelOptionsResponse,
    ModifyVPNTunnelOptionsResponse,

    -- * Response Lenses
    mvtorsVPNConnection,
    mvtorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyVPNTunnelOptions' smart constructor.
data ModifyVPNTunnelOptions = ModifyVPNTunnelOptions'
  { _mvtoDryRun ::
      !(Maybe Bool),
    _mvtoVPNConnectionId :: !Text,
    _mvtoVPNTunnelOutsideIPAddress :: !Text,
    _mvtoTunnelOptions ::
      !ModifyVPNTunnelOptionsSpecification
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyVPNTunnelOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvtoDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mvtoVPNConnectionId' - The ID of the AWS Site-to-Site VPN connection.
--
-- * 'mvtoVPNTunnelOutsideIPAddress' - The external IP address of the VPN tunnel.
--
-- * 'mvtoTunnelOptions' - The tunnel options to modify.
modifyVPNTunnelOptions ::
  -- | 'mvtoVPNConnectionId'
  Text ->
  -- | 'mvtoVPNTunnelOutsideIPAddress'
  Text ->
  -- | 'mvtoTunnelOptions'
  ModifyVPNTunnelOptionsSpecification ->
  ModifyVPNTunnelOptions
modifyVPNTunnelOptions
  pVPNConnectionId_
  pVPNTunnelOutsideIPAddress_
  pTunnelOptions_ =
    ModifyVPNTunnelOptions'
      { _mvtoDryRun = Nothing,
        _mvtoVPNConnectionId = pVPNConnectionId_,
        _mvtoVPNTunnelOutsideIPAddress = pVPNTunnelOutsideIPAddress_,
        _mvtoTunnelOptions = pTunnelOptions_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mvtoDryRun :: Lens' ModifyVPNTunnelOptions (Maybe Bool)
mvtoDryRun = lens _mvtoDryRun (\s a -> s {_mvtoDryRun = a})

-- | The ID of the AWS Site-to-Site VPN connection.
mvtoVPNConnectionId :: Lens' ModifyVPNTunnelOptions Text
mvtoVPNConnectionId = lens _mvtoVPNConnectionId (\s a -> s {_mvtoVPNConnectionId = a})

-- | The external IP address of the VPN tunnel.
mvtoVPNTunnelOutsideIPAddress :: Lens' ModifyVPNTunnelOptions Text
mvtoVPNTunnelOutsideIPAddress = lens _mvtoVPNTunnelOutsideIPAddress (\s a -> s {_mvtoVPNTunnelOutsideIPAddress = a})

-- | The tunnel options to modify.
mvtoTunnelOptions :: Lens' ModifyVPNTunnelOptions ModifyVPNTunnelOptionsSpecification
mvtoTunnelOptions = lens _mvtoTunnelOptions (\s a -> s {_mvtoTunnelOptions = a})

instance AWSRequest ModifyVPNTunnelOptions where
  type Rs ModifyVPNTunnelOptions = ModifyVPNTunnelOptionsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyVPNTunnelOptionsResponse'
            <$> (x .@? "vpnConnection") <*> (pure (fromEnum s))
      )

instance Hashable ModifyVPNTunnelOptions

instance NFData ModifyVPNTunnelOptions

instance ToHeaders ModifyVPNTunnelOptions where
  toHeaders = const mempty

instance ToPath ModifyVPNTunnelOptions where
  toPath = const "/"

instance ToQuery ModifyVPNTunnelOptions where
  toQuery ModifyVPNTunnelOptions' {..} =
    mconcat
      [ "Action" =: ("ModifyVpnTunnelOptions" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _mvtoDryRun,
        "VpnConnectionId" =: _mvtoVPNConnectionId,
        "VpnTunnelOutsideIpAddress" =: _mvtoVPNTunnelOutsideIPAddress,
        "TunnelOptions" =: _mvtoTunnelOptions
      ]

-- | /See:/ 'modifyVPNTunnelOptionsResponse' smart constructor.
data ModifyVPNTunnelOptionsResponse = ModifyVPNTunnelOptionsResponse'
  { _mvtorsVPNConnection ::
      !(Maybe VPNConnection),
    _mvtorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyVPNTunnelOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvtorsVPNConnection' - Undocumented member.
--
-- * 'mvtorsResponseStatus' - -- | The response status code.
modifyVPNTunnelOptionsResponse ::
  -- | 'mvtorsResponseStatus'
  Int ->
  ModifyVPNTunnelOptionsResponse
modifyVPNTunnelOptionsResponse pResponseStatus_ =
  ModifyVPNTunnelOptionsResponse'
    { _mvtorsVPNConnection = Nothing,
      _mvtorsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mvtorsVPNConnection :: Lens' ModifyVPNTunnelOptionsResponse (Maybe VPNConnection)
mvtorsVPNConnection = lens _mvtorsVPNConnection (\s a -> s {_mvtorsVPNConnection = a})

-- | -- | The response status code.
mvtorsResponseStatus :: Lens' ModifyVPNTunnelOptionsResponse Int
mvtorsResponseStatus = lens _mvtorsResponseStatus (\s a -> s {_mvtorsResponseStatus = a})

instance NFData ModifyVPNTunnelOptionsResponse
