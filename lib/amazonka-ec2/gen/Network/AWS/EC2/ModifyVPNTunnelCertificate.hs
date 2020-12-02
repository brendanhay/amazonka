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
-- Module      : Network.AWS.EC2.ModifyVPNTunnelCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the VPN tunnel endpoint certificate.
module Network.AWS.EC2.ModifyVPNTunnelCertificate
  ( -- * Creating a Request
    modifyVPNTunnelCertificate,
    ModifyVPNTunnelCertificate,

    -- * Request Lenses
    mvtcDryRun,
    mvtcVPNConnectionId,
    mvtcVPNTunnelOutsideIPAddress,

    -- * Destructuring the Response
    modifyVPNTunnelCertificateResponse,
    ModifyVPNTunnelCertificateResponse,

    -- * Response Lenses
    mvtcrsVPNConnection,
    mvtcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyVPNTunnelCertificate' smart constructor.
data ModifyVPNTunnelCertificate = ModifyVPNTunnelCertificate'
  { _mvtcDryRun ::
      !(Maybe Bool),
    _mvtcVPNConnectionId :: !Text,
    _mvtcVPNTunnelOutsideIPAddress ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyVPNTunnelCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvtcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mvtcVPNConnectionId' - The ID of the AWS Site-to-Site VPN connection.
--
-- * 'mvtcVPNTunnelOutsideIPAddress' - The external IP address of the VPN tunnel.
modifyVPNTunnelCertificate ::
  -- | 'mvtcVPNConnectionId'
  Text ->
  -- | 'mvtcVPNTunnelOutsideIPAddress'
  Text ->
  ModifyVPNTunnelCertificate
modifyVPNTunnelCertificate
  pVPNConnectionId_
  pVPNTunnelOutsideIPAddress_ =
    ModifyVPNTunnelCertificate'
      { _mvtcDryRun = Nothing,
        _mvtcVPNConnectionId = pVPNConnectionId_,
        _mvtcVPNTunnelOutsideIPAddress = pVPNTunnelOutsideIPAddress_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mvtcDryRun :: Lens' ModifyVPNTunnelCertificate (Maybe Bool)
mvtcDryRun = lens _mvtcDryRun (\s a -> s {_mvtcDryRun = a})

-- | The ID of the AWS Site-to-Site VPN connection.
mvtcVPNConnectionId :: Lens' ModifyVPNTunnelCertificate Text
mvtcVPNConnectionId = lens _mvtcVPNConnectionId (\s a -> s {_mvtcVPNConnectionId = a})

-- | The external IP address of the VPN tunnel.
mvtcVPNTunnelOutsideIPAddress :: Lens' ModifyVPNTunnelCertificate Text
mvtcVPNTunnelOutsideIPAddress = lens _mvtcVPNTunnelOutsideIPAddress (\s a -> s {_mvtcVPNTunnelOutsideIPAddress = a})

instance AWSRequest ModifyVPNTunnelCertificate where
  type
    Rs ModifyVPNTunnelCertificate =
      ModifyVPNTunnelCertificateResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyVPNTunnelCertificateResponse'
            <$> (x .@? "vpnConnection") <*> (pure (fromEnum s))
      )

instance Hashable ModifyVPNTunnelCertificate

instance NFData ModifyVPNTunnelCertificate

instance ToHeaders ModifyVPNTunnelCertificate where
  toHeaders = const mempty

instance ToPath ModifyVPNTunnelCertificate where
  toPath = const "/"

instance ToQuery ModifyVPNTunnelCertificate where
  toQuery ModifyVPNTunnelCertificate' {..} =
    mconcat
      [ "Action" =: ("ModifyVpnTunnelCertificate" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _mvtcDryRun,
        "VpnConnectionId" =: _mvtcVPNConnectionId,
        "VpnTunnelOutsideIpAddress" =: _mvtcVPNTunnelOutsideIPAddress
      ]

-- | /See:/ 'modifyVPNTunnelCertificateResponse' smart constructor.
data ModifyVPNTunnelCertificateResponse = ModifyVPNTunnelCertificateResponse'
  { _mvtcrsVPNConnection ::
      !( Maybe
           VPNConnection
       ),
    _mvtcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyVPNTunnelCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvtcrsVPNConnection' - Undocumented member.
--
-- * 'mvtcrsResponseStatus' - -- | The response status code.
modifyVPNTunnelCertificateResponse ::
  -- | 'mvtcrsResponseStatus'
  Int ->
  ModifyVPNTunnelCertificateResponse
modifyVPNTunnelCertificateResponse pResponseStatus_ =
  ModifyVPNTunnelCertificateResponse'
    { _mvtcrsVPNConnection =
        Nothing,
      _mvtcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mvtcrsVPNConnection :: Lens' ModifyVPNTunnelCertificateResponse (Maybe VPNConnection)
mvtcrsVPNConnection = lens _mvtcrsVPNConnection (\s a -> s {_mvtcrsVPNConnection = a})

-- | -- | The response status code.
mvtcrsResponseStatus :: Lens' ModifyVPNTunnelCertificateResponse Int
mvtcrsResponseStatus = lens _mvtcrsResponseStatus (\s a -> s {_mvtcrsResponseStatus = a})

instance NFData ModifyVPNTunnelCertificateResponse
