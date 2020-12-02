{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.VPNDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.VPNDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the virtual private network (VPN) between the VMware vSphere cluster and the AWS website.
--
--
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ >
--
--
-- /See:/ 'vpnDetails' smart constructor.
data VPNDetails = VPNDetails'
  { _vdVPNName :: !(Maybe Text),
    _vdVPNTunnelOriginatorIP :: !(Maybe Text),
    _vdVPNId :: !(Maybe Text),
    _vdVPNState :: !(Maybe Text),
    _vdVPNPSK :: !(Maybe (Sensitive Text)),
    _vdVPNGatewayIP :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPNDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdVPNName' - The name of the VPN.
--
-- * 'vdVPNTunnelOriginatorIP' - The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
--
-- * 'vdVPNId' - The ID of the VPN.
--
-- * 'vdVPNState' - The state of the VPN.
--
-- * 'vdVPNPSK' - The preshared key (PSK) for the VPN.
--
-- * 'vdVPNGatewayIP' - The IP address of network traffic from AWS to your on-premises data center.
vpnDetails ::
  VPNDetails
vpnDetails =
  VPNDetails'
    { _vdVPNName = Nothing,
      _vdVPNTunnelOriginatorIP = Nothing,
      _vdVPNId = Nothing,
      _vdVPNState = Nothing,
      _vdVPNPSK = Nothing,
      _vdVPNGatewayIP = Nothing
    }

-- | The name of the VPN.
vdVPNName :: Lens' VPNDetails (Maybe Text)
vdVPNName = lens _vdVPNName (\s a -> s {_vdVPNName = a})

-- | The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
vdVPNTunnelOriginatorIP :: Lens' VPNDetails (Maybe Text)
vdVPNTunnelOriginatorIP = lens _vdVPNTunnelOriginatorIP (\s a -> s {_vdVPNTunnelOriginatorIP = a})

-- | The ID of the VPN.
vdVPNId :: Lens' VPNDetails (Maybe Text)
vdVPNId = lens _vdVPNId (\s a -> s {_vdVPNId = a})

-- | The state of the VPN.
vdVPNState :: Lens' VPNDetails (Maybe Text)
vdVPNState = lens _vdVPNState (\s a -> s {_vdVPNState = a})

-- | The preshared key (PSK) for the VPN.
vdVPNPSK :: Lens' VPNDetails (Maybe Text)
vdVPNPSK = lens _vdVPNPSK (\s a -> s {_vdVPNPSK = a}) . mapping _Sensitive

-- | The IP address of network traffic from AWS to your on-premises data center.
vdVPNGatewayIP :: Lens' VPNDetails (Maybe Text)
vdVPNGatewayIP = lens _vdVPNGatewayIP (\s a -> s {_vdVPNGatewayIP = a})

instance FromXML VPNDetails where
  parseXML x =
    VPNDetails'
      <$> (x .@? "VpnName")
      <*> (x .@? "VpnTunnelOriginatorIP")
      <*> (x .@? "VpnId")
      <*> (x .@? "VpnState")
      <*> (x .@? "VpnPSK")
      <*> (x .@? "VpnGatewayIp")

instance Hashable VPNDetails

instance NFData VPNDetails
