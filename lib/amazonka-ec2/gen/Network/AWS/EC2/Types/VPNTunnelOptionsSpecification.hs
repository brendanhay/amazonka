{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNTunnelOptionsSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNTunnelOptionsSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IKEVersionsRequestListValue
import Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
import Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
import Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The tunnel options for a single VPN tunnel.
--
--
--
-- /See:/ 'vpnTunnelOptionsSpecification' smart constructor.
data VPNTunnelOptionsSpecification = VPNTunnelOptionsSpecification'
  { _vtosReplayWindowSize ::
      !(Maybe Int),
    _vtosDPDTimeoutAction ::
      !(Maybe Text),
    _vtosRekeyFuzzPercentage ::
      !(Maybe Int),
    _vtosPhase1LifetimeSeconds ::
      !(Maybe Int),
    _vtosIKEVersions ::
      !( Maybe
           [IKEVersionsRequestListValue]
       ),
    _vtosPhase2IntegrityAlgorithms ::
      !( Maybe
           [Phase2IntegrityAlgorithmsRequestListValue]
       ),
    _vtosPhase2LifetimeSeconds ::
      !(Maybe Int),
    _vtosPhase1EncryptionAlgorithms ::
      !( Maybe
           [Phase1EncryptionAlgorithmsRequestListValue]
       ),
    _vtosPhase1DHGroupNumbers ::
      !( Maybe
           [Phase1DHGroupNumbersRequestListValue]
       ),
    _vtosPhase1IntegrityAlgorithms ::
      !( Maybe
           [Phase1IntegrityAlgorithmsRequestListValue]
       ),
    _vtosRekeyMarginTimeSeconds ::
      !(Maybe Int),
    _vtosDPDTimeoutSeconds ::
      !(Maybe Int),
    _vtosTunnelInsideCidr ::
      !(Maybe Text),
    _vtosStartupAction ::
      !(Maybe Text),
    _vtosPhase2EncryptionAlgorithms ::
      !( Maybe
           [Phase2EncryptionAlgorithmsRequestListValue]
       ),
    _vtosPhase2DHGroupNumbers ::
      !( Maybe
           [Phase2DHGroupNumbersRequestListValue]
       ),
    _vtosPreSharedKey ::
      !(Maybe Text),
    _vtosTunnelInsideIPv6Cidr ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPNTunnelOptionsSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtosReplayWindowSize' - The number of packets in an IKE replay window. Constraints: A value between 64 and 2048. Default: @1024@
--
-- * 'vtosDPDTimeoutAction' - The action to take after DPD timeout occurs. Specify @restart@ to restart the IKE initiation. Specify @clear@ to end the IKE session. Valid Values: @clear@ | @none@ | @restart@  Default: @clear@
--
-- * 'vtosRekeyFuzzPercentage' - The percentage of the rekey window (determined by @RekeyMarginTimeSeconds@ ) during which the rekey time is randomly selected. Constraints: A value between 0 and 100. Default: @100@
--
-- * 'vtosPhase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds. Constraints: A value between 900 and 28,800. Default: @28800@
--
-- * 'vtosIKEVersions' - The IKE versions that are permitted for the VPN tunnel. Valid values: @ikev1@ | @ikev2@
--
-- * 'vtosPhase2IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- * 'vtosPhase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds. Constraints: A value between 900 and 3,600. The value must be less than the value for @Phase1LifetimeSeconds@ . Default: @3600@
--
-- * 'vtosPhase1EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- * 'vtosPhase1DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
--
-- * 'vtosPhase1IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- * 'vtosRekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey. The exact time of the rekey is randomly selected based on the value for @RekeyFuzzPercentage@ . Constraints: A value between 60 and half of @Phase2LifetimeSeconds@ . Default: @540@
--
-- * 'vtosDPDTimeoutSeconds' - The number of seconds after which a DPD timeout occurs. Constraints: A value between 0 and 30. Default: @30@
--
-- * 'vtosTunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same virtual private gateway.  Constraints: A size /30 CIDR block from the @169.254.0.0/16@ range. The following CIDR blocks are reserved and cannot be used:     * @169.254.0.0/30@      * @169.254.1.0/30@      * @169.254.2.0/30@      * @169.254.3.0/30@      * @169.254.4.0/30@      * @169.254.5.0/30@      * @169.254.169.252/30@
--
-- * 'vtosStartupAction' - The action to take when the establishing the tunnel for the VPN connection. By default, your customer gateway device must initiate the IKE negotiation and bring up the tunnel. Specify @start@ for AWS to initiate the IKE negotiation. Valid Values: @add@ | @start@  Default: @add@
--
-- * 'vtosPhase2EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- * 'vtosPhase2DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
--
-- * 'vtosPreSharedKey' - The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and customer gateway. Constraints: Allowed characters are alphanumeric characters, periods (.), and underscores (_). Must be between 8 and 64 characters in length and cannot start with zero (0).
--
-- * 'vtosTunnelInsideIPv6Cidr' - The range of inside IPv6 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same transit gateway. Constraints: A size /126 CIDR block from the local @fd00::/8@ range.
vpnTunnelOptionsSpecification ::
  VPNTunnelOptionsSpecification
vpnTunnelOptionsSpecification =
  VPNTunnelOptionsSpecification'
    { _vtosReplayWindowSize = Nothing,
      _vtosDPDTimeoutAction = Nothing,
      _vtosRekeyFuzzPercentage = Nothing,
      _vtosPhase1LifetimeSeconds = Nothing,
      _vtosIKEVersions = Nothing,
      _vtosPhase2IntegrityAlgorithms = Nothing,
      _vtosPhase2LifetimeSeconds = Nothing,
      _vtosPhase1EncryptionAlgorithms = Nothing,
      _vtosPhase1DHGroupNumbers = Nothing,
      _vtosPhase1IntegrityAlgorithms = Nothing,
      _vtosRekeyMarginTimeSeconds = Nothing,
      _vtosDPDTimeoutSeconds = Nothing,
      _vtosTunnelInsideCidr = Nothing,
      _vtosStartupAction = Nothing,
      _vtosPhase2EncryptionAlgorithms = Nothing,
      _vtosPhase2DHGroupNumbers = Nothing,
      _vtosPreSharedKey = Nothing,
      _vtosTunnelInsideIPv6Cidr = Nothing
    }

-- | The number of packets in an IKE replay window. Constraints: A value between 64 and 2048. Default: @1024@
vtosReplayWindowSize :: Lens' VPNTunnelOptionsSpecification (Maybe Int)
vtosReplayWindowSize = lens _vtosReplayWindowSize (\s a -> s {_vtosReplayWindowSize = a})

-- | The action to take after DPD timeout occurs. Specify @restart@ to restart the IKE initiation. Specify @clear@ to end the IKE session. Valid Values: @clear@ | @none@ | @restart@  Default: @clear@
vtosDPDTimeoutAction :: Lens' VPNTunnelOptionsSpecification (Maybe Text)
vtosDPDTimeoutAction = lens _vtosDPDTimeoutAction (\s a -> s {_vtosDPDTimeoutAction = a})

-- | The percentage of the rekey window (determined by @RekeyMarginTimeSeconds@ ) during which the rekey time is randomly selected. Constraints: A value between 0 and 100. Default: @100@
vtosRekeyFuzzPercentage :: Lens' VPNTunnelOptionsSpecification (Maybe Int)
vtosRekeyFuzzPercentage = lens _vtosRekeyFuzzPercentage (\s a -> s {_vtosRekeyFuzzPercentage = a})

-- | The lifetime for phase 1 of the IKE negotiation, in seconds. Constraints: A value between 900 and 28,800. Default: @28800@
vtosPhase1LifetimeSeconds :: Lens' VPNTunnelOptionsSpecification (Maybe Int)
vtosPhase1LifetimeSeconds = lens _vtosPhase1LifetimeSeconds (\s a -> s {_vtosPhase1LifetimeSeconds = a})

-- | The IKE versions that are permitted for the VPN tunnel. Valid values: @ikev1@ | @ikev2@
vtosIKEVersions :: Lens' VPNTunnelOptionsSpecification [IKEVersionsRequestListValue]
vtosIKEVersions = lens _vtosIKEVersions (\s a -> s {_vtosIKEVersions = a}) . _Default . _Coerce

-- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
vtosPhase2IntegrityAlgorithms :: Lens' VPNTunnelOptionsSpecification [Phase2IntegrityAlgorithmsRequestListValue]
vtosPhase2IntegrityAlgorithms = lens _vtosPhase2IntegrityAlgorithms (\s a -> s {_vtosPhase2IntegrityAlgorithms = a}) . _Default . _Coerce

-- | The lifetime for phase 2 of the IKE negotiation, in seconds. Constraints: A value between 900 and 3,600. The value must be less than the value for @Phase1LifetimeSeconds@ . Default: @3600@
vtosPhase2LifetimeSeconds :: Lens' VPNTunnelOptionsSpecification (Maybe Int)
vtosPhase2LifetimeSeconds = lens _vtosPhase2LifetimeSeconds (\s a -> s {_vtosPhase2LifetimeSeconds = a})

-- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
vtosPhase1EncryptionAlgorithms :: Lens' VPNTunnelOptionsSpecification [Phase1EncryptionAlgorithmsRequestListValue]
vtosPhase1EncryptionAlgorithms = lens _vtosPhase1EncryptionAlgorithms (\s a -> s {_vtosPhase1EncryptionAlgorithms = a}) . _Default . _Coerce

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
vtosPhase1DHGroupNumbers :: Lens' VPNTunnelOptionsSpecification [Phase1DHGroupNumbersRequestListValue]
vtosPhase1DHGroupNumbers = lens _vtosPhase1DHGroupNumbers (\s a -> s {_vtosPhase1DHGroupNumbers = a}) . _Default . _Coerce

-- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
vtosPhase1IntegrityAlgorithms :: Lens' VPNTunnelOptionsSpecification [Phase1IntegrityAlgorithmsRequestListValue]
vtosPhase1IntegrityAlgorithms = lens _vtosPhase1IntegrityAlgorithms (\s a -> s {_vtosPhase1IntegrityAlgorithms = a}) . _Default . _Coerce

-- | The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey. The exact time of the rekey is randomly selected based on the value for @RekeyFuzzPercentage@ . Constraints: A value between 60 and half of @Phase2LifetimeSeconds@ . Default: @540@
vtosRekeyMarginTimeSeconds :: Lens' VPNTunnelOptionsSpecification (Maybe Int)
vtosRekeyMarginTimeSeconds = lens _vtosRekeyMarginTimeSeconds (\s a -> s {_vtosRekeyMarginTimeSeconds = a})

-- | The number of seconds after which a DPD timeout occurs. Constraints: A value between 0 and 30. Default: @30@
vtosDPDTimeoutSeconds :: Lens' VPNTunnelOptionsSpecification (Maybe Int)
vtosDPDTimeoutSeconds = lens _vtosDPDTimeoutSeconds (\s a -> s {_vtosDPDTimeoutSeconds = a})

-- | The range of inside IPv4 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same virtual private gateway.  Constraints: A size /30 CIDR block from the @169.254.0.0/16@ range. The following CIDR blocks are reserved and cannot be used:     * @169.254.0.0/30@      * @169.254.1.0/30@      * @169.254.2.0/30@      * @169.254.3.0/30@      * @169.254.4.0/30@      * @169.254.5.0/30@      * @169.254.169.252/30@
vtosTunnelInsideCidr :: Lens' VPNTunnelOptionsSpecification (Maybe Text)
vtosTunnelInsideCidr = lens _vtosTunnelInsideCidr (\s a -> s {_vtosTunnelInsideCidr = a})

-- | The action to take when the establishing the tunnel for the VPN connection. By default, your customer gateway device must initiate the IKE negotiation and bring up the tunnel. Specify @start@ for AWS to initiate the IKE negotiation. Valid Values: @add@ | @start@  Default: @add@
vtosStartupAction :: Lens' VPNTunnelOptionsSpecification (Maybe Text)
vtosStartupAction = lens _vtosStartupAction (\s a -> s {_vtosStartupAction = a})

-- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
vtosPhase2EncryptionAlgorithms :: Lens' VPNTunnelOptionsSpecification [Phase2EncryptionAlgorithmsRequestListValue]
vtosPhase2EncryptionAlgorithms = lens _vtosPhase2EncryptionAlgorithms (\s a -> s {_vtosPhase2EncryptionAlgorithms = a}) . _Default . _Coerce

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
vtosPhase2DHGroupNumbers :: Lens' VPNTunnelOptionsSpecification [Phase2DHGroupNumbersRequestListValue]
vtosPhase2DHGroupNumbers = lens _vtosPhase2DHGroupNumbers (\s a -> s {_vtosPhase2DHGroupNumbers = a}) . _Default . _Coerce

-- | The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and customer gateway. Constraints: Allowed characters are alphanumeric characters, periods (.), and underscores (_). Must be between 8 and 64 characters in length and cannot start with zero (0).
vtosPreSharedKey :: Lens' VPNTunnelOptionsSpecification (Maybe Text)
vtosPreSharedKey = lens _vtosPreSharedKey (\s a -> s {_vtosPreSharedKey = a})

-- | The range of inside IPv6 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same transit gateway. Constraints: A size /126 CIDR block from the local @fd00::/8@ range.
vtosTunnelInsideIPv6Cidr :: Lens' VPNTunnelOptionsSpecification (Maybe Text)
vtosTunnelInsideIPv6Cidr = lens _vtosTunnelInsideIPv6Cidr (\s a -> s {_vtosTunnelInsideIPv6Cidr = a})

instance Hashable VPNTunnelOptionsSpecification

instance NFData VPNTunnelOptionsSpecification

instance ToQuery VPNTunnelOptionsSpecification where
  toQuery VPNTunnelOptionsSpecification' {..} =
    mconcat
      [ "ReplayWindowSize" =: _vtosReplayWindowSize,
        "DPDTimeoutAction" =: _vtosDPDTimeoutAction,
        "RekeyFuzzPercentage" =: _vtosRekeyFuzzPercentage,
        "Phase1LifetimeSeconds" =: _vtosPhase1LifetimeSeconds,
        toQuery (toQueryList "IKEVersion" <$> _vtosIKEVersions),
        toQuery
          ( toQueryList "Phase2IntegrityAlgorithm"
              <$> _vtosPhase2IntegrityAlgorithms
          ),
        "Phase2LifetimeSeconds" =: _vtosPhase2LifetimeSeconds,
        toQuery
          ( toQueryList "Phase1EncryptionAlgorithm"
              <$> _vtosPhase1EncryptionAlgorithms
          ),
        toQuery
          (toQueryList "Phase1DHGroupNumber" <$> _vtosPhase1DHGroupNumbers),
        toQuery
          ( toQueryList "Phase1IntegrityAlgorithm"
              <$> _vtosPhase1IntegrityAlgorithms
          ),
        "RekeyMarginTimeSeconds" =: _vtosRekeyMarginTimeSeconds,
        "DPDTimeoutSeconds" =: _vtosDPDTimeoutSeconds,
        "TunnelInsideCidr" =: _vtosTunnelInsideCidr,
        "StartupAction" =: _vtosStartupAction,
        toQuery
          ( toQueryList "Phase2EncryptionAlgorithm"
              <$> _vtosPhase2EncryptionAlgorithms
          ),
        toQuery
          (toQueryList "Phase2DHGroupNumber" <$> _vtosPhase2DHGroupNumbers),
        "PreSharedKey" =: _vtosPreSharedKey,
        "TunnelInsideIpv6Cidr" =: _vtosTunnelInsideIPv6Cidr
      ]
