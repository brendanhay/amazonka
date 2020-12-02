{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyVPNTunnelOptionsSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ModifyVPNTunnelOptionsSpecification where

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

-- | The AWS Site-to-Site VPN tunnel options to modify.
--
--
--
-- /See:/ 'modifyVPNTunnelOptionsSpecification' smart constructor.
data ModifyVPNTunnelOptionsSpecification = ModifyVPNTunnelOptionsSpecification'
  { _mvtosReplayWindowSize ::
      !(Maybe Int),
    _mvtosDPDTimeoutAction ::
      !(Maybe Text),
    _mvtosRekeyFuzzPercentage ::
      !(Maybe Int),
    _mvtosPhase1LifetimeSeconds ::
      !(Maybe Int),
    _mvtosIKEVersions ::
      !( Maybe
           [IKEVersionsRequestListValue]
       ),
    _mvtosPhase2IntegrityAlgorithms ::
      !( Maybe
           [Phase2IntegrityAlgorithmsRequestListValue]
       ),
    _mvtosPhase2LifetimeSeconds ::
      !(Maybe Int),
    _mvtosPhase1EncryptionAlgorithms ::
      !( Maybe
           [Phase1EncryptionAlgorithmsRequestListValue]
       ),
    _mvtosPhase1DHGroupNumbers ::
      !( Maybe
           [Phase1DHGroupNumbersRequestListValue]
       ),
    _mvtosPhase1IntegrityAlgorithms ::
      !( Maybe
           [Phase1IntegrityAlgorithmsRequestListValue]
       ),
    _mvtosRekeyMarginTimeSeconds ::
      !(Maybe Int),
    _mvtosDPDTimeoutSeconds ::
      !(Maybe Int),
    _mvtosTunnelInsideCidr ::
      !(Maybe Text),
    _mvtosStartupAction ::
      !(Maybe Text),
    _mvtosPhase2EncryptionAlgorithms ::
      !( Maybe
           [Phase2EncryptionAlgorithmsRequestListValue]
       ),
    _mvtosPhase2DHGroupNumbers ::
      !( Maybe
           [Phase2DHGroupNumbersRequestListValue]
       ),
    _mvtosPreSharedKey ::
      !(Maybe Text),
    _mvtosTunnelInsideIPv6Cidr ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyVPNTunnelOptionsSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvtosReplayWindowSize' - The number of packets in an IKE replay window. Constraints: A value between 64 and 2048. Default: @1024@
--
-- * 'mvtosDPDTimeoutAction' - The action to take after DPD timeout occurs. Specify @restart@ to restart the IKE initiation. Specify @clear@ to end the IKE session. Valid Values: @clear@ | @none@ | @restart@  Default: @clear@
--
-- * 'mvtosRekeyFuzzPercentage' - The percentage of the rekey window (determined by @RekeyMarginTimeSeconds@ ) during which the rekey time is randomly selected. Constraints: A value between 0 and 100. Default: @100@
--
-- * 'mvtosPhase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds. Constraints: A value between 900 and 28,800. Default: @28800@
--
-- * 'mvtosIKEVersions' - The IKE versions that are permitted for the VPN tunnel. Valid values: @ikev1@ | @ikev2@
--
-- * 'mvtosPhase2IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- * 'mvtosPhase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds. Constraints: A value between 900 and 3,600. The value must be less than the value for @Phase1LifetimeSeconds@ . Default: @3600@
--
-- * 'mvtosPhase1EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- * 'mvtosPhase1DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
--
-- * 'mvtosPhase1IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- * 'mvtosRekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey. The exact time of the rekey is randomly selected based on the value for @RekeyFuzzPercentage@ . Constraints: A value between 60 and half of @Phase2LifetimeSeconds@ . Default: @540@
--
-- * 'mvtosDPDTimeoutSeconds' - The number of seconds after which a DPD timeout occurs. Constraints: A value between 0 and 30. Default: @30@
--
-- * 'mvtosTunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same virtual private gateway.  Constraints: A size /30 CIDR block from the @169.254.0.0/16@ range. The following CIDR blocks are reserved and cannot be used:     * @169.254.0.0/30@      * @169.254.1.0/30@      * @169.254.2.0/30@      * @169.254.3.0/30@      * @169.254.4.0/30@      * @169.254.5.0/30@      * @169.254.169.252/30@
--
-- * 'mvtosStartupAction' - The action to take when the establishing the tunnel for the VPN connection. By default, your customer gateway device must initiate the IKE negotiation and bring up the tunnel. Specify @start@ for AWS to initiate the IKE negotiation. Valid Values: @add@ | @start@  Default: @add@
--
-- * 'mvtosPhase2EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- * 'mvtosPhase2DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
--
-- * 'mvtosPreSharedKey' - The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway. Constraints: Allowed characters are alphanumeric characters, periods (.), and underscores (_). Must be between 8 and 64 characters in length and cannot start with zero (0).
--
-- * 'mvtosTunnelInsideIPv6Cidr' - The range of inside IPv6 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same transit gateway. Constraints: A size /126 CIDR block from the local @fd00::/8@ range.
modifyVPNTunnelOptionsSpecification ::
  ModifyVPNTunnelOptionsSpecification
modifyVPNTunnelOptionsSpecification =
  ModifyVPNTunnelOptionsSpecification'
    { _mvtosReplayWindowSize =
        Nothing,
      _mvtosDPDTimeoutAction = Nothing,
      _mvtosRekeyFuzzPercentage = Nothing,
      _mvtosPhase1LifetimeSeconds = Nothing,
      _mvtosIKEVersions = Nothing,
      _mvtosPhase2IntegrityAlgorithms = Nothing,
      _mvtosPhase2LifetimeSeconds = Nothing,
      _mvtosPhase1EncryptionAlgorithms = Nothing,
      _mvtosPhase1DHGroupNumbers = Nothing,
      _mvtosPhase1IntegrityAlgorithms = Nothing,
      _mvtosRekeyMarginTimeSeconds = Nothing,
      _mvtosDPDTimeoutSeconds = Nothing,
      _mvtosTunnelInsideCidr = Nothing,
      _mvtosStartupAction = Nothing,
      _mvtosPhase2EncryptionAlgorithms = Nothing,
      _mvtosPhase2DHGroupNumbers = Nothing,
      _mvtosPreSharedKey = Nothing,
      _mvtosTunnelInsideIPv6Cidr = Nothing
    }

-- | The number of packets in an IKE replay window. Constraints: A value between 64 and 2048. Default: @1024@
mvtosReplayWindowSize :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Int)
mvtosReplayWindowSize = lens _mvtosReplayWindowSize (\s a -> s {_mvtosReplayWindowSize = a})

-- | The action to take after DPD timeout occurs. Specify @restart@ to restart the IKE initiation. Specify @clear@ to end the IKE session. Valid Values: @clear@ | @none@ | @restart@  Default: @clear@
mvtosDPDTimeoutAction :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Text)
mvtosDPDTimeoutAction = lens _mvtosDPDTimeoutAction (\s a -> s {_mvtosDPDTimeoutAction = a})

-- | The percentage of the rekey window (determined by @RekeyMarginTimeSeconds@ ) during which the rekey time is randomly selected. Constraints: A value between 0 and 100. Default: @100@
mvtosRekeyFuzzPercentage :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Int)
mvtosRekeyFuzzPercentage = lens _mvtosRekeyFuzzPercentage (\s a -> s {_mvtosRekeyFuzzPercentage = a})

-- | The lifetime for phase 1 of the IKE negotiation, in seconds. Constraints: A value between 900 and 28,800. Default: @28800@
mvtosPhase1LifetimeSeconds :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Int)
mvtosPhase1LifetimeSeconds = lens _mvtosPhase1LifetimeSeconds (\s a -> s {_mvtosPhase1LifetimeSeconds = a})

-- | The IKE versions that are permitted for the VPN tunnel. Valid values: @ikev1@ | @ikev2@
mvtosIKEVersions :: Lens' ModifyVPNTunnelOptionsSpecification [IKEVersionsRequestListValue]
mvtosIKEVersions = lens _mvtosIKEVersions (\s a -> s {_mvtosIKEVersions = a}) . _Default . _Coerce

-- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
mvtosPhase2IntegrityAlgorithms :: Lens' ModifyVPNTunnelOptionsSpecification [Phase2IntegrityAlgorithmsRequestListValue]
mvtosPhase2IntegrityAlgorithms = lens _mvtosPhase2IntegrityAlgorithms (\s a -> s {_mvtosPhase2IntegrityAlgorithms = a}) . _Default . _Coerce

-- | The lifetime for phase 2 of the IKE negotiation, in seconds. Constraints: A value between 900 and 3,600. The value must be less than the value for @Phase1LifetimeSeconds@ . Default: @3600@
mvtosPhase2LifetimeSeconds :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Int)
mvtosPhase2LifetimeSeconds = lens _mvtosPhase2LifetimeSeconds (\s a -> s {_mvtosPhase2LifetimeSeconds = a})

-- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
mvtosPhase1EncryptionAlgorithms :: Lens' ModifyVPNTunnelOptionsSpecification [Phase1EncryptionAlgorithmsRequestListValue]
mvtosPhase1EncryptionAlgorithms = lens _mvtosPhase1EncryptionAlgorithms (\s a -> s {_mvtosPhase1EncryptionAlgorithms = a}) . _Default . _Coerce

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
mvtosPhase1DHGroupNumbers :: Lens' ModifyVPNTunnelOptionsSpecification [Phase1DHGroupNumbersRequestListValue]
mvtosPhase1DHGroupNumbers = lens _mvtosPhase1DHGroupNumbers (\s a -> s {_mvtosPhase1DHGroupNumbers = a}) . _Default . _Coerce

-- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations. Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
mvtosPhase1IntegrityAlgorithms :: Lens' ModifyVPNTunnelOptionsSpecification [Phase1IntegrityAlgorithmsRequestListValue]
mvtosPhase1IntegrityAlgorithms = lens _mvtosPhase1IntegrityAlgorithms (\s a -> s {_mvtosPhase1IntegrityAlgorithms = a}) . _Default . _Coerce

-- | The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey. The exact time of the rekey is randomly selected based on the value for @RekeyFuzzPercentage@ . Constraints: A value between 60 and half of @Phase2LifetimeSeconds@ . Default: @540@
mvtosRekeyMarginTimeSeconds :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Int)
mvtosRekeyMarginTimeSeconds = lens _mvtosRekeyMarginTimeSeconds (\s a -> s {_mvtosRekeyMarginTimeSeconds = a})

-- | The number of seconds after which a DPD timeout occurs. Constraints: A value between 0 and 30. Default: @30@
mvtosDPDTimeoutSeconds :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Int)
mvtosDPDTimeoutSeconds = lens _mvtosDPDTimeoutSeconds (\s a -> s {_mvtosDPDTimeoutSeconds = a})

-- | The range of inside IPv4 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same virtual private gateway.  Constraints: A size /30 CIDR block from the @169.254.0.0/16@ range. The following CIDR blocks are reserved and cannot be used:     * @169.254.0.0/30@      * @169.254.1.0/30@      * @169.254.2.0/30@      * @169.254.3.0/30@      * @169.254.4.0/30@      * @169.254.5.0/30@      * @169.254.169.252/30@
mvtosTunnelInsideCidr :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Text)
mvtosTunnelInsideCidr = lens _mvtosTunnelInsideCidr (\s a -> s {_mvtosTunnelInsideCidr = a})

-- | The action to take when the establishing the tunnel for the VPN connection. By default, your customer gateway device must initiate the IKE negotiation and bring up the tunnel. Specify @start@ for AWS to initiate the IKE negotiation. Valid Values: @add@ | @start@  Default: @add@
mvtosStartupAction :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Text)
mvtosStartupAction = lens _mvtosStartupAction (\s a -> s {_mvtosStartupAction = a})

-- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
mvtosPhase2EncryptionAlgorithms :: Lens' ModifyVPNTunnelOptionsSpecification [Phase2EncryptionAlgorithmsRequestListValue]
mvtosPhase2EncryptionAlgorithms = lens _mvtosPhase2EncryptionAlgorithms (\s a -> s {_mvtosPhase2EncryptionAlgorithms = a}) . _Default . _Coerce

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 2 IKE negotiations. Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
mvtosPhase2DHGroupNumbers :: Lens' ModifyVPNTunnelOptionsSpecification [Phase2DHGroupNumbersRequestListValue]
mvtosPhase2DHGroupNumbers = lens _mvtosPhase2DHGroupNumbers (\s a -> s {_mvtosPhase2DHGroupNumbers = a}) . _Default . _Coerce

-- | The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway. Constraints: Allowed characters are alphanumeric characters, periods (.), and underscores (_). Must be between 8 and 64 characters in length and cannot start with zero (0).
mvtosPreSharedKey :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Text)
mvtosPreSharedKey = lens _mvtosPreSharedKey (\s a -> s {_mvtosPreSharedKey = a})

-- | The range of inside IPv6 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same transit gateway. Constraints: A size /126 CIDR block from the local @fd00::/8@ range.
mvtosTunnelInsideIPv6Cidr :: Lens' ModifyVPNTunnelOptionsSpecification (Maybe Text)
mvtosTunnelInsideIPv6Cidr = lens _mvtosTunnelInsideIPv6Cidr (\s a -> s {_mvtosTunnelInsideIPv6Cidr = a})

instance Hashable ModifyVPNTunnelOptionsSpecification

instance NFData ModifyVPNTunnelOptionsSpecification

instance ToQuery ModifyVPNTunnelOptionsSpecification where
  toQuery ModifyVPNTunnelOptionsSpecification' {..} =
    mconcat
      [ "ReplayWindowSize" =: _mvtosReplayWindowSize,
        "DPDTimeoutAction" =: _mvtosDPDTimeoutAction,
        "RekeyFuzzPercentage" =: _mvtosRekeyFuzzPercentage,
        "Phase1LifetimeSeconds" =: _mvtosPhase1LifetimeSeconds,
        toQuery (toQueryList "IKEVersion" <$> _mvtosIKEVersions),
        toQuery
          ( toQueryList "Phase2IntegrityAlgorithm"
              <$> _mvtosPhase2IntegrityAlgorithms
          ),
        "Phase2LifetimeSeconds" =: _mvtosPhase2LifetimeSeconds,
        toQuery
          ( toQueryList "Phase1EncryptionAlgorithm"
              <$> _mvtosPhase1EncryptionAlgorithms
          ),
        toQuery
          (toQueryList "Phase1DHGroupNumber" <$> _mvtosPhase1DHGroupNumbers),
        toQuery
          ( toQueryList "Phase1IntegrityAlgorithm"
              <$> _mvtosPhase1IntegrityAlgorithms
          ),
        "RekeyMarginTimeSeconds" =: _mvtosRekeyMarginTimeSeconds,
        "DPDTimeoutSeconds" =: _mvtosDPDTimeoutSeconds,
        "TunnelInsideCidr" =: _mvtosTunnelInsideCidr,
        "StartupAction" =: _mvtosStartupAction,
        toQuery
          ( toQueryList "Phase2EncryptionAlgorithm"
              <$> _mvtosPhase2EncryptionAlgorithms
          ),
        toQuery
          (toQueryList "Phase2DHGroupNumber" <$> _mvtosPhase2DHGroupNumbers),
        "PreSharedKey" =: _mvtosPreSharedKey,
        "TunnelInsideIpv6Cidr" =: _mvtosTunnelInsideIPv6Cidr
      ]
