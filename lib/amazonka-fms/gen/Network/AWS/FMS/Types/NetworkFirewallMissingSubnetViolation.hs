{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Violation details for AWS Network Firewall for an Availability Zone that's missing the expected Firewall Manager managed subnet.
--
--
--
-- /See:/ 'networkFirewallMissingSubnetViolation' smart constructor.
data NetworkFirewallMissingSubnetViolation = NetworkFirewallMissingSubnetViolation'
  { _nfmsvTargetViolationReason ::
      !(Maybe Text),
    _nfmsvAvailabilityZone ::
      !(Maybe Text),
    _nfmsvVPC ::
      !(Maybe Text),
    _nfmsvViolationTarget ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkFirewallMissingSubnetViolation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfmsvTargetViolationReason' - The reason the resource has this violation, if one is available.
--
-- * 'nfmsvAvailabilityZone' - The Availability Zone of a violating subnet.
--
-- * 'nfmsvVPC' - The resource ID of the VPC associated with a violating subnet.
--
-- * 'nfmsvViolationTarget' - The ID of the AWS Network Firewall or VPC resource that's in violation.
networkFirewallMissingSubnetViolation ::
  NetworkFirewallMissingSubnetViolation
networkFirewallMissingSubnetViolation =
  NetworkFirewallMissingSubnetViolation'
    { _nfmsvTargetViolationReason =
        Nothing,
      _nfmsvAvailabilityZone = Nothing,
      _nfmsvVPC = Nothing,
      _nfmsvViolationTarget = Nothing
    }

-- | The reason the resource has this violation, if one is available.
nfmsvTargetViolationReason :: Lens' NetworkFirewallMissingSubnetViolation (Maybe Text)
nfmsvTargetViolationReason = lens _nfmsvTargetViolationReason (\s a -> s {_nfmsvTargetViolationReason = a})

-- | The Availability Zone of a violating subnet.
nfmsvAvailabilityZone :: Lens' NetworkFirewallMissingSubnetViolation (Maybe Text)
nfmsvAvailabilityZone = lens _nfmsvAvailabilityZone (\s a -> s {_nfmsvAvailabilityZone = a})

-- | The resource ID of the VPC associated with a violating subnet.
nfmsvVPC :: Lens' NetworkFirewallMissingSubnetViolation (Maybe Text)
nfmsvVPC = lens _nfmsvVPC (\s a -> s {_nfmsvVPC = a})

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
nfmsvViolationTarget :: Lens' NetworkFirewallMissingSubnetViolation (Maybe Text)
nfmsvViolationTarget = lens _nfmsvViolationTarget (\s a -> s {_nfmsvViolationTarget = a})

instance FromJSON NetworkFirewallMissingSubnetViolation where
  parseJSON =
    withObject
      "NetworkFirewallMissingSubnetViolation"
      ( \x ->
          NetworkFirewallMissingSubnetViolation'
            <$> (x .:? "TargetViolationReason")
            <*> (x .:? "AvailabilityZone")
            <*> (x .:? "VPC")
            <*> (x .:? "ViolationTarget")
      )

instance Hashable NetworkFirewallMissingSubnetViolation

instance NFData NetworkFirewallMissingSubnetViolation
