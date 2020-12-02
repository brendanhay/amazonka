{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Violation details for AWS Network Firewall for a subnet that's not associated to the expected Firewall Manager managed route table.
--
--
--
-- /See:/ 'networkFirewallMissingExpectedRTViolation' smart constructor.
data NetworkFirewallMissingExpectedRTViolation = NetworkFirewallMissingExpectedRTViolation'
  { _nfmertvCurrentRouteTable ::
      !( Maybe
           Text
       ),
    _nfmertvAvailabilityZone ::
      !( Maybe
           Text
       ),
    _nfmertvVPC ::
      !( Maybe
           Text
       ),
    _nfmertvViolationTarget ::
      !( Maybe
           Text
       ),
    _nfmertvExpectedRouteTable ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'NetworkFirewallMissingExpectedRTViolation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfmertvCurrentRouteTable' - The resource ID of the current route table that's associated with the subnet, if one is available.
--
-- * 'nfmertvAvailabilityZone' - The Availability Zone of a violating subnet.
--
-- * 'nfmertvVPC' - The resource ID of the VPC associated with a violating subnet.
--
-- * 'nfmertvViolationTarget' - The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- * 'nfmertvExpectedRouteTable' - The resource ID of the route table that should be associated with the subnet.
networkFirewallMissingExpectedRTViolation ::
  NetworkFirewallMissingExpectedRTViolation
networkFirewallMissingExpectedRTViolation =
  NetworkFirewallMissingExpectedRTViolation'
    { _nfmertvCurrentRouteTable =
        Nothing,
      _nfmertvAvailabilityZone = Nothing,
      _nfmertvVPC = Nothing,
      _nfmertvViolationTarget = Nothing,
      _nfmertvExpectedRouteTable = Nothing
    }

-- | The resource ID of the current route table that's associated with the subnet, if one is available.
nfmertvCurrentRouteTable :: Lens' NetworkFirewallMissingExpectedRTViolation (Maybe Text)
nfmertvCurrentRouteTable = lens _nfmertvCurrentRouteTable (\s a -> s {_nfmertvCurrentRouteTable = a})

-- | The Availability Zone of a violating subnet.
nfmertvAvailabilityZone :: Lens' NetworkFirewallMissingExpectedRTViolation (Maybe Text)
nfmertvAvailabilityZone = lens _nfmertvAvailabilityZone (\s a -> s {_nfmertvAvailabilityZone = a})

-- | The resource ID of the VPC associated with a violating subnet.
nfmertvVPC :: Lens' NetworkFirewallMissingExpectedRTViolation (Maybe Text)
nfmertvVPC = lens _nfmertvVPC (\s a -> s {_nfmertvVPC = a})

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
nfmertvViolationTarget :: Lens' NetworkFirewallMissingExpectedRTViolation (Maybe Text)
nfmertvViolationTarget = lens _nfmertvViolationTarget (\s a -> s {_nfmertvViolationTarget = a})

-- | The resource ID of the route table that should be associated with the subnet.
nfmertvExpectedRouteTable :: Lens' NetworkFirewallMissingExpectedRTViolation (Maybe Text)
nfmertvExpectedRouteTable = lens _nfmertvExpectedRouteTable (\s a -> s {_nfmertvExpectedRouteTable = a})

instance FromJSON NetworkFirewallMissingExpectedRTViolation where
  parseJSON =
    withObject
      "NetworkFirewallMissingExpectedRTViolation"
      ( \x ->
          NetworkFirewallMissingExpectedRTViolation'
            <$> (x .:? "CurrentRouteTable")
            <*> (x .:? "AvailabilityZone")
            <*> (x .:? "VPC")
            <*> (x .:? "ViolationTarget")
            <*> (x .:? "ExpectedRouteTable")
      )

instance Hashable NetworkFirewallMissingExpectedRTViolation

instance NFData NetworkFirewallMissingExpectedRTViolation
