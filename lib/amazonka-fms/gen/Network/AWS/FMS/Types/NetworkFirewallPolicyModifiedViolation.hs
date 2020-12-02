{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation where

import Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Violation details for AWS Network Firewall for a firewall policy that has a different 'NetworkFirewallPolicyDescription' than is required by the Firewall Manager policy.
--
--
--
-- /See:/ 'networkFirewallPolicyModifiedViolation' smart constructor.
data NetworkFirewallPolicyModifiedViolation = NetworkFirewallPolicyModifiedViolation'
  { _nfpmvCurrentPolicyDescription ::
      !( Maybe
           NetworkFirewallPolicyDescription
       ),
    _nfpmvViolationTarget ::
      !(Maybe Text),
    _nfpmvExpectedPolicyDescription ::
      !( Maybe
           NetworkFirewallPolicyDescription
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkFirewallPolicyModifiedViolation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfpmvCurrentPolicyDescription' - The policy that's currently in use in the individual account.
--
-- * 'nfpmvViolationTarget' - The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- * 'nfpmvExpectedPolicyDescription' - The policy that should be in use in the individual account in order to be compliant.
networkFirewallPolicyModifiedViolation ::
  NetworkFirewallPolicyModifiedViolation
networkFirewallPolicyModifiedViolation =
  NetworkFirewallPolicyModifiedViolation'
    { _nfpmvCurrentPolicyDescription =
        Nothing,
      _nfpmvViolationTarget = Nothing,
      _nfpmvExpectedPolicyDescription = Nothing
    }

-- | The policy that's currently in use in the individual account.
nfpmvCurrentPolicyDescription :: Lens' NetworkFirewallPolicyModifiedViolation (Maybe NetworkFirewallPolicyDescription)
nfpmvCurrentPolicyDescription = lens _nfpmvCurrentPolicyDescription (\s a -> s {_nfpmvCurrentPolicyDescription = a})

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
nfpmvViolationTarget :: Lens' NetworkFirewallPolicyModifiedViolation (Maybe Text)
nfpmvViolationTarget = lens _nfpmvViolationTarget (\s a -> s {_nfpmvViolationTarget = a})

-- | The policy that should be in use in the individual account in order to be compliant.
nfpmvExpectedPolicyDescription :: Lens' NetworkFirewallPolicyModifiedViolation (Maybe NetworkFirewallPolicyDescription)
nfpmvExpectedPolicyDescription = lens _nfpmvExpectedPolicyDescription (\s a -> s {_nfpmvExpectedPolicyDescription = a})

instance FromJSON NetworkFirewallPolicyModifiedViolation where
  parseJSON =
    withObject
      "NetworkFirewallPolicyModifiedViolation"
      ( \x ->
          NetworkFirewallPolicyModifiedViolation'
            <$> (x .:? "CurrentPolicyDescription")
            <*> (x .:? "ViolationTarget")
            <*> (x .:? "ExpectedPolicyDescription")
      )

instance Hashable NetworkFirewallPolicyModifiedViolation

instance NFData NetworkFirewallPolicyModifiedViolation
