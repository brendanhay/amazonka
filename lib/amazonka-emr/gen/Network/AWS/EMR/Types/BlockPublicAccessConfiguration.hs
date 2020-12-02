{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BlockPublicAccessConfiguration where

import Network.AWS.EMR.Types.PortRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A configuration for Amazon EMR block public access. When @BlockPublicSecurityGroupRules@ is set to @true@ , Amazon EMR prevents cluster creation if one of the cluster's security groups has a rule that allows inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ .
--
--
--
-- /See:/ 'blockPublicAccessConfiguration' smart constructor.
data BlockPublicAccessConfiguration = BlockPublicAccessConfiguration'
  { _bpacPermittedPublicSecurityGroupRuleRanges ::
      !(Maybe [PortRange]),
    _bpacBlockPublicSecurityGroupRules ::
      !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlockPublicAccessConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpacPermittedPublicSecurityGroupRuleRanges' - Specifies ports and port ranges that are permitted to have security group rules that allow inbound traffic from all public sources. For example, if Port 23 (Telnet) is specified for @PermittedPublicSecurityGroupRuleRanges@ , Amazon EMR allows cluster creation if a security group associated with the cluster has a rule that allows inbound traffic on Port 23 from IPv4 0.0.0.0/0 or IPv6 port ::/0 as the source. By default, Port 22, which is used for SSH access to the cluster EC2 instances, is in the list of @PermittedPublicSecurityGroupRuleRanges@ .
--
-- * 'bpacBlockPublicSecurityGroupRules' - Indicates whether Amazon EMR block public access is enabled (@true@ ) or disabled (@false@ ). By default, the value is @false@ for accounts that have created EMR clusters before July 2019. For accounts created after this, the default is @true@ .
blockPublicAccessConfiguration ::
  -- | 'bpacBlockPublicSecurityGroupRules'
  Bool ->
  BlockPublicAccessConfiguration
blockPublicAccessConfiguration pBlockPublicSecurityGroupRules_ =
  BlockPublicAccessConfiguration'
    { _bpacPermittedPublicSecurityGroupRuleRanges =
        Nothing,
      _bpacBlockPublicSecurityGroupRules =
        pBlockPublicSecurityGroupRules_
    }

-- | Specifies ports and port ranges that are permitted to have security group rules that allow inbound traffic from all public sources. For example, if Port 23 (Telnet) is specified for @PermittedPublicSecurityGroupRuleRanges@ , Amazon EMR allows cluster creation if a security group associated with the cluster has a rule that allows inbound traffic on Port 23 from IPv4 0.0.0.0/0 or IPv6 port ::/0 as the source. By default, Port 22, which is used for SSH access to the cluster EC2 instances, is in the list of @PermittedPublicSecurityGroupRuleRanges@ .
bpacPermittedPublicSecurityGroupRuleRanges :: Lens' BlockPublicAccessConfiguration [PortRange]
bpacPermittedPublicSecurityGroupRuleRanges = lens _bpacPermittedPublicSecurityGroupRuleRanges (\s a -> s {_bpacPermittedPublicSecurityGroupRuleRanges = a}) . _Default . _Coerce

-- | Indicates whether Amazon EMR block public access is enabled (@true@ ) or disabled (@false@ ). By default, the value is @false@ for accounts that have created EMR clusters before July 2019. For accounts created after this, the default is @true@ .
bpacBlockPublicSecurityGroupRules :: Lens' BlockPublicAccessConfiguration Bool
bpacBlockPublicSecurityGroupRules = lens _bpacBlockPublicSecurityGroupRules (\s a -> s {_bpacBlockPublicSecurityGroupRules = a})

instance FromJSON BlockPublicAccessConfiguration where
  parseJSON =
    withObject
      "BlockPublicAccessConfiguration"
      ( \x ->
          BlockPublicAccessConfiguration'
            <$> (x .:? "PermittedPublicSecurityGroupRuleRanges" .!= mempty)
            <*> (x .: "BlockPublicSecurityGroupRules")
      )

instance Hashable BlockPublicAccessConfiguration

instance NFData BlockPublicAccessConfiguration

instance ToJSON BlockPublicAccessConfiguration where
  toJSON BlockPublicAccessConfiguration' {..} =
    object
      ( catMaybes
          [ ("PermittedPublicSecurityGroupRuleRanges" .=)
              <$> _bpacPermittedPublicSecurityGroupRuleRanges,
            Just
              ( "BlockPublicSecurityGroupRules"
                  .= _bpacBlockPublicSecurityGroupRules
              )
          ]
      )
