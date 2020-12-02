{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.StatelessRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.StatelessRuleGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | AWS Network Firewall stateless rule group, used in a 'NetworkFirewallPolicyDescription' .
--
--
--
-- /See:/ 'statelessRuleGroup' smart constructor.
data StatelessRuleGroup = StatelessRuleGroup'
  { _sResourceId ::
      !(Maybe Text),
    _sPriority :: !(Maybe Nat),
    _sRuleGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StatelessRuleGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sResourceId' - The resource ID of the rule group.
--
-- * 'sPriority' - The priority of the rule group. AWS Network Firewall evaluates the stateless rule groups in a firewall policy starting from the lowest priority setting.
--
-- * 'sRuleGroupName' - The name of the rule group.
statelessRuleGroup ::
  StatelessRuleGroup
statelessRuleGroup =
  StatelessRuleGroup'
    { _sResourceId = Nothing,
      _sPriority = Nothing,
      _sRuleGroupName = Nothing
    }

-- | The resource ID of the rule group.
sResourceId :: Lens' StatelessRuleGroup (Maybe Text)
sResourceId = lens _sResourceId (\s a -> s {_sResourceId = a})

-- | The priority of the rule group. AWS Network Firewall evaluates the stateless rule groups in a firewall policy starting from the lowest priority setting.
sPriority :: Lens' StatelessRuleGroup (Maybe Natural)
sPriority = lens _sPriority (\s a -> s {_sPriority = a}) . mapping _Nat

-- | The name of the rule group.
sRuleGroupName :: Lens' StatelessRuleGroup (Maybe Text)
sRuleGroupName = lens _sRuleGroupName (\s a -> s {_sRuleGroupName = a})

instance FromJSON StatelessRuleGroup where
  parseJSON =
    withObject
      "StatelessRuleGroup"
      ( \x ->
          StatelessRuleGroup'
            <$> (x .:? "ResourceId")
            <*> (x .:? "Priority")
            <*> (x .:? "RuleGroupName")
      )

instance Hashable StatelessRuleGroup

instance NFData StatelessRuleGroup
