{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Rule where

import Network.AWS.ELBv2.Types.Action
import Network.AWS.ELBv2.Types.RuleCondition
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a rule.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rPriority :: !(Maybe Text),
    _rActions :: !(Maybe [Action]),
    _rConditions :: !(Maybe [RuleCondition]),
    _rRuleARN :: !(Maybe Text),
    _rIsDefault :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rPriority' - The priority.
--
-- * 'rActions' - The actions. Each rule must include exactly one of the following types of actions: @forward@ , @redirect@ , or @fixed-response@ , and it must be the last action to be performed.
--
-- * 'rConditions' - The conditions. Each rule can include zero or one of the following conditions: @http-request-method@ , @host-header@ , @path-pattern@ , and @source-ip@ , and zero or more of the following conditions: @http-header@ and @query-string@ .
--
-- * 'rRuleARN' - The Amazon Resource Name (ARN) of the rule.
--
-- * 'rIsDefault' - Indicates whether this is the default rule.
rule ::
  Rule
rule =
  Rule'
    { _rPriority = Nothing,
      _rActions = Nothing,
      _rConditions = Nothing,
      _rRuleARN = Nothing,
      _rIsDefault = Nothing
    }

-- | The priority.
rPriority :: Lens' Rule (Maybe Text)
rPriority = lens _rPriority (\s a -> s {_rPriority = a})

-- | The actions. Each rule must include exactly one of the following types of actions: @forward@ , @redirect@ , or @fixed-response@ , and it must be the last action to be performed.
rActions :: Lens' Rule [Action]
rActions = lens _rActions (\s a -> s {_rActions = a}) . _Default . _Coerce

-- | The conditions. Each rule can include zero or one of the following conditions: @http-request-method@ , @host-header@ , @path-pattern@ , and @source-ip@ , and zero or more of the following conditions: @http-header@ and @query-string@ .
rConditions :: Lens' Rule [RuleCondition]
rConditions = lens _rConditions (\s a -> s {_rConditions = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the rule.
rRuleARN :: Lens' Rule (Maybe Text)
rRuleARN = lens _rRuleARN (\s a -> s {_rRuleARN = a})

-- | Indicates whether this is the default rule.
rIsDefault :: Lens' Rule (Maybe Bool)
rIsDefault = lens _rIsDefault (\s a -> s {_rIsDefault = a})

instance FromXML Rule where
  parseXML x =
    Rule'
      <$> (x .@? "Priority")
      <*> (x .@? "Actions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Conditions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "RuleArn")
      <*> (x .@? "IsDefault")

instance Hashable Rule

instance NFData Rule
