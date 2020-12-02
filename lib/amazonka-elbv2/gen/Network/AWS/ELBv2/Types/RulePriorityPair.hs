{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RulePriorityPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.RulePriorityPair where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the priorities for the rules for a listener.
--
--
--
-- /See:/ 'rulePriorityPair' smart constructor.
data RulePriorityPair = RulePriorityPair'
  { _rppPriority ::
      !(Maybe Nat),
    _rppRuleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RulePriorityPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rppPriority' - The rule priority.
--
-- * 'rppRuleARN' - The Amazon Resource Name (ARN) of the rule.
rulePriorityPair ::
  RulePriorityPair
rulePriorityPair =
  RulePriorityPair' {_rppPriority = Nothing, _rppRuleARN = Nothing}

-- | The rule priority.
rppPriority :: Lens' RulePriorityPair (Maybe Natural)
rppPriority = lens _rppPriority (\s a -> s {_rppPriority = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the rule.
rppRuleARN :: Lens' RulePriorityPair (Maybe Text)
rppRuleARN = lens _rppRuleARN (\s a -> s {_rppRuleARN = a})

instance Hashable RulePriorityPair

instance NFData RulePriorityPair

instance ToQuery RulePriorityPair where
  toQuery RulePriorityPair' {..} =
    mconcat ["Priority" =: _rppPriority, "RuleArn" =: _rppRuleARN]
