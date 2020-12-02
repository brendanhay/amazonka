{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RuleGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RuleGroupSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the identifier and the friendly name or description of the @RuleGroup@ .
--
--
--
-- /See:/ 'ruleGroupSummary' smart constructor.
data RuleGroupSummary = RuleGroupSummary'
  { _rgsRuleGroupId :: !Text,
    _rgsName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RuleGroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsRuleGroupId' - A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ). @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- * 'rgsName' - A friendly name or description of the 'RuleGroup' . You can't change the name of a @RuleGroup@ after you create it.
ruleGroupSummary ::
  -- | 'rgsRuleGroupId'
  Text ->
  -- | 'rgsName'
  Text ->
  RuleGroupSummary
ruleGroupSummary pRuleGroupId_ pName_ =
  RuleGroupSummary'
    { _rgsRuleGroupId = pRuleGroupId_,
      _rgsName = pName_
    }

-- | A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ). @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
rgsRuleGroupId :: Lens' RuleGroupSummary Text
rgsRuleGroupId = lens _rgsRuleGroupId (\s a -> s {_rgsRuleGroupId = a})

-- | A friendly name or description of the 'RuleGroup' . You can't change the name of a @RuleGroup@ after you create it.
rgsName :: Lens' RuleGroupSummary Text
rgsName = lens _rgsName (\s a -> s {_rgsName = a})

instance FromJSON RuleGroupSummary where
  parseJSON =
    withObject
      "RuleGroupSummary"
      ( \x ->
          RuleGroupSummary' <$> (x .: "RuleGroupId") <*> (x .: "Name")
      )

instance Hashable RuleGroupSummary

instance NFData RuleGroupSummary
