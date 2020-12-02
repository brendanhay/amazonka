{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RuleGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of predefined rules that you can add to a web ACL.
--
--
-- Rule groups are subject to the following limits:
--
--     * Three rule groups per account. You can request an increase to this limit by contacting customer support.
--
--     * One rule group per web ACL.
--
--     * Ten rules per rule group.
--
--
--
--
-- /See:/ 'ruleGroup' smart constructor.
data RuleGroup = RuleGroup'
  { _rgMetricName :: !(Maybe Text),
    _rgName :: !(Maybe Text),
    _rgRuleGroupId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RuleGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgMetricName' - A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
--
-- * 'rgName' - The friendly name or description for the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
--
-- * 'rgRuleGroupId' - A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ). @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
ruleGroup ::
  -- | 'rgRuleGroupId'
  Text ->
  RuleGroup
ruleGroup pRuleGroupId_ =
  RuleGroup'
    { _rgMetricName = Nothing,
      _rgName = Nothing,
      _rgRuleGroupId = pRuleGroupId_
    }

-- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
rgMetricName :: Lens' RuleGroup (Maybe Text)
rgMetricName = lens _rgMetricName (\s a -> s {_rgMetricName = a})

-- | The friendly name or description for the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
rgName :: Lens' RuleGroup (Maybe Text)
rgName = lens _rgName (\s a -> s {_rgName = a})

-- | A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ). @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
rgRuleGroupId :: Lens' RuleGroup Text
rgRuleGroupId = lens _rgRuleGroupId (\s a -> s {_rgRuleGroupId = a})

instance FromJSON RuleGroup where
  parseJSON =
    withObject
      "RuleGroup"
      ( \x ->
          RuleGroup'
            <$> (x .:? "MetricName") <*> (x .:? "Name") <*> (x .: "RuleGroupId")
      )

instance Hashable RuleGroup

instance NFData RuleGroup
