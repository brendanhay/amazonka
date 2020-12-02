{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SubscribedRuleGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SubscribedRuleGroupSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of the rule groups you are subscribed to.
--
--
--
-- /See:/ 'subscribedRuleGroupSummary' smart constructor.
data SubscribedRuleGroupSummary = SubscribedRuleGroupSummary'
  { _srgsRuleGroupId ::
      !Text,
    _srgsName :: !Text,
    _srgsMetricName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubscribedRuleGroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srgsRuleGroupId' - A unique identifier for a @RuleGroup@ .
--
-- * 'srgsName' - A friendly name or description of the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
--
-- * 'srgsMetricName' - A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
subscribedRuleGroupSummary ::
  -- | 'srgsRuleGroupId'
  Text ->
  -- | 'srgsName'
  Text ->
  -- | 'srgsMetricName'
  Text ->
  SubscribedRuleGroupSummary
subscribedRuleGroupSummary pRuleGroupId_ pName_ pMetricName_ =
  SubscribedRuleGroupSummary'
    { _srgsRuleGroupId = pRuleGroupId_,
      _srgsName = pName_,
      _srgsMetricName = pMetricName_
    }

-- | A unique identifier for a @RuleGroup@ .
srgsRuleGroupId :: Lens' SubscribedRuleGroupSummary Text
srgsRuleGroupId = lens _srgsRuleGroupId (\s a -> s {_srgsRuleGroupId = a})

-- | A friendly name or description of the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
srgsName :: Lens' SubscribedRuleGroupSummary Text
srgsName = lens _srgsName (\s a -> s {_srgsName = a})

-- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
srgsMetricName :: Lens' SubscribedRuleGroupSummary Text
srgsMetricName = lens _srgsMetricName (\s a -> s {_srgsMetricName = a})

instance FromJSON SubscribedRuleGroupSummary where
  parseJSON =
    withObject
      "SubscribedRuleGroupSummary"
      ( \x ->
          SubscribedRuleGroupSummary'
            <$> (x .: "RuleGroupId") <*> (x .: "Name") <*> (x .: "MetricName")
      )

instance Hashable SubscribedRuleGroupSummary

instance NFData SubscribedRuleGroupSummary
