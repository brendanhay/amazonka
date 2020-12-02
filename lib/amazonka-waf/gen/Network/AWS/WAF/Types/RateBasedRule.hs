{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RateBasedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RateBasedRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.Predicate
import Network.AWS.WAF.Types.RateKey

-- | A @RateBasedRule@ is identical to a regular 'Rule' , with one addition: a @RateBasedRule@ counts the number of requests that arrive from a specified IP address every five minutes. For example, based on recent requests that you've seen from an attacker, you might create a @RateBasedRule@ that includes the following conditions:
--
--
--     * The requests come from 192.0.2.44.
--
--     * They contain the value @BadBot@ in the @User-Agent@ header.
--
--
--
-- In the rule, you also define the rate limit as 1,000.
--
-- Requests that meet both of these conditions and exceed 1,000 requests every five minutes trigger the rule's action (block or count), which is defined in the web ACL.
--
--
-- /See:/ 'rateBasedRule' smart constructor.
data RateBasedRule = RateBasedRule'
  { _rbrMetricName ::
      !(Maybe Text),
    _rbrName :: !(Maybe Text),
    _rbrRuleId :: !Text,
    _rbrMatchPredicates :: ![Predicate],
    _rbrRateKey :: !RateKey,
    _rbrRateLimit :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RateBasedRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rbrMetricName' - A friendly name or description for the metrics for a @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
--
-- * 'rbrName' - A friendly name or description for a @RateBasedRule@ . You can't change the name of a @RateBasedRule@ after you create it.
--
-- * 'rbrRuleId' - A unique identifier for a @RateBasedRule@ . You use @RuleId@ to get more information about a @RateBasedRule@ (see 'GetRateBasedRule' ), update a @RateBasedRule@ (see 'UpdateRateBasedRule' ), insert a @RateBasedRule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RateBasedRule@ from AWS WAF (see 'DeleteRateBasedRule' ).
--
-- * 'rbrMatchPredicates' - The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @RateBasedRule@ .
--
-- * 'rbrRateKey' - The field that AWS WAF uses to determine if requests are likely arriving from single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests arriving from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
--
-- * 'rbrRateLimit' - The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
rateBasedRule ::
  -- | 'rbrRuleId'
  Text ->
  -- | 'rbrRateKey'
  RateKey ->
  -- | 'rbrRateLimit'
  Natural ->
  RateBasedRule
rateBasedRule pRuleId_ pRateKey_ pRateLimit_ =
  RateBasedRule'
    { _rbrMetricName = Nothing,
      _rbrName = Nothing,
      _rbrRuleId = pRuleId_,
      _rbrMatchPredicates = mempty,
      _rbrRateKey = pRateKey_,
      _rbrRateLimit = _Nat # pRateLimit_
    }

-- | A friendly name or description for the metrics for a @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
rbrMetricName :: Lens' RateBasedRule (Maybe Text)
rbrMetricName = lens _rbrMetricName (\s a -> s {_rbrMetricName = a})

-- | A friendly name or description for a @RateBasedRule@ . You can't change the name of a @RateBasedRule@ after you create it.
rbrName :: Lens' RateBasedRule (Maybe Text)
rbrName = lens _rbrName (\s a -> s {_rbrName = a})

-- | A unique identifier for a @RateBasedRule@ . You use @RuleId@ to get more information about a @RateBasedRule@ (see 'GetRateBasedRule' ), update a @RateBasedRule@ (see 'UpdateRateBasedRule' ), insert a @RateBasedRule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RateBasedRule@ from AWS WAF (see 'DeleteRateBasedRule' ).
rbrRuleId :: Lens' RateBasedRule Text
rbrRuleId = lens _rbrRuleId (\s a -> s {_rbrRuleId = a})

-- | The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @RateBasedRule@ .
rbrMatchPredicates :: Lens' RateBasedRule [Predicate]
rbrMatchPredicates = lens _rbrMatchPredicates (\s a -> s {_rbrMatchPredicates = a}) . _Coerce

-- | The field that AWS WAF uses to determine if requests are likely arriving from single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests arriving from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
rbrRateKey :: Lens' RateBasedRule RateKey
rbrRateKey = lens _rbrRateKey (\s a -> s {_rbrRateKey = a})

-- | The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
rbrRateLimit :: Lens' RateBasedRule Natural
rbrRateLimit = lens _rbrRateLimit (\s a -> s {_rbrRateLimit = a}) . _Nat

instance FromJSON RateBasedRule where
  parseJSON =
    withObject
      "RateBasedRule"
      ( \x ->
          RateBasedRule'
            <$> (x .:? "MetricName")
            <*> (x .:? "Name")
            <*> (x .: "RuleId")
            <*> (x .:? "MatchPredicates" .!= mempty)
            <*> (x .: "RateKey")
            <*> (x .: "RateLimit")
      )

instance Hashable RateBasedRule

instance NFData RateBasedRule
