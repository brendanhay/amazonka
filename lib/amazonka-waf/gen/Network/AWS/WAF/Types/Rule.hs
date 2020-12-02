{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.Rule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.Predicate

-- | A combination of 'ByteMatchSet' , 'IPSet' , and/or 'SqlInjectionMatchSet' objects that identify the web requests that you want to allow, block, or count. For example, you might create a @Rule@ that includes the following predicates:
--
--
--     * An @IPSet@ that causes AWS WAF to search for web requests that originate from the IP address @192.0.2.44@
--
--     * A @ByteMatchSet@ that causes AWS WAF to search for web requests for which the value of the @User-Agent@ header is @BadBot@ .
--
--
--
-- To match the settings in this @Rule@ , a request must originate from @192.0.2.44@ AND include a @User-Agent@ header for which the value is @BadBot@ .
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rMetricName :: !(Maybe Text),
    _rName :: !(Maybe Text),
    _rRuleId :: !Text,
    _rPredicates :: ![Predicate]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rMetricName' - A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @Rule@ .
--
-- * 'rName' - The friendly name or description for the @Rule@ . You can't change the name of a @Rule@ after you create it.
--
-- * 'rRuleId' - A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- * 'rPredicates' - The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @Rule@ .
rule ::
  -- | 'rRuleId'
  Text ->
  Rule
rule pRuleId_ =
  Rule'
    { _rMetricName = Nothing,
      _rName = Nothing,
      _rRuleId = pRuleId_,
      _rPredicates = mempty
    }

-- | A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @Rule@ .
rMetricName :: Lens' Rule (Maybe Text)
rMetricName = lens _rMetricName (\s a -> s {_rMetricName = a})

-- | The friendly name or description for the @Rule@ . You can't change the name of a @Rule@ after you create it.
rName :: Lens' Rule (Maybe Text)
rName = lens _rName (\s a -> s {_rName = a})

-- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
rRuleId :: Lens' Rule Text
rRuleId = lens _rRuleId (\s a -> s {_rRuleId = a})

-- | The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @Rule@ .
rPredicates :: Lens' Rule [Predicate]
rPredicates = lens _rPredicates (\s a -> s {_rPredicates = a}) . _Coerce

instance FromJSON Rule where
  parseJSON =
    withObject
      "Rule"
      ( \x ->
          Rule'
            <$> (x .:? "MetricName")
            <*> (x .:? "Name")
            <*> (x .: "RuleId")
            <*> (x .:? "Predicates" .!= mempty)
      )

instance Hashable Rule

instance NFData Rule
