{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.Rule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.Rule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAFRegional.Types.Predicate

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- A combination of ByteMatchSet, IPSet, and\/or SqlInjectionMatchSet
-- objects that identify the web requests that you want to allow, block, or
-- count. For example, you might create a @Rule@ that includes the
-- following predicates:
--
-- -   An @IPSet@ that causes AWS WAF to search for web requests that
--     originate from the IP address @192.0.2.44@
--
-- -   A @ByteMatchSet@ that causes AWS WAF to search for web requests for
--     which the value of the @User-Agent@ header is @BadBot@.
--
-- To match the settings in this @Rule@, a request must originate from
-- @192.0.2.44@ AND include a @User-Agent@ header for which the value is
-- @BadBot@.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | A friendly name or description for the metrics for this @Rule@. The name
    -- can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum
    -- length 128 and minimum length one. It can\'t contain whitespace or
    -- metric names reserved for AWS WAF, including \"All\" and
    -- \"Default_Action.\" You can\'t change @MetricName@ after you create the
    -- @Rule@.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The friendly name or description for the @Rule@. You can\'t change the
    -- name of a @Rule@ after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a @Rule@. You use @RuleId@ to get more
    -- information about a @Rule@ (see GetRule), update a @Rule@ (see
    -- UpdateRule), insert a @Rule@ into a @WebACL@ or delete a one from a
    -- @WebACL@ (see UpdateWebACL), or delete a @Rule@ from AWS WAF (see
    -- DeleteRule).
    --
    -- @RuleId@ is returned by CreateRule and by ListRules.
    ruleId :: Prelude.Text,
    -- | The @Predicates@ object contains one @Predicate@ element for each
    -- ByteMatchSet, IPSet, or SqlInjectionMatchSet object that you want to
    -- include in a @Rule@.
    predicates :: [Predicate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'rule_metricName' - A friendly name or description for the metrics for this @Rule@. The name
-- can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum
-- length 128 and minimum length one. It can\'t contain whitespace or
-- metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change @MetricName@ after you create the
-- @Rule@.
--
-- 'name', 'rule_name' - The friendly name or description for the @Rule@. You can\'t change the
-- name of a @Rule@ after you create it.
--
-- 'ruleId', 'rule_ruleId' - A unique identifier for a @Rule@. You use @RuleId@ to get more
-- information about a @Rule@ (see GetRule), update a @Rule@ (see
-- UpdateRule), insert a @Rule@ into a @WebACL@ or delete a one from a
-- @WebACL@ (see UpdateWebACL), or delete a @Rule@ from AWS WAF (see
-- DeleteRule).
--
-- @RuleId@ is returned by CreateRule and by ListRules.
--
-- 'predicates', 'rule_predicates' - The @Predicates@ object contains one @Predicate@ element for each
-- ByteMatchSet, IPSet, or SqlInjectionMatchSet object that you want to
-- include in a @Rule@.
newRule ::
  -- | 'ruleId'
  Prelude.Text ->
  Rule
newRule pRuleId_ =
  Rule'
    { metricName = Prelude.Nothing,
      name = Prelude.Nothing,
      ruleId = pRuleId_,
      predicates = Prelude.mempty
    }

-- | A friendly name or description for the metrics for this @Rule@. The name
-- can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum
-- length 128 and minimum length one. It can\'t contain whitespace or
-- metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change @MetricName@ after you create the
-- @Rule@.
rule_metricName :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_metricName = Lens.lens (\Rule' {metricName} -> metricName) (\s@Rule' {} a -> s {metricName = a} :: Rule)

-- | The friendly name or description for the @Rule@. You can\'t change the
-- name of a @Rule@ after you create it.
rule_name :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_name = Lens.lens (\Rule' {name} -> name) (\s@Rule' {} a -> s {name = a} :: Rule)

-- | A unique identifier for a @Rule@. You use @RuleId@ to get more
-- information about a @Rule@ (see GetRule), update a @Rule@ (see
-- UpdateRule), insert a @Rule@ into a @WebACL@ or delete a one from a
-- @WebACL@ (see UpdateWebACL), or delete a @Rule@ from AWS WAF (see
-- DeleteRule).
--
-- @RuleId@ is returned by CreateRule and by ListRules.
rule_ruleId :: Lens.Lens' Rule Prelude.Text
rule_ruleId = Lens.lens (\Rule' {ruleId} -> ruleId) (\s@Rule' {} a -> s {ruleId = a} :: Rule)

-- | The @Predicates@ object contains one @Predicate@ element for each
-- ByteMatchSet, IPSet, or SqlInjectionMatchSet object that you want to
-- include in a @Rule@.
rule_predicates :: Lens.Lens' Rule [Predicate]
rule_predicates = Lens.lens (\Rule' {predicates} -> predicates) (\s@Rule' {} a -> s {predicates = a} :: Rule) Prelude.. Prelude._Coerce

instance Prelude.FromJSON Rule where
  parseJSON =
    Prelude.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Prelude..:? "MetricName")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..: "RuleId")
            Prelude.<*> ( x Prelude..:? "Predicates"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Rule

instance Prelude.NFData Rule
