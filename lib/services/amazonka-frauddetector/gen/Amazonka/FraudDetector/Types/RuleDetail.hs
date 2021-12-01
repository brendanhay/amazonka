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
-- Module      : Amazonka.FraudDetector.Types.RuleDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.RuleDetail where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.Language
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of the rule.
--
-- /See:/ 'newRuleDetail' smart constructor.
data RuleDetail = RuleDetail'
  { -- | The rule version.
    ruleVersion :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of the last time the rule was updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The rule ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the rule was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The rule ID.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The rule outcomes.
    outcomes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The detector for which the rule is associated.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The rule expression.
    expression :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The rule language.
    language :: Prelude.Maybe Language,
    -- | The rule description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleVersion', 'ruleDetail_ruleVersion' - The rule version.
--
-- 'lastUpdatedTime', 'ruleDetail_lastUpdatedTime' - Timestamp of the last time the rule was updated.
--
-- 'arn', 'ruleDetail_arn' - The rule ARN.
--
-- 'createdTime', 'ruleDetail_createdTime' - The timestamp of when the rule was created.
--
-- 'ruleId', 'ruleDetail_ruleId' - The rule ID.
--
-- 'outcomes', 'ruleDetail_outcomes' - The rule outcomes.
--
-- 'detectorId', 'ruleDetail_detectorId' - The detector for which the rule is associated.
--
-- 'expression', 'ruleDetail_expression' - The rule expression.
--
-- 'language', 'ruleDetail_language' - The rule language.
--
-- 'description', 'ruleDetail_description' - The rule description.
newRuleDetail ::
  RuleDetail
newRuleDetail =
  RuleDetail'
    { ruleVersion = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      outcomes = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      expression = Prelude.Nothing,
      language = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The rule version.
ruleDetail_ruleVersion :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_ruleVersion = Lens.lens (\RuleDetail' {ruleVersion} -> ruleVersion) (\s@RuleDetail' {} a -> s {ruleVersion = a} :: RuleDetail)

-- | Timestamp of the last time the rule was updated.
ruleDetail_lastUpdatedTime :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_lastUpdatedTime = Lens.lens (\RuleDetail' {lastUpdatedTime} -> lastUpdatedTime) (\s@RuleDetail' {} a -> s {lastUpdatedTime = a} :: RuleDetail)

-- | The rule ARN.
ruleDetail_arn :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_arn = Lens.lens (\RuleDetail' {arn} -> arn) (\s@RuleDetail' {} a -> s {arn = a} :: RuleDetail)

-- | The timestamp of when the rule was created.
ruleDetail_createdTime :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_createdTime = Lens.lens (\RuleDetail' {createdTime} -> createdTime) (\s@RuleDetail' {} a -> s {createdTime = a} :: RuleDetail)

-- | The rule ID.
ruleDetail_ruleId :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_ruleId = Lens.lens (\RuleDetail' {ruleId} -> ruleId) (\s@RuleDetail' {} a -> s {ruleId = a} :: RuleDetail)

-- | The rule outcomes.
ruleDetail_outcomes :: Lens.Lens' RuleDetail (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
ruleDetail_outcomes = Lens.lens (\RuleDetail' {outcomes} -> outcomes) (\s@RuleDetail' {} a -> s {outcomes = a} :: RuleDetail) Prelude.. Lens.mapping Lens.coerced

-- | The detector for which the rule is associated.
ruleDetail_detectorId :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_detectorId = Lens.lens (\RuleDetail' {detectorId} -> detectorId) (\s@RuleDetail' {} a -> s {detectorId = a} :: RuleDetail)

-- | The rule expression.
ruleDetail_expression :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_expression = Lens.lens (\RuleDetail' {expression} -> expression) (\s@RuleDetail' {} a -> s {expression = a} :: RuleDetail) Prelude.. Lens.mapping Core._Sensitive

-- | The rule language.
ruleDetail_language :: Lens.Lens' RuleDetail (Prelude.Maybe Language)
ruleDetail_language = Lens.lens (\RuleDetail' {language} -> language) (\s@RuleDetail' {} a -> s {language = a} :: RuleDetail)

-- | The rule description.
ruleDetail_description :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_description = Lens.lens (\RuleDetail' {description} -> description) (\s@RuleDetail' {} a -> s {description = a} :: RuleDetail)

instance Core.FromJSON RuleDetail where
  parseJSON =
    Core.withObject
      "RuleDetail"
      ( \x ->
          RuleDetail'
            Prelude.<$> (x Core..:? "ruleVersion")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "ruleId")
            Prelude.<*> (x Core..:? "outcomes")
            Prelude.<*> (x Core..:? "detectorId")
            Prelude.<*> (x Core..:? "expression")
            Prelude.<*> (x Core..:? "language")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable RuleDetail where
  hashWithSalt salt' RuleDetail' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` outcomes
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` ruleVersion

instance Prelude.NFData RuleDetail where
  rnf RuleDetail' {..} =
    Prelude.rnf ruleVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf outcomes
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
