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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.RuleDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.Language
import qualified Amazonka.Prelude as Prelude

-- | The details of the rule.
--
-- /See:/ 'newRuleDetail' smart constructor.
data RuleDetail = RuleDetail'
  { -- | The rule version.
    ruleVersion :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the rule was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The rule ID.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The rule ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of the last time the rule was updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The rule expression.
    expression :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The rule outcomes.
    outcomes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The detector for which the rule is associated.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The rule language.
    language :: Prelude.Maybe Language
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
-- 'createdTime', 'ruleDetail_createdTime' - The timestamp of when the rule was created.
--
-- 'ruleId', 'ruleDetail_ruleId' - The rule ID.
--
-- 'arn', 'ruleDetail_arn' - The rule ARN.
--
-- 'description', 'ruleDetail_description' - The rule description.
--
-- 'lastUpdatedTime', 'ruleDetail_lastUpdatedTime' - Timestamp of the last time the rule was updated.
--
-- 'expression', 'ruleDetail_expression' - The rule expression.
--
-- 'outcomes', 'ruleDetail_outcomes' - The rule outcomes.
--
-- 'detectorId', 'ruleDetail_detectorId' - The detector for which the rule is associated.
--
-- 'language', 'ruleDetail_language' - The rule language.
newRuleDetail ::
  RuleDetail
newRuleDetail =
  RuleDetail'
    { ruleVersion = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      expression = Prelude.Nothing,
      outcomes = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      language = Prelude.Nothing
    }

-- | The rule version.
ruleDetail_ruleVersion :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_ruleVersion = Lens.lens (\RuleDetail' {ruleVersion} -> ruleVersion) (\s@RuleDetail' {} a -> s {ruleVersion = a} :: RuleDetail)

-- | The timestamp of when the rule was created.
ruleDetail_createdTime :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_createdTime = Lens.lens (\RuleDetail' {createdTime} -> createdTime) (\s@RuleDetail' {} a -> s {createdTime = a} :: RuleDetail)

-- | The rule ID.
ruleDetail_ruleId :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_ruleId = Lens.lens (\RuleDetail' {ruleId} -> ruleId) (\s@RuleDetail' {} a -> s {ruleId = a} :: RuleDetail)

-- | The rule ARN.
ruleDetail_arn :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_arn = Lens.lens (\RuleDetail' {arn} -> arn) (\s@RuleDetail' {} a -> s {arn = a} :: RuleDetail)

-- | The rule description.
ruleDetail_description :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_description = Lens.lens (\RuleDetail' {description} -> description) (\s@RuleDetail' {} a -> s {description = a} :: RuleDetail)

-- | Timestamp of the last time the rule was updated.
ruleDetail_lastUpdatedTime :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_lastUpdatedTime = Lens.lens (\RuleDetail' {lastUpdatedTime} -> lastUpdatedTime) (\s@RuleDetail' {} a -> s {lastUpdatedTime = a} :: RuleDetail)

-- | The rule expression.
ruleDetail_expression :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_expression = Lens.lens (\RuleDetail' {expression} -> expression) (\s@RuleDetail' {} a -> s {expression = a} :: RuleDetail) Prelude.. Lens.mapping Data._Sensitive

-- | The rule outcomes.
ruleDetail_outcomes :: Lens.Lens' RuleDetail (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
ruleDetail_outcomes = Lens.lens (\RuleDetail' {outcomes} -> outcomes) (\s@RuleDetail' {} a -> s {outcomes = a} :: RuleDetail) Prelude.. Lens.mapping Lens.coerced

-- | The detector for which the rule is associated.
ruleDetail_detectorId :: Lens.Lens' RuleDetail (Prelude.Maybe Prelude.Text)
ruleDetail_detectorId = Lens.lens (\RuleDetail' {detectorId} -> detectorId) (\s@RuleDetail' {} a -> s {detectorId = a} :: RuleDetail)

-- | The rule language.
ruleDetail_language :: Lens.Lens' RuleDetail (Prelude.Maybe Language)
ruleDetail_language = Lens.lens (\RuleDetail' {language} -> language) (\s@RuleDetail' {} a -> s {language = a} :: RuleDetail)

instance Data.FromJSON RuleDetail where
  parseJSON =
    Data.withObject
      "RuleDetail"
      ( \x ->
          RuleDetail'
            Prelude.<$> (x Data..:? "ruleVersion")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "ruleId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "expression")
            Prelude.<*> (x Data..:? "outcomes")
            Prelude.<*> (x Data..:? "detectorId")
            Prelude.<*> (x Data..:? "language")
      )

instance Prelude.Hashable RuleDetail where
  hashWithSalt _salt RuleDetail' {..} =
    _salt `Prelude.hashWithSalt` ruleVersion
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` outcomes
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` language

instance Prelude.NFData RuleDetail where
  rnf RuleDetail' {..} =
    Prelude.rnf ruleVersion
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf outcomes
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf language
