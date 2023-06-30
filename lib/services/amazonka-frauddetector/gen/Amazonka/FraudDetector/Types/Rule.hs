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
-- Module      : Amazonka.FraudDetector.Types.Rule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.Rule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A rule.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | The detector for which the rule is associated.
    detectorId :: Prelude.Text,
    -- | The rule ID.
    ruleId :: Prelude.Text,
    -- | The rule version.
    ruleVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'rule_detectorId' - The detector for which the rule is associated.
--
-- 'ruleId', 'rule_ruleId' - The rule ID.
--
-- 'ruleVersion', 'rule_ruleVersion' - The rule version.
newRule ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'ruleVersion'
  Prelude.Text ->
  Rule
newRule pDetectorId_ pRuleId_ pRuleVersion_ =
  Rule'
    { detectorId = pDetectorId_,
      ruleId = pRuleId_,
      ruleVersion = pRuleVersion_
    }

-- | The detector for which the rule is associated.
rule_detectorId :: Lens.Lens' Rule Prelude.Text
rule_detectorId = Lens.lens (\Rule' {detectorId} -> detectorId) (\s@Rule' {} a -> s {detectorId = a} :: Rule)

-- | The rule ID.
rule_ruleId :: Lens.Lens' Rule Prelude.Text
rule_ruleId = Lens.lens (\Rule' {ruleId} -> ruleId) (\s@Rule' {} a -> s {ruleId = a} :: Rule)

-- | The rule version.
rule_ruleVersion :: Lens.Lens' Rule Prelude.Text
rule_ruleVersion = Lens.lens (\Rule' {ruleVersion} -> ruleVersion) (\s@Rule' {} a -> s {ruleVersion = a} :: Rule)

instance Data.FromJSON Rule where
  parseJSON =
    Data.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Data..: "detectorId")
            Prelude.<*> (x Data..: "ruleId")
            Prelude.<*> (x Data..: "ruleVersion")
      )

instance Prelude.Hashable Rule where
  hashWithSalt _salt Rule' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` ruleVersion

instance Prelude.NFData Rule where
  rnf Rule' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf ruleVersion

instance Data.ToJSON Rule where
  toJSON Rule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("detectorId" Data..= detectorId),
            Prelude.Just ("ruleId" Data..= ruleId),
            Prelude.Just ("ruleVersion" Data..= ruleVersion)
          ]
      )
