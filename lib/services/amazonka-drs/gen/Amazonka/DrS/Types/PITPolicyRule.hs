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
-- Module      : Amazonka.DrS.Types.PITPolicyRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.PITPolicyRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.PITPolicyRuleUnits
import qualified Amazonka.Prelude as Prelude

-- | A rule in the Point in Time (PIT) policy representing when to take
-- snapshots and how long to retain them for.
--
-- /See:/ 'newPITPolicyRule' smart constructor.
data PITPolicyRule = PITPolicyRule'
  { -- | Whether this rule is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the rule.
    ruleID :: Prelude.Maybe Prelude.Natural,
    -- | How often, in the chosen units, a snapshot should be taken.
    interval :: Prelude.Natural,
    -- | The duration to retain a snapshot for, in the chosen units.
    retentionDuration :: Prelude.Natural,
    -- | The units used to measure the interval and retentionDuration.
    units :: PITPolicyRuleUnits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PITPolicyRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'pITPolicyRule_enabled' - Whether this rule is enabled or not.
--
-- 'ruleID', 'pITPolicyRule_ruleID' - The ID of the rule.
--
-- 'interval', 'pITPolicyRule_interval' - How often, in the chosen units, a snapshot should be taken.
--
-- 'retentionDuration', 'pITPolicyRule_retentionDuration' - The duration to retain a snapshot for, in the chosen units.
--
-- 'units', 'pITPolicyRule_units' - The units used to measure the interval and retentionDuration.
newPITPolicyRule ::
  -- | 'interval'
  Prelude.Natural ->
  -- | 'retentionDuration'
  Prelude.Natural ->
  -- | 'units'
  PITPolicyRuleUnits ->
  PITPolicyRule
newPITPolicyRule
  pInterval_
  pRetentionDuration_
  pUnits_ =
    PITPolicyRule'
      { enabled = Prelude.Nothing,
        ruleID = Prelude.Nothing,
        interval = pInterval_,
        retentionDuration = pRetentionDuration_,
        units = pUnits_
      }

-- | Whether this rule is enabled or not.
pITPolicyRule_enabled :: Lens.Lens' PITPolicyRule (Prelude.Maybe Prelude.Bool)
pITPolicyRule_enabled = Lens.lens (\PITPolicyRule' {enabled} -> enabled) (\s@PITPolicyRule' {} a -> s {enabled = a} :: PITPolicyRule)

-- | The ID of the rule.
pITPolicyRule_ruleID :: Lens.Lens' PITPolicyRule (Prelude.Maybe Prelude.Natural)
pITPolicyRule_ruleID = Lens.lens (\PITPolicyRule' {ruleID} -> ruleID) (\s@PITPolicyRule' {} a -> s {ruleID = a} :: PITPolicyRule)

-- | How often, in the chosen units, a snapshot should be taken.
pITPolicyRule_interval :: Lens.Lens' PITPolicyRule Prelude.Natural
pITPolicyRule_interval = Lens.lens (\PITPolicyRule' {interval} -> interval) (\s@PITPolicyRule' {} a -> s {interval = a} :: PITPolicyRule)

-- | The duration to retain a snapshot for, in the chosen units.
pITPolicyRule_retentionDuration :: Lens.Lens' PITPolicyRule Prelude.Natural
pITPolicyRule_retentionDuration = Lens.lens (\PITPolicyRule' {retentionDuration} -> retentionDuration) (\s@PITPolicyRule' {} a -> s {retentionDuration = a} :: PITPolicyRule)

-- | The units used to measure the interval and retentionDuration.
pITPolicyRule_units :: Lens.Lens' PITPolicyRule PITPolicyRuleUnits
pITPolicyRule_units = Lens.lens (\PITPolicyRule' {units} -> units) (\s@PITPolicyRule' {} a -> s {units = a} :: PITPolicyRule)

instance Data.FromJSON PITPolicyRule where
  parseJSON =
    Data.withObject
      "PITPolicyRule"
      ( \x ->
          PITPolicyRule'
            Prelude.<$> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "ruleID")
            Prelude.<*> (x Data..: "interval")
            Prelude.<*> (x Data..: "retentionDuration")
            Prelude.<*> (x Data..: "units")
      )

instance Prelude.Hashable PITPolicyRule where
  hashWithSalt _salt PITPolicyRule' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` ruleID
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` retentionDuration
      `Prelude.hashWithSalt` units

instance Prelude.NFData PITPolicyRule where
  rnf PITPolicyRule' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf ruleID
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf retentionDuration
      `Prelude.seq` Prelude.rnf units

instance Data.ToJSON PITPolicyRule where
  toJSON PITPolicyRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("ruleID" Data..=) Prelude.<$> ruleID,
            Prelude.Just ("interval" Data..= interval),
            Prelude.Just
              ("retentionDuration" Data..= retentionDuration),
            Prelude.Just ("units" Data..= units)
          ]
      )
