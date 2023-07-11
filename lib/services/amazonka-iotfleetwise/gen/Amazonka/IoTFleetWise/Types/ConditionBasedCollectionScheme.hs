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
-- Module      : Amazonka.IoTFleetWise.Types.ConditionBasedCollectionScheme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.ConditionBasedCollectionScheme where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.TriggerMode
import qualified Amazonka.Prelude as Prelude

-- | Information about a collection scheme that uses a simple logical
-- expression to recognize what data to collect.
--
-- /See:/ 'newConditionBasedCollectionScheme' smart constructor.
data ConditionBasedCollectionScheme = ConditionBasedCollectionScheme'
  { -- | Specifies the version of the conditional expression language.
    conditionLanguageVersion :: Prelude.Maybe Prelude.Natural,
    -- | The minimum duration of time between two triggering events to collect
    -- data, in milliseconds.
    --
    -- If a signal changes often, you might want to collect data at a slower
    -- rate.
    minimumTriggerIntervalMs :: Prelude.Maybe Prelude.Natural,
    -- | Whether to collect data for all triggering events (@ALWAYS@). Specify
    -- (@RISING_EDGE@), or specify only when the condition first evaluates to
    -- false. For example, triggering on \"AirbagDeployed\"; Users aren\'t
    -- interested on triggering when the airbag is already exploded; they only
    -- care about the change from not deployed => deployed.
    triggerMode :: Prelude.Maybe TriggerMode,
    -- | The logical expression used to recognize what data to collect. For
    -- example, @$variable.Vehicle.OutsideAirTemperature >= 105.0@.
    expression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionBasedCollectionScheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionLanguageVersion', 'conditionBasedCollectionScheme_conditionLanguageVersion' - Specifies the version of the conditional expression language.
--
-- 'minimumTriggerIntervalMs', 'conditionBasedCollectionScheme_minimumTriggerIntervalMs' - The minimum duration of time between two triggering events to collect
-- data, in milliseconds.
--
-- If a signal changes often, you might want to collect data at a slower
-- rate.
--
-- 'triggerMode', 'conditionBasedCollectionScheme_triggerMode' - Whether to collect data for all triggering events (@ALWAYS@). Specify
-- (@RISING_EDGE@), or specify only when the condition first evaluates to
-- false. For example, triggering on \"AirbagDeployed\"; Users aren\'t
-- interested on triggering when the airbag is already exploded; they only
-- care about the change from not deployed => deployed.
--
-- 'expression', 'conditionBasedCollectionScheme_expression' - The logical expression used to recognize what data to collect. For
-- example, @$variable.Vehicle.OutsideAirTemperature >= 105.0@.
newConditionBasedCollectionScheme ::
  -- | 'expression'
  Prelude.Text ->
  ConditionBasedCollectionScheme
newConditionBasedCollectionScheme pExpression_ =
  ConditionBasedCollectionScheme'
    { conditionLanguageVersion =
        Prelude.Nothing,
      minimumTriggerIntervalMs = Prelude.Nothing,
      triggerMode = Prelude.Nothing,
      expression = pExpression_
    }

-- | Specifies the version of the conditional expression language.
conditionBasedCollectionScheme_conditionLanguageVersion :: Lens.Lens' ConditionBasedCollectionScheme (Prelude.Maybe Prelude.Natural)
conditionBasedCollectionScheme_conditionLanguageVersion = Lens.lens (\ConditionBasedCollectionScheme' {conditionLanguageVersion} -> conditionLanguageVersion) (\s@ConditionBasedCollectionScheme' {} a -> s {conditionLanguageVersion = a} :: ConditionBasedCollectionScheme)

-- | The minimum duration of time between two triggering events to collect
-- data, in milliseconds.
--
-- If a signal changes often, you might want to collect data at a slower
-- rate.
conditionBasedCollectionScheme_minimumTriggerIntervalMs :: Lens.Lens' ConditionBasedCollectionScheme (Prelude.Maybe Prelude.Natural)
conditionBasedCollectionScheme_minimumTriggerIntervalMs = Lens.lens (\ConditionBasedCollectionScheme' {minimumTriggerIntervalMs} -> minimumTriggerIntervalMs) (\s@ConditionBasedCollectionScheme' {} a -> s {minimumTriggerIntervalMs = a} :: ConditionBasedCollectionScheme)

-- | Whether to collect data for all triggering events (@ALWAYS@). Specify
-- (@RISING_EDGE@), or specify only when the condition first evaluates to
-- false. For example, triggering on \"AirbagDeployed\"; Users aren\'t
-- interested on triggering when the airbag is already exploded; they only
-- care about the change from not deployed => deployed.
conditionBasedCollectionScheme_triggerMode :: Lens.Lens' ConditionBasedCollectionScheme (Prelude.Maybe TriggerMode)
conditionBasedCollectionScheme_triggerMode = Lens.lens (\ConditionBasedCollectionScheme' {triggerMode} -> triggerMode) (\s@ConditionBasedCollectionScheme' {} a -> s {triggerMode = a} :: ConditionBasedCollectionScheme)

-- | The logical expression used to recognize what data to collect. For
-- example, @$variable.Vehicle.OutsideAirTemperature >= 105.0@.
conditionBasedCollectionScheme_expression :: Lens.Lens' ConditionBasedCollectionScheme Prelude.Text
conditionBasedCollectionScheme_expression = Lens.lens (\ConditionBasedCollectionScheme' {expression} -> expression) (\s@ConditionBasedCollectionScheme' {} a -> s {expression = a} :: ConditionBasedCollectionScheme)

instance Data.FromJSON ConditionBasedCollectionScheme where
  parseJSON =
    Data.withObject
      "ConditionBasedCollectionScheme"
      ( \x ->
          ConditionBasedCollectionScheme'
            Prelude.<$> (x Data..:? "conditionLanguageVersion")
            Prelude.<*> (x Data..:? "minimumTriggerIntervalMs")
            Prelude.<*> (x Data..:? "triggerMode")
            Prelude.<*> (x Data..: "expression")
      )

instance
  Prelude.Hashable
    ConditionBasedCollectionScheme
  where
  hashWithSalt
    _salt
    ConditionBasedCollectionScheme' {..} =
      _salt
        `Prelude.hashWithSalt` conditionLanguageVersion
        `Prelude.hashWithSalt` minimumTriggerIntervalMs
        `Prelude.hashWithSalt` triggerMode
        `Prelude.hashWithSalt` expression

instance
  Prelude.NFData
    ConditionBasedCollectionScheme
  where
  rnf ConditionBasedCollectionScheme' {..} =
    Prelude.rnf conditionLanguageVersion
      `Prelude.seq` Prelude.rnf minimumTriggerIntervalMs
      `Prelude.seq` Prelude.rnf triggerMode
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON ConditionBasedCollectionScheme where
  toJSON ConditionBasedCollectionScheme' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("conditionLanguageVersion" Data..=)
              Prelude.<$> conditionLanguageVersion,
            ("minimumTriggerIntervalMs" Data..=)
              Prelude.<$> minimumTriggerIntervalMs,
            ("triggerMode" Data..=) Prelude.<$> triggerMode,
            Prelude.Just ("expression" Data..= expression)
          ]
      )
