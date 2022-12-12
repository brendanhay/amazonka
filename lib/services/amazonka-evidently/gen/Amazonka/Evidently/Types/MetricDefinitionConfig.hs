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
-- Module      : Amazonka.Evidently.Types.MetricDefinitionConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.MetricDefinitionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure defines a metric that you want to use to evaluate the
-- variations during a launch or experiment.
--
-- /See:/ 'newMetricDefinitionConfig' smart constructor.
data MetricDefinitionConfig = MetricDefinitionConfig'
  { -- | The EventBridge event pattern that defines how the metric is recorded.
    --
    -- For more information about EventBridge event patterns, see
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-event-patterns.html Amazon EventBridge event patterns>.
    eventPattern :: Prelude.Maybe Prelude.Text,
    -- | A label for the units that the metric is measuring.
    unitLabel :: Prelude.Maybe Prelude.Text,
    -- | The entity, such as a user or session, that does an action that causes a
    -- metric value to be recorded. An example is @userDetails.userID@.
    entityIdKey :: Prelude.Text,
    -- | A name for the metric.
    name :: Prelude.Text,
    -- | The value that is tracked to produce the metric.
    valueKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDefinitionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventPattern', 'metricDefinitionConfig_eventPattern' - The EventBridge event pattern that defines how the metric is recorded.
--
-- For more information about EventBridge event patterns, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-event-patterns.html Amazon EventBridge event patterns>.
--
-- 'unitLabel', 'metricDefinitionConfig_unitLabel' - A label for the units that the metric is measuring.
--
-- 'entityIdKey', 'metricDefinitionConfig_entityIdKey' - The entity, such as a user or session, that does an action that causes a
-- metric value to be recorded. An example is @userDetails.userID@.
--
-- 'name', 'metricDefinitionConfig_name' - A name for the metric.
--
-- 'valueKey', 'metricDefinitionConfig_valueKey' - The value that is tracked to produce the metric.
newMetricDefinitionConfig ::
  -- | 'entityIdKey'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'valueKey'
  Prelude.Text ->
  MetricDefinitionConfig
newMetricDefinitionConfig
  pEntityIdKey_
  pName_
  pValueKey_ =
    MetricDefinitionConfig'
      { eventPattern =
          Prelude.Nothing,
        unitLabel = Prelude.Nothing,
        entityIdKey = pEntityIdKey_,
        name = pName_,
        valueKey = pValueKey_
      }

-- | The EventBridge event pattern that defines how the metric is recorded.
--
-- For more information about EventBridge event patterns, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-event-patterns.html Amazon EventBridge event patterns>.
metricDefinitionConfig_eventPattern :: Lens.Lens' MetricDefinitionConfig (Prelude.Maybe Prelude.Text)
metricDefinitionConfig_eventPattern = Lens.lens (\MetricDefinitionConfig' {eventPattern} -> eventPattern) (\s@MetricDefinitionConfig' {} a -> s {eventPattern = a} :: MetricDefinitionConfig)

-- | A label for the units that the metric is measuring.
metricDefinitionConfig_unitLabel :: Lens.Lens' MetricDefinitionConfig (Prelude.Maybe Prelude.Text)
metricDefinitionConfig_unitLabel = Lens.lens (\MetricDefinitionConfig' {unitLabel} -> unitLabel) (\s@MetricDefinitionConfig' {} a -> s {unitLabel = a} :: MetricDefinitionConfig)

-- | The entity, such as a user or session, that does an action that causes a
-- metric value to be recorded. An example is @userDetails.userID@.
metricDefinitionConfig_entityIdKey :: Lens.Lens' MetricDefinitionConfig Prelude.Text
metricDefinitionConfig_entityIdKey = Lens.lens (\MetricDefinitionConfig' {entityIdKey} -> entityIdKey) (\s@MetricDefinitionConfig' {} a -> s {entityIdKey = a} :: MetricDefinitionConfig)

-- | A name for the metric.
metricDefinitionConfig_name :: Lens.Lens' MetricDefinitionConfig Prelude.Text
metricDefinitionConfig_name = Lens.lens (\MetricDefinitionConfig' {name} -> name) (\s@MetricDefinitionConfig' {} a -> s {name = a} :: MetricDefinitionConfig)

-- | The value that is tracked to produce the metric.
metricDefinitionConfig_valueKey :: Lens.Lens' MetricDefinitionConfig Prelude.Text
metricDefinitionConfig_valueKey = Lens.lens (\MetricDefinitionConfig' {valueKey} -> valueKey) (\s@MetricDefinitionConfig' {} a -> s {valueKey = a} :: MetricDefinitionConfig)

instance Prelude.Hashable MetricDefinitionConfig where
  hashWithSalt _salt MetricDefinitionConfig' {..} =
    _salt `Prelude.hashWithSalt` eventPattern
      `Prelude.hashWithSalt` unitLabel
      `Prelude.hashWithSalt` entityIdKey
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` valueKey

instance Prelude.NFData MetricDefinitionConfig where
  rnf MetricDefinitionConfig' {..} =
    Prelude.rnf eventPattern
      `Prelude.seq` Prelude.rnf unitLabel
      `Prelude.seq` Prelude.rnf entityIdKey
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf valueKey

instance Data.ToJSON MetricDefinitionConfig where
  toJSON MetricDefinitionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("eventPattern" Data..=) Prelude.<$> eventPattern,
            ("unitLabel" Data..=) Prelude.<$> unitLabel,
            Prelude.Just ("entityIdKey" Data..= entityIdKey),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("valueKey" Data..= valueKey)
          ]
      )
