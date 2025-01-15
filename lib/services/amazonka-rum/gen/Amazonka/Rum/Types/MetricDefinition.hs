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
-- Module      : Amazonka.Rum.Types.MetricDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.MetricDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that displays the definition of one extended metric that RUM
-- sends to CloudWatch or CloudWatch Evidently. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-vended-metrics.html Additional metrics that you can send to CloudWatch and CloudWatch Evidently>.
--
-- /See:/ 'newMetricDefinition' smart constructor.
data MetricDefinition = MetricDefinition'
  { -- | This field is a map of field paths to dimension names. It defines the
    -- dimensions to associate with this metric in CloudWatch The value of this
    -- field is used only if the metric destination is @CloudWatch@. If the
    -- metric destination is @Evidently@, the value of @DimensionKeys@ is
    -- ignored.
    dimensionKeys :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The pattern that defines the metric. RUM checks events that happen in a
    -- user\'s session against the pattern, and events that match the pattern
    -- are sent to the metric destination.
    --
    -- If the metrics destination is @CloudWatch@ and the event also matches a
    -- value in @DimensionKeys@, then the metric is published with the
    -- specified dimensions.
    eventPattern :: Prelude.Maybe Prelude.Text,
    -- | Use this field only if you are sending this metric to CloudWatch. It
    -- defines the CloudWatch metric unit that this metric is measured in.
    unitLabel :: Prelude.Maybe Prelude.Text,
    -- | The field within the event object that the metric value is sourced from.
    valueKey :: Prelude.Maybe Prelude.Text,
    -- | The ID of this metric definition.
    metricDefinitionId :: Prelude.Text,
    -- | The name of the metric that is defined in this structure.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionKeys', 'metricDefinition_dimensionKeys' - This field is a map of field paths to dimension names. It defines the
-- dimensions to associate with this metric in CloudWatch The value of this
-- field is used only if the metric destination is @CloudWatch@. If the
-- metric destination is @Evidently@, the value of @DimensionKeys@ is
-- ignored.
--
-- 'eventPattern', 'metricDefinition_eventPattern' - The pattern that defines the metric. RUM checks events that happen in a
-- user\'s session against the pattern, and events that match the pattern
-- are sent to the metric destination.
--
-- If the metrics destination is @CloudWatch@ and the event also matches a
-- value in @DimensionKeys@, then the metric is published with the
-- specified dimensions.
--
-- 'unitLabel', 'metricDefinition_unitLabel' - Use this field only if you are sending this metric to CloudWatch. It
-- defines the CloudWatch metric unit that this metric is measured in.
--
-- 'valueKey', 'metricDefinition_valueKey' - The field within the event object that the metric value is sourced from.
--
-- 'metricDefinitionId', 'metricDefinition_metricDefinitionId' - The ID of this metric definition.
--
-- 'name', 'metricDefinition_name' - The name of the metric that is defined in this structure.
newMetricDefinition ::
  -- | 'metricDefinitionId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  MetricDefinition
newMetricDefinition pMetricDefinitionId_ pName_ =
  MetricDefinition'
    { dimensionKeys = Prelude.Nothing,
      eventPattern = Prelude.Nothing,
      unitLabel = Prelude.Nothing,
      valueKey = Prelude.Nothing,
      metricDefinitionId = pMetricDefinitionId_,
      name = pName_
    }

-- | This field is a map of field paths to dimension names. It defines the
-- dimensions to associate with this metric in CloudWatch The value of this
-- field is used only if the metric destination is @CloudWatch@. If the
-- metric destination is @Evidently@, the value of @DimensionKeys@ is
-- ignored.
metricDefinition_dimensionKeys :: Lens.Lens' MetricDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
metricDefinition_dimensionKeys = Lens.lens (\MetricDefinition' {dimensionKeys} -> dimensionKeys) (\s@MetricDefinition' {} a -> s {dimensionKeys = a} :: MetricDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The pattern that defines the metric. RUM checks events that happen in a
-- user\'s session against the pattern, and events that match the pattern
-- are sent to the metric destination.
--
-- If the metrics destination is @CloudWatch@ and the event also matches a
-- value in @DimensionKeys@, then the metric is published with the
-- specified dimensions.
metricDefinition_eventPattern :: Lens.Lens' MetricDefinition (Prelude.Maybe Prelude.Text)
metricDefinition_eventPattern = Lens.lens (\MetricDefinition' {eventPattern} -> eventPattern) (\s@MetricDefinition' {} a -> s {eventPattern = a} :: MetricDefinition)

-- | Use this field only if you are sending this metric to CloudWatch. It
-- defines the CloudWatch metric unit that this metric is measured in.
metricDefinition_unitLabel :: Lens.Lens' MetricDefinition (Prelude.Maybe Prelude.Text)
metricDefinition_unitLabel = Lens.lens (\MetricDefinition' {unitLabel} -> unitLabel) (\s@MetricDefinition' {} a -> s {unitLabel = a} :: MetricDefinition)

-- | The field within the event object that the metric value is sourced from.
metricDefinition_valueKey :: Lens.Lens' MetricDefinition (Prelude.Maybe Prelude.Text)
metricDefinition_valueKey = Lens.lens (\MetricDefinition' {valueKey} -> valueKey) (\s@MetricDefinition' {} a -> s {valueKey = a} :: MetricDefinition)

-- | The ID of this metric definition.
metricDefinition_metricDefinitionId :: Lens.Lens' MetricDefinition Prelude.Text
metricDefinition_metricDefinitionId = Lens.lens (\MetricDefinition' {metricDefinitionId} -> metricDefinitionId) (\s@MetricDefinition' {} a -> s {metricDefinitionId = a} :: MetricDefinition)

-- | The name of the metric that is defined in this structure.
metricDefinition_name :: Lens.Lens' MetricDefinition Prelude.Text
metricDefinition_name = Lens.lens (\MetricDefinition' {name} -> name) (\s@MetricDefinition' {} a -> s {name = a} :: MetricDefinition)

instance Data.FromJSON MetricDefinition where
  parseJSON =
    Data.withObject
      "MetricDefinition"
      ( \x ->
          MetricDefinition'
            Prelude.<$> (x Data..:? "DimensionKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EventPattern")
            Prelude.<*> (x Data..:? "UnitLabel")
            Prelude.<*> (x Data..:? "ValueKey")
            Prelude.<*> (x Data..: "MetricDefinitionId")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable MetricDefinition where
  hashWithSalt _salt MetricDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` dimensionKeys
      `Prelude.hashWithSalt` eventPattern
      `Prelude.hashWithSalt` unitLabel
      `Prelude.hashWithSalt` valueKey
      `Prelude.hashWithSalt` metricDefinitionId
      `Prelude.hashWithSalt` name

instance Prelude.NFData MetricDefinition where
  rnf MetricDefinition' {..} =
    Prelude.rnf dimensionKeys `Prelude.seq`
      Prelude.rnf eventPattern `Prelude.seq`
        Prelude.rnf unitLabel `Prelude.seq`
          Prelude.rnf valueKey `Prelude.seq`
            Prelude.rnf metricDefinitionId `Prelude.seq`
              Prelude.rnf name
