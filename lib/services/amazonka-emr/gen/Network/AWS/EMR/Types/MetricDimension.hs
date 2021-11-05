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
-- Module      : Amazonka.EMR.Types.MetricDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.MetricDimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A CloudWatch dimension, which is specified using a @Key@ (known as a
-- @Name@ in CloudWatch), @Value@ pair. By default, Amazon EMR uses one
-- dimension whose @Key@ is @JobFlowID@ and @Value@ is a variable
-- representing the cluster ID, which is @${emr.clusterId}@. This enables
-- the rule to bootstrap when the cluster ID becomes available.
--
-- /See:/ 'newMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | The dimension value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The dimension name.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'metricDimension_value' - The dimension value.
--
-- 'key', 'metricDimension_key' - The dimension name.
newMetricDimension ::
  MetricDimension
newMetricDimension =
  MetricDimension'
    { value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The dimension value.
metricDimension_value :: Lens.Lens' MetricDimension (Prelude.Maybe Prelude.Text)
metricDimension_value = Lens.lens (\MetricDimension' {value} -> value) (\s@MetricDimension' {} a -> s {value = a} :: MetricDimension)

-- | The dimension name.
metricDimension_key :: Lens.Lens' MetricDimension (Prelude.Maybe Prelude.Text)
metricDimension_key = Lens.lens (\MetricDimension' {key} -> key) (\s@MetricDimension' {} a -> s {key = a} :: MetricDimension)

instance Core.FromJSON MetricDimension where
  parseJSON =
    Core.withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Key")
      )

instance Prelude.Hashable MetricDimension

instance Prelude.NFData MetricDimension

instance Core.ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Key" Core..=) Prelude.<$> key
          ]
      )
