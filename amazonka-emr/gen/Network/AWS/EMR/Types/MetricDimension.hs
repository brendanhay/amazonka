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
-- Module      : Network.AWS.EMR.Types.MetricDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.MetricDimension where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A CloudWatch dimension, which is specified using a @Key@ (known as a
-- @Name@ in CloudWatch), @Value@ pair. By default, Amazon EMR uses one
-- dimension whose @Key@ is @JobFlowID@ and @Value@ is a variable
-- representing the cluster ID, which is @${emr.clusterId}@. This enables
-- the rule to bootstrap when the cluster ID becomes available.
--
-- /See:/ 'newMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | The dimension name.
    key :: Prelude.Maybe Prelude.Text,
    -- | The dimension value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MetricDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'metricDimension_key' - The dimension name.
--
-- 'value', 'metricDimension_value' - The dimension value.
newMetricDimension ::
  MetricDimension
newMetricDimension =
  MetricDimension'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The dimension name.
metricDimension_key :: Lens.Lens' MetricDimension (Prelude.Maybe Prelude.Text)
metricDimension_key = Lens.lens (\MetricDimension' {key} -> key) (\s@MetricDimension' {} a -> s {key = a} :: MetricDimension)

-- | The dimension value.
metricDimension_value :: Lens.Lens' MetricDimension (Prelude.Maybe Prelude.Text)
metricDimension_value = Lens.lens (\MetricDimension' {value} -> value) (\s@MetricDimension' {} a -> s {value = a} :: MetricDimension)

instance Prelude.FromJSON MetricDimension where
  parseJSON =
    Prelude.withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable MetricDimension

instance Prelude.NFData MetricDimension

instance Prelude.ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
