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
-- Module      : Network.AWS.AutoScaling.Types.MetricDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricDimension where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the dimension of a metric.
--
-- /See:/ 'newMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | The name of the dimension.
    name :: Prelude.Text,
    -- | The value of the dimension.
    value :: Prelude.Text
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
-- 'name', 'metricDimension_name' - The name of the dimension.
--
-- 'value', 'metricDimension_value' - The value of the dimension.
newMetricDimension ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  MetricDimension
newMetricDimension pName_ pValue_ =
  MetricDimension' {name = pName_, value = pValue_}

-- | The name of the dimension.
metricDimension_name :: Lens.Lens' MetricDimension Prelude.Text
metricDimension_name = Lens.lens (\MetricDimension' {name} -> name) (\s@MetricDimension' {} a -> s {name = a} :: MetricDimension)

-- | The value of the dimension.
metricDimension_value :: Lens.Lens' MetricDimension Prelude.Text
metricDimension_value = Lens.lens (\MetricDimension' {value} -> value) (\s@MetricDimension' {} a -> s {value = a} :: MetricDimension)

instance Prelude.FromXML MetricDimension where
  parseXML x =
    MetricDimension'
      Prelude.<$> (x Prelude..@ "Name")
      Prelude.<*> (x Prelude..@ "Value")

instance Prelude.Hashable MetricDimension

instance Prelude.NFData MetricDimension

instance Prelude.ToQuery MetricDimension where
  toQuery MetricDimension' {..} =
    Prelude.mconcat
      ["Name" Prelude.=: name, "Value" Prelude.=: value]
