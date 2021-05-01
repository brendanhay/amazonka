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
-- Module      : Network.AWS.IoT.Types.MetricDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricDimension where

import Network.AWS.IoT.Types.DimensionValueOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The dimension of a metric.
--
-- /See:/ 'newMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | Defines how the @dimensionValues@ of a dimension are interpreted. For
    -- example, for dimension type TOPIC_FILTER, the @IN@ operator, a message
    -- will be counted only if its topic matches one of the topic filters. With
    -- @NOT_IN@ operator, a message will be counted only if it doesn\'t match
    -- any of the topic filters. The operator is optional: if it\'s not
    -- provided (is @null@), it will be interpreted as @IN@.
    operator :: Prelude.Maybe DimensionValueOperator,
    -- | A unique identifier for the dimension.
    dimensionName :: Prelude.Text
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
-- 'operator', 'metricDimension_operator' - Defines how the @dimensionValues@ of a dimension are interpreted. For
-- example, for dimension type TOPIC_FILTER, the @IN@ operator, a message
-- will be counted only if its topic matches one of the topic filters. With
-- @NOT_IN@ operator, a message will be counted only if it doesn\'t match
-- any of the topic filters. The operator is optional: if it\'s not
-- provided (is @null@), it will be interpreted as @IN@.
--
-- 'dimensionName', 'metricDimension_dimensionName' - A unique identifier for the dimension.
newMetricDimension ::
  -- | 'dimensionName'
  Prelude.Text ->
  MetricDimension
newMetricDimension pDimensionName_ =
  MetricDimension'
    { operator = Prelude.Nothing,
      dimensionName = pDimensionName_
    }

-- | Defines how the @dimensionValues@ of a dimension are interpreted. For
-- example, for dimension type TOPIC_FILTER, the @IN@ operator, a message
-- will be counted only if its topic matches one of the topic filters. With
-- @NOT_IN@ operator, a message will be counted only if it doesn\'t match
-- any of the topic filters. The operator is optional: if it\'s not
-- provided (is @null@), it will be interpreted as @IN@.
metricDimension_operator :: Lens.Lens' MetricDimension (Prelude.Maybe DimensionValueOperator)
metricDimension_operator = Lens.lens (\MetricDimension' {operator} -> operator) (\s@MetricDimension' {} a -> s {operator = a} :: MetricDimension)

-- | A unique identifier for the dimension.
metricDimension_dimensionName :: Lens.Lens' MetricDimension Prelude.Text
metricDimension_dimensionName = Lens.lens (\MetricDimension' {dimensionName} -> dimensionName) (\s@MetricDimension' {} a -> s {dimensionName = a} :: MetricDimension)

instance Prelude.FromJSON MetricDimension where
  parseJSON =
    Prelude.withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            Prelude.<$> (x Prelude..:? "operator")
            Prelude.<*> (x Prelude..: "dimensionName")
      )

instance Prelude.Hashable MetricDimension

instance Prelude.NFData MetricDimension

instance Prelude.ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("operator" Prelude..=) Prelude.<$> operator,
            Prelude.Just
              ("dimensionName" Prelude..= dimensionName)
          ]
      )
