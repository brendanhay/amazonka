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
-- Module      : Amazonka.ApplicationAutoScaling.Types.TargetTrackingMetricDimension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.TargetTrackingMetricDimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the dimension of a metric.
--
-- /See:/ 'newTargetTrackingMetricDimension' smart constructor.
data TargetTrackingMetricDimension = TargetTrackingMetricDimension'
  { -- | The name of the dimension.
    name :: Prelude.Text,
    -- | The value of the dimension.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetTrackingMetricDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'targetTrackingMetricDimension_name' - The name of the dimension.
--
-- 'value', 'targetTrackingMetricDimension_value' - The value of the dimension.
newTargetTrackingMetricDimension ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  TargetTrackingMetricDimension
newTargetTrackingMetricDimension pName_ pValue_ =
  TargetTrackingMetricDimension'
    { name = pName_,
      value = pValue_
    }

-- | The name of the dimension.
targetTrackingMetricDimension_name :: Lens.Lens' TargetTrackingMetricDimension Prelude.Text
targetTrackingMetricDimension_name = Lens.lens (\TargetTrackingMetricDimension' {name} -> name) (\s@TargetTrackingMetricDimension' {} a -> s {name = a} :: TargetTrackingMetricDimension)

-- | The value of the dimension.
targetTrackingMetricDimension_value :: Lens.Lens' TargetTrackingMetricDimension Prelude.Text
targetTrackingMetricDimension_value = Lens.lens (\TargetTrackingMetricDimension' {value} -> value) (\s@TargetTrackingMetricDimension' {} a -> s {value = a} :: TargetTrackingMetricDimension)

instance Data.FromJSON TargetTrackingMetricDimension where
  parseJSON =
    Data.withObject
      "TargetTrackingMetricDimension"
      ( \x ->
          TargetTrackingMetricDimension'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Value")
      )

instance
  Prelude.Hashable
    TargetTrackingMetricDimension
  where
  hashWithSalt _salt TargetTrackingMetricDimension' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData TargetTrackingMetricDimension where
  rnf TargetTrackingMetricDimension' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON TargetTrackingMetricDimension where
  toJSON TargetTrackingMetricDimension' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
