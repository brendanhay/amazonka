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
-- Module      : Amazonka.SageMaker.Types.CapacitySize
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CapacitySize where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CapacitySizeType

-- | Specifies the endpoint capacity to activate for production.
--
-- /See:/ 'newCapacitySize' smart constructor.
data CapacitySize = CapacitySize'
  { -- | Specifies the endpoint capacity type.
    --
    -- -   @INSTANCE_COUNT@: The endpoint activates based on the number of
    --     instances.
    --
    -- -   @CAPACITY_PERCENT@: The endpoint activates based on the specified
    --     percentage of capacity.
    type' :: CapacitySizeType,
    -- | Defines the capacity size, either as a number of instances or a capacity
    -- percentage.
    value :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacitySize' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'capacitySize_type' - Specifies the endpoint capacity type.
--
-- -   @INSTANCE_COUNT@: The endpoint activates based on the number of
--     instances.
--
-- -   @CAPACITY_PERCENT@: The endpoint activates based on the specified
--     percentage of capacity.
--
-- 'value', 'capacitySize_value' - Defines the capacity size, either as a number of instances or a capacity
-- percentage.
newCapacitySize ::
  -- | 'type''
  CapacitySizeType ->
  -- | 'value'
  Prelude.Natural ->
  CapacitySize
newCapacitySize pType_ pValue_ =
  CapacitySize' {type' = pType_, value = pValue_}

-- | Specifies the endpoint capacity type.
--
-- -   @INSTANCE_COUNT@: The endpoint activates based on the number of
--     instances.
--
-- -   @CAPACITY_PERCENT@: The endpoint activates based on the specified
--     percentage of capacity.
capacitySize_type :: Lens.Lens' CapacitySize CapacitySizeType
capacitySize_type = Lens.lens (\CapacitySize' {type'} -> type') (\s@CapacitySize' {} a -> s {type' = a} :: CapacitySize)

-- | Defines the capacity size, either as a number of instances or a capacity
-- percentage.
capacitySize_value :: Lens.Lens' CapacitySize Prelude.Natural
capacitySize_value = Lens.lens (\CapacitySize' {value} -> value) (\s@CapacitySize' {} a -> s {value = a} :: CapacitySize)

instance Data.FromJSON CapacitySize where
  parseJSON =
    Data.withObject
      "CapacitySize"
      ( \x ->
          CapacitySize'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable CapacitySize where
  hashWithSalt _salt CapacitySize' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData CapacitySize where
  rnf CapacitySize' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Data.ToJSON CapacitySize where
  toJSON CapacitySize' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Value" Data..= value)
          ]
      )
