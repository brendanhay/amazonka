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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CapacitySize where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CapacitySizeType

-- | Currently, the @CapacitySize@ API is not supported.
--
-- /See:/ 'newCapacitySize' smart constructor.
data CapacitySize = CapacitySize'
  { -- | This API is not supported.
    type' :: CapacitySizeType,
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
-- 'type'', 'capacitySize_type' - This API is not supported.
--
-- 'value', 'capacitySize_value' -
newCapacitySize ::
  -- | 'type''
  CapacitySizeType ->
  -- | 'value'
  Prelude.Natural ->
  CapacitySize
newCapacitySize pType_ pValue_ =
  CapacitySize' {type' = pType_, value = pValue_}

-- | This API is not supported.
capacitySize_type :: Lens.Lens' CapacitySize CapacitySizeType
capacitySize_type = Lens.lens (\CapacitySize' {type'} -> type') (\s@CapacitySize' {} a -> s {type' = a} :: CapacitySize)

-- |
capacitySize_value :: Lens.Lens' CapacitySize Prelude.Natural
capacitySize_value = Lens.lens (\CapacitySize' {value} -> value) (\s@CapacitySize' {} a -> s {value = a} :: CapacitySize)

instance Core.FromJSON CapacitySize where
  parseJSON =
    Core.withObject
      "CapacitySize"
      ( \x ->
          CapacitySize'
            Prelude.<$> (x Core..: "Type") Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable CapacitySize where
  hashWithSalt salt' CapacitySize' {..} =
    salt' `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CapacitySize where
  rnf CapacitySize' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Core.ToJSON CapacitySize where
  toJSON CapacitySize' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("Value" Core..= value)
          ]
      )
