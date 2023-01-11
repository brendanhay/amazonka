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
-- Module      : Amazonka.LookoutMetrics.Types.DimensionNameValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DimensionNameValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A dimension name and value.
--
-- /See:/ 'newDimensionNameValue' smart constructor.
data DimensionNameValue = DimensionNameValue'
  { -- | The name of the dimension.
    dimensionName :: Prelude.Text,
    -- | The value of the dimension.
    dimensionValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionNameValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionName', 'dimensionNameValue_dimensionName' - The name of the dimension.
--
-- 'dimensionValue', 'dimensionNameValue_dimensionValue' - The value of the dimension.
newDimensionNameValue ::
  -- | 'dimensionName'
  Prelude.Text ->
  -- | 'dimensionValue'
  Prelude.Text ->
  DimensionNameValue
newDimensionNameValue
  pDimensionName_
  pDimensionValue_ =
    DimensionNameValue'
      { dimensionName =
          pDimensionName_,
        dimensionValue = pDimensionValue_
      }

-- | The name of the dimension.
dimensionNameValue_dimensionName :: Lens.Lens' DimensionNameValue Prelude.Text
dimensionNameValue_dimensionName = Lens.lens (\DimensionNameValue' {dimensionName} -> dimensionName) (\s@DimensionNameValue' {} a -> s {dimensionName = a} :: DimensionNameValue)

-- | The value of the dimension.
dimensionNameValue_dimensionValue :: Lens.Lens' DimensionNameValue Prelude.Text
dimensionNameValue_dimensionValue = Lens.lens (\DimensionNameValue' {dimensionValue} -> dimensionValue) (\s@DimensionNameValue' {} a -> s {dimensionValue = a} :: DimensionNameValue)

instance Data.FromJSON DimensionNameValue where
  parseJSON =
    Data.withObject
      "DimensionNameValue"
      ( \x ->
          DimensionNameValue'
            Prelude.<$> (x Data..: "DimensionName")
            Prelude.<*> (x Data..: "DimensionValue")
      )

instance Prelude.Hashable DimensionNameValue where
  hashWithSalt _salt DimensionNameValue' {..} =
    _salt `Prelude.hashWithSalt` dimensionName
      `Prelude.hashWithSalt` dimensionValue

instance Prelude.NFData DimensionNameValue where
  rnf DimensionNameValue' {..} =
    Prelude.rnf dimensionName
      `Prelude.seq` Prelude.rnf dimensionValue
