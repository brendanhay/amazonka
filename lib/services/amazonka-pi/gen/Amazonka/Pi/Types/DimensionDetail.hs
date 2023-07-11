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
-- Module      : Amazonka.Pi.Types.DimensionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.DimensionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The information about a dimension.
--
-- /See:/ 'newDimensionDetail' smart constructor.
data DimensionDetail = DimensionDetail'
  { -- | The identifier of a dimension.
    identifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'dimensionDetail_identifier' - The identifier of a dimension.
newDimensionDetail ::
  DimensionDetail
newDimensionDetail =
  DimensionDetail' {identifier = Prelude.Nothing}

-- | The identifier of a dimension.
dimensionDetail_identifier :: Lens.Lens' DimensionDetail (Prelude.Maybe Prelude.Text)
dimensionDetail_identifier = Lens.lens (\DimensionDetail' {identifier} -> identifier) (\s@DimensionDetail' {} a -> s {identifier = a} :: DimensionDetail)

instance Data.FromJSON DimensionDetail where
  parseJSON =
    Data.withObject
      "DimensionDetail"
      ( \x ->
          DimensionDetail'
            Prelude.<$> (x Data..:? "Identifier")
      )

instance Prelude.Hashable DimensionDetail where
  hashWithSalt _salt DimensionDetail' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData DimensionDetail where
  rnf DimensionDetail' {..} = Prelude.rnf identifier
