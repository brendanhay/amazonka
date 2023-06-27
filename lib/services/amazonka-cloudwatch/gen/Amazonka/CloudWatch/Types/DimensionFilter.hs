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
-- Module      : Amazonka.CloudWatch.Types.DimensionFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.DimensionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents filters for a dimension.
--
-- /See:/ 'newDimensionFilter' smart constructor.
data DimensionFilter = DimensionFilter'
  { -- | The value of the dimension to be matched.
    value :: Prelude.Maybe Prelude.Text,
    -- | The dimension name to be matched.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'dimensionFilter_value' - The value of the dimension to be matched.
--
-- 'name', 'dimensionFilter_name' - The dimension name to be matched.
newDimensionFilter ::
  -- | 'name'
  Prelude.Text ->
  DimensionFilter
newDimensionFilter pName_ =
  DimensionFilter'
    { value = Prelude.Nothing,
      name = pName_
    }

-- | The value of the dimension to be matched.
dimensionFilter_value :: Lens.Lens' DimensionFilter (Prelude.Maybe Prelude.Text)
dimensionFilter_value = Lens.lens (\DimensionFilter' {value} -> value) (\s@DimensionFilter' {} a -> s {value = a} :: DimensionFilter)

-- | The dimension name to be matched.
dimensionFilter_name :: Lens.Lens' DimensionFilter Prelude.Text
dimensionFilter_name = Lens.lens (\DimensionFilter' {name} -> name) (\s@DimensionFilter' {} a -> s {name = a} :: DimensionFilter)

instance Prelude.Hashable DimensionFilter where
  hashWithSalt _salt DimensionFilter' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name

instance Prelude.NFData DimensionFilter where
  rnf DimensionFilter' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Data.ToQuery DimensionFilter where
  toQuery DimensionFilter' {..} =
    Prelude.mconcat
      ["Value" Data.=: value, "Name" Data.=: name]
