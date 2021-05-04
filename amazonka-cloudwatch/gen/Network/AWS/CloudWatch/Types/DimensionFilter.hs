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
-- Module      : Network.AWS.CloudWatch.Types.DimensionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.DimensionFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents filters for a dimension.
--
-- /See:/ 'newDimensionFilter' smart constructor.
data DimensionFilter = DimensionFilter'
  { -- | The value of the dimension to be matched.
    value :: Prelude.Maybe Prelude.Text,
    -- | The dimension name to be matched.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable DimensionFilter

instance Prelude.NFData DimensionFilter

instance Prelude.ToQuery DimensionFilter where
  toQuery DimensionFilter' {..} =
    Prelude.mconcat
      ["Value" Prelude.=: value, "Name" Prelude.=: name]
