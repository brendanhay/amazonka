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
-- Module      : Amazonka.Inspector2.Types.NumberFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.NumberFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that describes the details of a number filter.
--
-- /See:/ 'newNumberFilter' smart constructor.
data NumberFilter = NumberFilter'
  { -- | The lowest number to be included in the filter.
    lowerInclusive :: Prelude.Maybe Prelude.Double,
    -- | The highest number to be included in the filter.
    upperInclusive :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumberFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerInclusive', 'numberFilter_lowerInclusive' - The lowest number to be included in the filter.
--
-- 'upperInclusive', 'numberFilter_upperInclusive' - The highest number to be included in the filter.
newNumberFilter ::
  NumberFilter
newNumberFilter =
  NumberFilter'
    { lowerInclusive = Prelude.Nothing,
      upperInclusive = Prelude.Nothing
    }

-- | The lowest number to be included in the filter.
numberFilter_lowerInclusive :: Lens.Lens' NumberFilter (Prelude.Maybe Prelude.Double)
numberFilter_lowerInclusive = Lens.lens (\NumberFilter' {lowerInclusive} -> lowerInclusive) (\s@NumberFilter' {} a -> s {lowerInclusive = a} :: NumberFilter)

-- | The highest number to be included in the filter.
numberFilter_upperInclusive :: Lens.Lens' NumberFilter (Prelude.Maybe Prelude.Double)
numberFilter_upperInclusive = Lens.lens (\NumberFilter' {upperInclusive} -> upperInclusive) (\s@NumberFilter' {} a -> s {upperInclusive = a} :: NumberFilter)

instance Data.FromJSON NumberFilter where
  parseJSON =
    Data.withObject
      "NumberFilter"
      ( \x ->
          NumberFilter'
            Prelude.<$> (x Data..:? "lowerInclusive")
            Prelude.<*> (x Data..:? "upperInclusive")
      )

instance Prelude.Hashable NumberFilter where
  hashWithSalt _salt NumberFilter' {..} =
    _salt
      `Prelude.hashWithSalt` lowerInclusive
      `Prelude.hashWithSalt` upperInclusive

instance Prelude.NFData NumberFilter where
  rnf NumberFilter' {..} =
    Prelude.rnf lowerInclusive
      `Prelude.seq` Prelude.rnf upperInclusive

instance Data.ToJSON NumberFilter where
  toJSON NumberFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("lowerInclusive" Data..=)
              Prelude.<$> lowerInclusive,
            ("upperInclusive" Data..=)
              Prelude.<$> upperInclusive
          ]
      )
