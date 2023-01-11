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
-- Module      : Amazonka.Glue.Types.SkewedInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SkewedInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies skewed values in a table. Skewed values are those that occur
-- with very high frequency.
--
-- /See:/ 'newSkewedInfo' smart constructor.
data SkewedInfo = SkewedInfo'
  { -- | A list of names of columns that contain skewed values.
    skewedColumnNames :: Prelude.Maybe [Prelude.Text],
    -- | A mapping of skewed values to the columns that contain them.
    skewedColumnValueLocationMaps :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of values that appear so frequently as to be considered skewed.
    skewedColumnValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SkewedInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skewedColumnNames', 'skewedInfo_skewedColumnNames' - A list of names of columns that contain skewed values.
--
-- 'skewedColumnValueLocationMaps', 'skewedInfo_skewedColumnValueLocationMaps' - A mapping of skewed values to the columns that contain them.
--
-- 'skewedColumnValues', 'skewedInfo_skewedColumnValues' - A list of values that appear so frequently as to be considered skewed.
newSkewedInfo ::
  SkewedInfo
newSkewedInfo =
  SkewedInfo'
    { skewedColumnNames = Prelude.Nothing,
      skewedColumnValueLocationMaps = Prelude.Nothing,
      skewedColumnValues = Prelude.Nothing
    }

-- | A list of names of columns that contain skewed values.
skewedInfo_skewedColumnNames :: Lens.Lens' SkewedInfo (Prelude.Maybe [Prelude.Text])
skewedInfo_skewedColumnNames = Lens.lens (\SkewedInfo' {skewedColumnNames} -> skewedColumnNames) (\s@SkewedInfo' {} a -> s {skewedColumnNames = a} :: SkewedInfo) Prelude.. Lens.mapping Lens.coerced

-- | A mapping of skewed values to the columns that contain them.
skewedInfo_skewedColumnValueLocationMaps :: Lens.Lens' SkewedInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
skewedInfo_skewedColumnValueLocationMaps = Lens.lens (\SkewedInfo' {skewedColumnValueLocationMaps} -> skewedColumnValueLocationMaps) (\s@SkewedInfo' {} a -> s {skewedColumnValueLocationMaps = a} :: SkewedInfo) Prelude.. Lens.mapping Lens.coerced

-- | A list of values that appear so frequently as to be considered skewed.
skewedInfo_skewedColumnValues :: Lens.Lens' SkewedInfo (Prelude.Maybe [Prelude.Text])
skewedInfo_skewedColumnValues = Lens.lens (\SkewedInfo' {skewedColumnValues} -> skewedColumnValues) (\s@SkewedInfo' {} a -> s {skewedColumnValues = a} :: SkewedInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SkewedInfo where
  parseJSON =
    Data.withObject
      "SkewedInfo"
      ( \x ->
          SkewedInfo'
            Prelude.<$> ( x Data..:? "SkewedColumnNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "SkewedColumnValueLocationMaps"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "SkewedColumnValues"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SkewedInfo where
  hashWithSalt _salt SkewedInfo' {..} =
    _salt `Prelude.hashWithSalt` skewedColumnNames
      `Prelude.hashWithSalt` skewedColumnValueLocationMaps
      `Prelude.hashWithSalt` skewedColumnValues

instance Prelude.NFData SkewedInfo where
  rnf SkewedInfo' {..} =
    Prelude.rnf skewedColumnNames
      `Prelude.seq` Prelude.rnf skewedColumnValueLocationMaps
      `Prelude.seq` Prelude.rnf skewedColumnValues

instance Data.ToJSON SkewedInfo where
  toJSON SkewedInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SkewedColumnNames" Data..=)
              Prelude.<$> skewedColumnNames,
            ("SkewedColumnValueLocationMaps" Data..=)
              Prelude.<$> skewedColumnValueLocationMaps,
            ("SkewedColumnValues" Data..=)
              Prelude.<$> skewedColumnValues
          ]
      )
