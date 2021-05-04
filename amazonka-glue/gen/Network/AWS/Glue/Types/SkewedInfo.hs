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
-- Module      : Network.AWS.Glue.Types.SkewedInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SkewedInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies skewed values in a table. Skewed values are those that occur
-- with very high frequency.
--
-- /See:/ 'newSkewedInfo' smart constructor.
data SkewedInfo = SkewedInfo'
  { -- | A list of names of columns that contain skewed values.
    skewedColumnNames :: Prelude.Maybe [Prelude.Text],
    -- | A list of values that appear so frequently as to be considered skewed.
    skewedColumnValues :: Prelude.Maybe [Prelude.Text],
    -- | A mapping of skewed values to the columns that contain them.
    skewedColumnValueLocationMaps :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'skewedColumnValues', 'skewedInfo_skewedColumnValues' - A list of values that appear so frequently as to be considered skewed.
--
-- 'skewedColumnValueLocationMaps', 'skewedInfo_skewedColumnValueLocationMaps' - A mapping of skewed values to the columns that contain them.
newSkewedInfo ::
  SkewedInfo
newSkewedInfo =
  SkewedInfo'
    { skewedColumnNames = Prelude.Nothing,
      skewedColumnValues = Prelude.Nothing,
      skewedColumnValueLocationMaps = Prelude.Nothing
    }

-- | A list of names of columns that contain skewed values.
skewedInfo_skewedColumnNames :: Lens.Lens' SkewedInfo (Prelude.Maybe [Prelude.Text])
skewedInfo_skewedColumnNames = Lens.lens (\SkewedInfo' {skewedColumnNames} -> skewedColumnNames) (\s@SkewedInfo' {} a -> s {skewedColumnNames = a} :: SkewedInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of values that appear so frequently as to be considered skewed.
skewedInfo_skewedColumnValues :: Lens.Lens' SkewedInfo (Prelude.Maybe [Prelude.Text])
skewedInfo_skewedColumnValues = Lens.lens (\SkewedInfo' {skewedColumnValues} -> skewedColumnValues) (\s@SkewedInfo' {} a -> s {skewedColumnValues = a} :: SkewedInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | A mapping of skewed values to the columns that contain them.
skewedInfo_skewedColumnValueLocationMaps :: Lens.Lens' SkewedInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
skewedInfo_skewedColumnValueLocationMaps = Lens.lens (\SkewedInfo' {skewedColumnValueLocationMaps} -> skewedColumnValueLocationMaps) (\s@SkewedInfo' {} a -> s {skewedColumnValueLocationMaps = a} :: SkewedInfo) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON SkewedInfo where
  parseJSON =
    Prelude.withObject
      "SkewedInfo"
      ( \x ->
          SkewedInfo'
            Prelude.<$> ( x Prelude..:? "SkewedColumnNames"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "SkewedColumnValues"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "SkewedColumnValueLocationMaps"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SkewedInfo

instance Prelude.NFData SkewedInfo

instance Prelude.ToJSON SkewedInfo where
  toJSON SkewedInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SkewedColumnNames" Prelude..=)
              Prelude.<$> skewedColumnNames,
            ("SkewedColumnValues" Prelude..=)
              Prelude.<$> skewedColumnValues,
            ("SkewedColumnValueLocationMaps" Prelude..=)
              Prelude.<$> skewedColumnValueLocationMaps
          ]
      )
