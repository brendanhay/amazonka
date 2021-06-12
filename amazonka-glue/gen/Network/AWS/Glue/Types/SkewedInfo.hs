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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies skewed values in a table. Skewed values are those that occur
-- with very high frequency.
--
-- /See:/ 'newSkewedInfo' smart constructor.
data SkewedInfo = SkewedInfo'
  { -- | A list of names of columns that contain skewed values.
    skewedColumnNames :: Core.Maybe [Core.Text],
    -- | A list of values that appear so frequently as to be considered skewed.
    skewedColumnValues :: Core.Maybe [Core.Text],
    -- | A mapping of skewed values to the columns that contain them.
    skewedColumnValueLocationMaps :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { skewedColumnNames = Core.Nothing,
      skewedColumnValues = Core.Nothing,
      skewedColumnValueLocationMaps = Core.Nothing
    }

-- | A list of names of columns that contain skewed values.
skewedInfo_skewedColumnNames :: Lens.Lens' SkewedInfo (Core.Maybe [Core.Text])
skewedInfo_skewedColumnNames = Lens.lens (\SkewedInfo' {skewedColumnNames} -> skewedColumnNames) (\s@SkewedInfo' {} a -> s {skewedColumnNames = a} :: SkewedInfo) Core.. Lens.mapping Lens._Coerce

-- | A list of values that appear so frequently as to be considered skewed.
skewedInfo_skewedColumnValues :: Lens.Lens' SkewedInfo (Core.Maybe [Core.Text])
skewedInfo_skewedColumnValues = Lens.lens (\SkewedInfo' {skewedColumnValues} -> skewedColumnValues) (\s@SkewedInfo' {} a -> s {skewedColumnValues = a} :: SkewedInfo) Core.. Lens.mapping Lens._Coerce

-- | A mapping of skewed values to the columns that contain them.
skewedInfo_skewedColumnValueLocationMaps :: Lens.Lens' SkewedInfo (Core.Maybe (Core.HashMap Core.Text Core.Text))
skewedInfo_skewedColumnValueLocationMaps = Lens.lens (\SkewedInfo' {skewedColumnValueLocationMaps} -> skewedColumnValueLocationMaps) (\s@SkewedInfo' {} a -> s {skewedColumnValueLocationMaps = a} :: SkewedInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON SkewedInfo where
  parseJSON =
    Core.withObject
      "SkewedInfo"
      ( \x ->
          SkewedInfo'
            Core.<$> (x Core..:? "SkewedColumnNames" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "SkewedColumnValues"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "SkewedColumnValueLocationMaps"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable SkewedInfo

instance Core.NFData SkewedInfo

instance Core.ToJSON SkewedInfo where
  toJSON SkewedInfo' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SkewedColumnNames" Core..=)
              Core.<$> skewedColumnNames,
            ("SkewedColumnValues" Core..=)
              Core.<$> skewedColumnValues,
            ("SkewedColumnValueLocationMaps" Core..=)
              Core.<$> skewedColumnValueLocationMaps
          ]
      )
