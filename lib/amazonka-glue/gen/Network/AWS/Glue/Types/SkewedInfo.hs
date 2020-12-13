{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SkewedInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SkewedInfo
  ( SkewedInfo (..),

    -- * Smart constructor
    mkSkewedInfo,

    -- * Lenses
    siSkewedColumnValueLocationMaps,
    siSkewedColumnValues,
    siSkewedColumnNames,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies skewed values in a table. Skewed values are those that occur with very high frequency.
--
-- /See:/ 'mkSkewedInfo' smart constructor.
data SkewedInfo = SkewedInfo'
  { -- | A mapping of skewed values to the columns that contain them.
    skewedColumnValueLocationMaps :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A list of values that appear so frequently as to be considered skewed.
    skewedColumnValues :: Lude.Maybe [Lude.Text],
    -- | A list of names of columns that contain skewed values.
    skewedColumnNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SkewedInfo' with the minimum fields required to make a request.
--
-- * 'skewedColumnValueLocationMaps' - A mapping of skewed values to the columns that contain them.
-- * 'skewedColumnValues' - A list of values that appear so frequently as to be considered skewed.
-- * 'skewedColumnNames' - A list of names of columns that contain skewed values.
mkSkewedInfo ::
  SkewedInfo
mkSkewedInfo =
  SkewedInfo'
    { skewedColumnValueLocationMaps = Lude.Nothing,
      skewedColumnValues = Lude.Nothing,
      skewedColumnNames = Lude.Nothing
    }

-- | A mapping of skewed values to the columns that contain them.
--
-- /Note:/ Consider using 'skewedColumnValueLocationMaps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSkewedColumnValueLocationMaps :: Lens.Lens' SkewedInfo (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
siSkewedColumnValueLocationMaps = Lens.lens (skewedColumnValueLocationMaps :: SkewedInfo -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {skewedColumnValueLocationMaps = a} :: SkewedInfo)
{-# DEPRECATED siSkewedColumnValueLocationMaps "Use generic-lens or generic-optics with 'skewedColumnValueLocationMaps' instead." #-}

-- | A list of values that appear so frequently as to be considered skewed.
--
-- /Note:/ Consider using 'skewedColumnValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSkewedColumnValues :: Lens.Lens' SkewedInfo (Lude.Maybe [Lude.Text])
siSkewedColumnValues = Lens.lens (skewedColumnValues :: SkewedInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {skewedColumnValues = a} :: SkewedInfo)
{-# DEPRECATED siSkewedColumnValues "Use generic-lens or generic-optics with 'skewedColumnValues' instead." #-}

-- | A list of names of columns that contain skewed values.
--
-- /Note:/ Consider using 'skewedColumnNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSkewedColumnNames :: Lens.Lens' SkewedInfo (Lude.Maybe [Lude.Text])
siSkewedColumnNames = Lens.lens (skewedColumnNames :: SkewedInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {skewedColumnNames = a} :: SkewedInfo)
{-# DEPRECATED siSkewedColumnNames "Use generic-lens or generic-optics with 'skewedColumnNames' instead." #-}

instance Lude.FromJSON SkewedInfo where
  parseJSON =
    Lude.withObject
      "SkewedInfo"
      ( \x ->
          SkewedInfo'
            Lude.<$> (x Lude..:? "SkewedColumnValueLocationMaps" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SkewedColumnValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SkewedColumnNames" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SkewedInfo where
  toJSON SkewedInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SkewedColumnValueLocationMaps" Lude..=)
              Lude.<$> skewedColumnValueLocationMaps,
            ("SkewedColumnValues" Lude..=) Lude.<$> skewedColumnValues,
            ("SkewedColumnNames" Lude..=) Lude.<$> skewedColumnNames
          ]
      )
