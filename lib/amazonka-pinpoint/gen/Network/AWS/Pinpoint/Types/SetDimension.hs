{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SetDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SetDimension
  ( SetDimension (..),

    -- * Smart constructor
    mkSetDimension,

    -- * Lenses
    sdValues,
    sdDimensionType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DimensionType
import qualified Network.AWS.Prelude as Lude

-- | Specifies the dimension type and values for a segment dimension.
--
-- /See:/ 'mkSetDimension' smart constructor.
data SetDimension = SetDimension'
  { -- | The criteria values to use for the segment dimension. Depending on the value of the DimensionType property, endpoints are included or excluded from the segment if their values match the criteria values.
    values :: [Lude.Text],
    -- | The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
    dimensionType :: Lude.Maybe DimensionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDimension' with the minimum fields required to make a request.
--
-- * 'values' - The criteria values to use for the segment dimension. Depending on the value of the DimensionType property, endpoints are included or excluded from the segment if their values match the criteria values.
-- * 'dimensionType' - The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
mkSetDimension ::
  SetDimension
mkSetDimension =
  SetDimension' {values = Lude.mempty, dimensionType = Lude.Nothing}

-- | The criteria values to use for the segment dimension. Depending on the value of the DimensionType property, endpoints are included or excluded from the segment if their values match the criteria values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdValues :: Lens.Lens' SetDimension [Lude.Text]
sdValues = Lens.lens (values :: SetDimension -> [Lude.Text]) (\s a -> s {values = a} :: SetDimension)
{-# DEPRECATED sdValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
--
-- /Note:/ Consider using 'dimensionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDimensionType :: Lens.Lens' SetDimension (Lude.Maybe DimensionType)
sdDimensionType = Lens.lens (dimensionType :: SetDimension -> Lude.Maybe DimensionType) (\s a -> s {dimensionType = a} :: SetDimension)
{-# DEPRECATED sdDimensionType "Use generic-lens or generic-optics with 'dimensionType' instead." #-}

instance Lude.FromJSON SetDimension where
  parseJSON =
    Lude.withObject
      "SetDimension"
      ( \x ->
          SetDimension'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DimensionType")
      )

instance Lude.ToJSON SetDimension where
  toJSON SetDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Values" Lude..= values),
            ("DimensionType" Lude..=) Lude.<$> dimensionType
          ]
      )
