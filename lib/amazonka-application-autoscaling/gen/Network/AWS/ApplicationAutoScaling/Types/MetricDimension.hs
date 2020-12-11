-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.MetricDimension
  ( MetricDimension (..),

    -- * Smart constructor
    mkMetricDimension,

    -- * Lenses
    mdName,
    mdValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the dimension names and values associated with a metric.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { name :: Lude.Text,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- * 'name' - The name of the dimension.
-- * 'value' - The value of the dimension.
mkMetricDimension ::
  -- | 'name'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  MetricDimension
mkMetricDimension pName_ pValue_ =
  MetricDimension' {name = pName_, value = pValue_}

-- | The name of the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdName :: Lens.Lens' MetricDimension Lude.Text
mdName = Lens.lens (name :: MetricDimension -> Lude.Text) (\s a -> s {name = a} :: MetricDimension)
{-# DEPRECATED mdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the dimension.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDimension Lude.Text
mdValue = Lens.lens (value :: MetricDimension -> Lude.Text) (\s a -> s {value = a} :: MetricDimension)
{-# DEPRECATED mdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON MetricDimension where
  parseJSON =
    Lude.withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "Value")
      )

instance Lude.ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("Value" Lude..= value)
          ]
      )
