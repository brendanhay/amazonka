{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.MetricDimension
  ( MetricDimension (..),

    -- * Smart constructor
    mkMetricDimension,

    -- * Lenses
    mdValue,
    mdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a dimension for a customized metric.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | The value of the dimension.
    value :: Lude.Text,
    -- | The name of the dimension.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- * 'value' - The value of the dimension.
-- * 'name' - The name of the dimension.
mkMetricDimension ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  MetricDimension
mkMetricDimension pValue_ pName_ =
  MetricDimension' {value = pValue_, name = pName_}

-- | The value of the dimension.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDimension Lude.Text
mdValue = Lens.lens (value :: MetricDimension -> Lude.Text) (\s a -> s {value = a} :: MetricDimension)
{-# DEPRECATED mdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdName :: Lens.Lens' MetricDimension Lude.Text
mdName = Lens.lens (name :: MetricDimension -> Lude.Text) (\s a -> s {name = a} :: MetricDimension)
{-# DEPRECATED mdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON MetricDimension where
  parseJSON =
    Lude.withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            Lude.<$> (x Lude..: "Value") Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Value" Lude..= value),
            Lude.Just ("Name" Lude..= name)
          ]
      )
