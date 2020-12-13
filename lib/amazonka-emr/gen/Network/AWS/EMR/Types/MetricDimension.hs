{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.MetricDimension
  ( MetricDimension (..),

    -- * Smart constructor
    mkMetricDimension,

    -- * Lenses
    mdValue,
    mdKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A CloudWatch dimension, which is specified using a @Key@ (known as a @Name@ in CloudWatch), @Value@ pair. By default, Amazon EMR uses one dimension whose @Key@ is @JobFlowID@ and @Value@ is a variable representing the cluster ID, which is @> {emr.clusterId}@ . This enables the rule to bootstrap when the cluster ID becomes available.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | The dimension value.
    value :: Lude.Maybe Lude.Text,
    -- | The dimension name.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- * 'value' - The dimension value.
-- * 'key' - The dimension name.
mkMetricDimension ::
  MetricDimension
mkMetricDimension =
  MetricDimension' {value = Lude.Nothing, key = Lude.Nothing}

-- | The dimension value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDimension (Lude.Maybe Lude.Text)
mdValue = Lens.lens (value :: MetricDimension -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: MetricDimension)
{-# DEPRECATED mdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The dimension name.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdKey :: Lens.Lens' MetricDimension (Lude.Maybe Lude.Text)
mdKey = Lens.lens (key :: MetricDimension -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: MetricDimension)
{-# DEPRECATED mdKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON MetricDimension where
  parseJSON =
    Lude.withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Key")
      )

instance Lude.ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Key" Lude..=) Lude.<$> key]
      )
