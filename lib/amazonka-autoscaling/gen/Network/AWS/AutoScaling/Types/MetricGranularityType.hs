{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricGranularityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricGranularityType
  ( MetricGranularityType (..),

    -- * Smart constructor
    mkMetricGranularityType,

    -- * Lenses
    mgtGranularity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a granularity of a metric.
--
-- /See:/ 'mkMetricGranularityType' smart constructor.
newtype MetricGranularityType = MetricGranularityType'
  { granularity ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricGranularityType' with the minimum fields required to make a request.
--
-- * 'granularity' - The granularity. The only valid value is @1Minute@ .
mkMetricGranularityType ::
  MetricGranularityType
mkMetricGranularityType =
  MetricGranularityType' {granularity = Lude.Nothing}

-- | The granularity. The only valid value is @1Minute@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgtGranularity :: Lens.Lens' MetricGranularityType (Lude.Maybe Lude.Text)
mgtGranularity = Lens.lens (granularity :: MetricGranularityType -> Lude.Maybe Lude.Text) (\s a -> s {granularity = a} :: MetricGranularityType)
{-# DEPRECATED mgtGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

instance Lude.FromXML MetricGranularityType where
  parseXML x =
    MetricGranularityType' Lude.<$> (x Lude..@? "Granularity")
