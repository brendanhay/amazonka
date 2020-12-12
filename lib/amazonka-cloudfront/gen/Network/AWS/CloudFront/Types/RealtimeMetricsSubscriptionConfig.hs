{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
  ( RealtimeMetricsSubscriptionConfig (..),

    -- * Smart constructor
    mkRealtimeMetricsSubscriptionConfig,

    -- * Lenses
    rmscRealtimeMetricsSubscriptionStatus,
  )
where

import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A subscription configuration for additional CloudWatch metrics.
--
-- /See:/ 'mkRealtimeMetricsSubscriptionConfig' smart constructor.
newtype RealtimeMetricsSubscriptionConfig = RealtimeMetricsSubscriptionConfig'
  { realtimeMetricsSubscriptionStatus ::
      RealtimeMetricsSubscriptionStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RealtimeMetricsSubscriptionConfig' with the minimum fields required to make a request.
--
-- * 'realtimeMetricsSubscriptionStatus' - A flag that indicates whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
mkRealtimeMetricsSubscriptionConfig ::
  -- | 'realtimeMetricsSubscriptionStatus'
  RealtimeMetricsSubscriptionStatus ->
  RealtimeMetricsSubscriptionConfig
mkRealtimeMetricsSubscriptionConfig
  pRealtimeMetricsSubscriptionStatus_ =
    RealtimeMetricsSubscriptionConfig'
      { realtimeMetricsSubscriptionStatus =
          pRealtimeMetricsSubscriptionStatus_
      }

-- | A flag that indicates whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /Note:/ Consider using 'realtimeMetricsSubscriptionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmscRealtimeMetricsSubscriptionStatus :: Lens.Lens' RealtimeMetricsSubscriptionConfig RealtimeMetricsSubscriptionStatus
rmscRealtimeMetricsSubscriptionStatus = Lens.lens (realtimeMetricsSubscriptionStatus :: RealtimeMetricsSubscriptionConfig -> RealtimeMetricsSubscriptionStatus) (\s a -> s {realtimeMetricsSubscriptionStatus = a} :: RealtimeMetricsSubscriptionConfig)
{-# DEPRECATED rmscRealtimeMetricsSubscriptionStatus "Use generic-lens or generic-optics with 'realtimeMetricsSubscriptionStatus' instead." #-}

instance Lude.FromXML RealtimeMetricsSubscriptionConfig where
  parseXML x =
    RealtimeMetricsSubscriptionConfig'
      Lude.<$> (x Lude..@ "RealtimeMetricsSubscriptionStatus")

instance Lude.ToXML RealtimeMetricsSubscriptionConfig where
  toXML RealtimeMetricsSubscriptionConfig' {..} =
    Lude.mconcat
      [ "RealtimeMetricsSubscriptionStatus"
          Lude.@= realtimeMetricsSubscriptionStatus
      ]
