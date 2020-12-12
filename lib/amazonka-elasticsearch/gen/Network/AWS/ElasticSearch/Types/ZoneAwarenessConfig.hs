{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
  ( ZoneAwarenessConfig (..),

    -- * Smart constructor
    mkZoneAwarenessConfig,

    -- * Lenses
    zacAvailabilityZoneCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the zone awareness configuration for the domain cluster, such as the number of availability zones.
--
-- /See:/ 'mkZoneAwarenessConfig' smart constructor.
newtype ZoneAwarenessConfig = ZoneAwarenessConfig'
  { availabilityZoneCount ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ZoneAwarenessConfig' with the minimum fields required to make a request.
--
-- * 'availabilityZoneCount' - An integer value to indicate the number of availability zones for a domain when zone awareness is enabled. This should be equal to number of subnets if VPC endpoints is enabled
mkZoneAwarenessConfig ::
  ZoneAwarenessConfig
mkZoneAwarenessConfig =
  ZoneAwarenessConfig' {availabilityZoneCount = Lude.Nothing}

-- | An integer value to indicate the number of availability zones for a domain when zone awareness is enabled. This should be equal to number of subnets if VPC endpoints is enabled
--
-- /Note:/ Consider using 'availabilityZoneCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
zacAvailabilityZoneCount :: Lens.Lens' ZoneAwarenessConfig (Lude.Maybe Lude.Int)
zacAvailabilityZoneCount = Lens.lens (availabilityZoneCount :: ZoneAwarenessConfig -> Lude.Maybe Lude.Int) (\s a -> s {availabilityZoneCount = a} :: ZoneAwarenessConfig)
{-# DEPRECATED zacAvailabilityZoneCount "Use generic-lens or generic-optics with 'availabilityZoneCount' instead." #-}

instance Lude.FromJSON ZoneAwarenessConfig where
  parseJSON =
    Lude.withObject
      "ZoneAwarenessConfig"
      ( \x ->
          ZoneAwarenessConfig' Lude.<$> (x Lude..:? "AvailabilityZoneCount")
      )

instance Lude.ToJSON ZoneAwarenessConfig where
  toJSON ZoneAwarenessConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [("AvailabilityZoneCount" Lude..=) Lude.<$> availabilityZoneCount]
      )
