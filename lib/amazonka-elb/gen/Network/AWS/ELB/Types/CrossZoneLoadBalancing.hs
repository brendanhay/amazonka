{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.CrossZoneLoadBalancing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.CrossZoneLoadBalancing
  ( CrossZoneLoadBalancing (..),

    -- * Smart constructor
    mkCrossZoneLoadBalancing,

    -- * Lenses
    czlbEnabled,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the @CrossZoneLoadBalancing@ attribute.
--
-- /See:/ 'mkCrossZoneLoadBalancing' smart constructor.
newtype CrossZoneLoadBalancing = CrossZoneLoadBalancing'
  { -- | Specifies whether cross-zone load balancing is enabled for the load balancer.
    enabled :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CrossZoneLoadBalancing' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies whether cross-zone load balancing is enabled for the load balancer.
mkCrossZoneLoadBalancing ::
  -- | 'enabled'
  Lude.Bool ->
  CrossZoneLoadBalancing
mkCrossZoneLoadBalancing pEnabled_ =
  CrossZoneLoadBalancing' {enabled = pEnabled_}

-- | Specifies whether cross-zone load balancing is enabled for the load balancer.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
czlbEnabled :: Lens.Lens' CrossZoneLoadBalancing Lude.Bool
czlbEnabled = Lens.lens (enabled :: CrossZoneLoadBalancing -> Lude.Bool) (\s a -> s {enabled = a} :: CrossZoneLoadBalancing)
{-# DEPRECATED czlbEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML CrossZoneLoadBalancing where
  parseXML x = CrossZoneLoadBalancing' Lude.<$> (x Lude..@ "Enabled")

instance Lude.ToQuery CrossZoneLoadBalancing where
  toQuery CrossZoneLoadBalancing' {..} =
    Lude.mconcat ["Enabled" Lude.=: enabled]
