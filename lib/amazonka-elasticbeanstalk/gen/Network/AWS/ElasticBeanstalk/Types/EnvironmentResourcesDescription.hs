{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
  ( EnvironmentResourcesDescription (..),

    -- * Smart constructor
    mkEnvironmentResourcesDescription,

    -- * Lenses
    erdLoadBalancer,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the AWS resources in use by this environment. This data is not live data.
--
-- /See:/ 'mkEnvironmentResourcesDescription' smart constructor.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription'
  { -- | Describes the LoadBalancer.
    loadBalancer :: Core.Maybe Types.LoadBalancerDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentResourcesDescription' value with any optional fields omitted.
mkEnvironmentResourcesDescription ::
  EnvironmentResourcesDescription
mkEnvironmentResourcesDescription =
  EnvironmentResourcesDescription' {loadBalancer = Core.Nothing}

-- | Describes the LoadBalancer.
--
-- /Note:/ Consider using 'loadBalancer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdLoadBalancer :: Lens.Lens' EnvironmentResourcesDescription (Core.Maybe Types.LoadBalancerDescription)
erdLoadBalancer = Lens.field @"loadBalancer"
{-# DEPRECATED erdLoadBalancer "Use generic-lens or generic-optics with 'loadBalancer' instead." #-}

instance Core.FromXML EnvironmentResourcesDescription where
  parseXML x =
    EnvironmentResourcesDescription'
      Core.<$> (x Core..@? "LoadBalancer")
