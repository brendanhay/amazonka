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

import Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the AWS resources in use by this environment. This data is not live data.
--
-- /See:/ 'mkEnvironmentResourcesDescription' smart constructor.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription'
  { loadBalancer ::
      Lude.Maybe
        LoadBalancerDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentResourcesDescription' with the minimum fields required to make a request.
--
-- * 'loadBalancer' - Describes the LoadBalancer.
mkEnvironmentResourcesDescription ::
  EnvironmentResourcesDescription
mkEnvironmentResourcesDescription =
  EnvironmentResourcesDescription' {loadBalancer = Lude.Nothing}

-- | Describes the LoadBalancer.
--
-- /Note:/ Consider using 'loadBalancer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdLoadBalancer :: Lens.Lens' EnvironmentResourcesDescription (Lude.Maybe LoadBalancerDescription)
erdLoadBalancer = Lens.lens (loadBalancer :: EnvironmentResourcesDescription -> Lude.Maybe LoadBalancerDescription) (\s a -> s {loadBalancer = a} :: EnvironmentResourcesDescription)
{-# DEPRECATED erdLoadBalancer "Use generic-lens or generic-optics with 'loadBalancer' instead." #-}

instance Lude.FromXML EnvironmentResourcesDescription where
  parseXML x =
    EnvironmentResourcesDescription'
      Lude.<$> (x Lude..@? "LoadBalancer")
