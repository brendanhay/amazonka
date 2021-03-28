{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.LoadBalancer
  ( LoadBalancer (..)
  -- * Smart constructor
  , mkLoadBalancer
  -- * Lenses
  , lbName
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ResourceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a LoadBalancer.
--
-- /See:/ 'mkLoadBalancer' smart constructor.
newtype LoadBalancer = LoadBalancer'
  { name :: Core.Maybe Types.ResourceId
    -- ^ The name of the LoadBalancer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancer' value with any optional fields omitted.
mkLoadBalancer
    :: LoadBalancer
mkLoadBalancer = LoadBalancer'{name = Core.Nothing}

-- | The name of the LoadBalancer.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbName :: Lens.Lens' LoadBalancer (Core.Maybe Types.ResourceId)
lbName = Lens.field @"name"
{-# INLINEABLE lbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML LoadBalancer where
        parseXML x = LoadBalancer' Core.<$> (x Core..@? "Name")
