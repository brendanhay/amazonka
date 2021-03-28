{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ElasticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.ElasticIp
  ( ElasticIp (..)
  -- * Smart constructor
  , mkElasticIp
  -- * Lenses
  , eiDomain
  , eiInstanceId
  , eiIp
  , eiName
  , eiRegion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Elastic IP address.
--
-- /See:/ 'mkElasticIp' smart constructor.
data ElasticIp = ElasticIp'
  { domain :: Core.Maybe Core.Text
    -- ^ The domain.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance that the address is attached to.
  , ip :: Core.Maybe Core.Text
    -- ^ The IP address.
  , name :: Core.Maybe Core.Text
    -- ^ The name.
  , region :: Core.Maybe Core.Text
    -- ^ The AWS region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticIp' value with any optional fields omitted.
mkElasticIp
    :: ElasticIp
mkElasticIp
  = ElasticIp'{domain = Core.Nothing, instanceId = Core.Nothing,
               ip = Core.Nothing, name = Core.Nothing, region = Core.Nothing}

-- | The domain.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDomain :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
eiDomain = Lens.field @"domain"
{-# INLINEABLE eiDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The ID of the instance that the address is attached to.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiInstanceId :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
eiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE eiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The IP address.
--
-- /Note:/ Consider using 'ip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiIp :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
eiIp = Lens.field @"ip"
{-# INLINEABLE eiIp #-}
{-# DEPRECATED ip "Use generic-lens or generic-optics with 'ip' instead"  #-}

-- | The name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiName :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
eiName = Lens.field @"name"
{-# INLINEABLE eiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The AWS region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiRegion :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
eiRegion = Lens.field @"region"
{-# INLINEABLE eiRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

instance Core.FromJSON ElasticIp where
        parseJSON
          = Core.withObject "ElasticIp" Core.$
              \ x ->
                ElasticIp' Core.<$>
                  (x Core..:? "Domain") Core.<*> x Core..:? "InstanceId" Core.<*>
                    x Core..:? "Ip"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Region"
