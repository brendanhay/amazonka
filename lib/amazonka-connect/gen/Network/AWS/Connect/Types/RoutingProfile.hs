{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.RoutingProfile
  ( RoutingProfile (..)
  -- * Smart constructor
  , mkRoutingProfile
  -- * Lenses
  , rpDefaultOutboundQueueId
  , rpDescription
  , rpInstanceId
  , rpMediaConcurrencies
  , rpName
  , rpRoutingProfileArn
  , rpRoutingProfileId
  , rpTags
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.InstanceId as Types
import qualified Network.AWS.Connect.Types.MediaConcurrency as Types
import qualified Network.AWS.Connect.Types.QueueId as Types
import qualified Network.AWS.Connect.Types.RoutingProfileDescription as Types
import qualified Network.AWS.Connect.Types.RoutingProfileId as Types
import qualified Network.AWS.Connect.Types.RoutingProfileName as Types
import qualified Network.AWS.Connect.Types.TagKey as Types
import qualified Network.AWS.Connect.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a routing profile.
--
-- /See:/ 'mkRoutingProfile' smart constructor.
data RoutingProfile = RoutingProfile'
  { defaultOutboundQueueId :: Core.Maybe Types.QueueId
    -- ^ The identifier of the default outbound queue for this routing profile.
  , description :: Core.Maybe Types.RoutingProfileDescription
    -- ^ The description of the routing profile.
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , mediaConcurrencies :: Core.Maybe [Types.MediaConcurrency]
    -- ^ The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
  , name :: Core.Maybe Types.RoutingProfileName
    -- ^ The name of the routing profile.
  , routingProfileArn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the routing profile.
  , routingProfileId :: Core.Maybe Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ One or more tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoutingProfile' value with any optional fields omitted.
mkRoutingProfile
    :: RoutingProfile
mkRoutingProfile
  = RoutingProfile'{defaultOutboundQueueId = Core.Nothing,
                    description = Core.Nothing, instanceId = Core.Nothing,
                    mediaConcurrencies = Core.Nothing, name = Core.Nothing,
                    routingProfileArn = Core.Nothing, routingProfileId = Core.Nothing,
                    tags = Core.Nothing}

-- | The identifier of the default outbound queue for this routing profile.
--
-- /Note:/ Consider using 'defaultOutboundQueueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpDefaultOutboundQueueId :: Lens.Lens' RoutingProfile (Core.Maybe Types.QueueId)
rpDefaultOutboundQueueId = Lens.field @"defaultOutboundQueueId"
{-# INLINEABLE rpDefaultOutboundQueueId #-}
{-# DEPRECATED defaultOutboundQueueId "Use generic-lens or generic-optics with 'defaultOutboundQueueId' instead"  #-}

-- | The description of the routing profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpDescription :: Lens.Lens' RoutingProfile (Core.Maybe Types.RoutingProfileDescription)
rpDescription = Lens.field @"description"
{-# INLINEABLE rpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpInstanceId :: Lens.Lens' RoutingProfile (Core.Maybe Types.InstanceId)
rpInstanceId = Lens.field @"instanceId"
{-# INLINEABLE rpInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
--
-- /Note:/ Consider using 'mediaConcurrencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpMediaConcurrencies :: Lens.Lens' RoutingProfile (Core.Maybe [Types.MediaConcurrency])
rpMediaConcurrencies = Lens.field @"mediaConcurrencies"
{-# INLINEABLE rpMediaConcurrencies #-}
{-# DEPRECATED mediaConcurrencies "Use generic-lens or generic-optics with 'mediaConcurrencies' instead"  #-}

-- | The name of the routing profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpName :: Lens.Lens' RoutingProfile (Core.Maybe Types.RoutingProfileName)
rpName = Lens.field @"name"
{-# INLINEABLE rpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Amazon Resource Name (ARN) of the routing profile.
--
-- /Note:/ Consider using 'routingProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRoutingProfileArn :: Lens.Lens' RoutingProfile (Core.Maybe Types.ARN)
rpRoutingProfileArn = Lens.field @"routingProfileArn"
{-# INLINEABLE rpRoutingProfileArn #-}
{-# DEPRECATED routingProfileArn "Use generic-lens or generic-optics with 'routingProfileArn' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRoutingProfileId :: Lens.Lens' RoutingProfile (Core.Maybe Types.RoutingProfileId)
rpRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE rpRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpTags :: Lens.Lens' RoutingProfile (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
rpTags = Lens.field @"tags"
{-# INLINEABLE rpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON RoutingProfile where
        parseJSON
          = Core.withObject "RoutingProfile" Core.$
              \ x ->
                RoutingProfile' Core.<$>
                  (x Core..:? "DefaultOutboundQueueId") Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "InstanceId"
                    Core.<*> x Core..:? "MediaConcurrencies"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "RoutingProfileArn"
                    Core.<*> x Core..:? "RoutingProfileId"
                    Core.<*> x Core..:? "Tags"
