{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a subscription definition.
module Network.AWS.Greengrass.GetSubscriptionDefinition
    (
    -- * Creating a request
      GetSubscriptionDefinition (..)
    , mkGetSubscriptionDefinition
    -- ** Request lenses
    , gsdSubscriptionDefinitionId

    -- * Destructuring the response
    , GetSubscriptionDefinitionResponse (..)
    , mkGetSubscriptionDefinitionResponse
    -- ** Response lenses
    , gsdrrsArn
    , gsdrrsCreationTimestamp
    , gsdrrsId
    , gsdrrsLastUpdatedTimestamp
    , gsdrrsLatestVersion
    , gsdrrsLatestVersionArn
    , gsdrrsName
    , gsdrrsTags
    , gsdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSubscriptionDefinition' smart constructor.
newtype GetSubscriptionDefinition = GetSubscriptionDefinition'
  { subscriptionDefinitionId :: Core.Text
    -- ^ The ID of the subscription definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionDefinition' value with any optional fields omitted.
mkGetSubscriptionDefinition
    :: Core.Text -- ^ 'subscriptionDefinitionId'
    -> GetSubscriptionDefinition
mkGetSubscriptionDefinition subscriptionDefinitionId
  = GetSubscriptionDefinition'{subscriptionDefinitionId}

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdSubscriptionDefinitionId :: Lens.Lens' GetSubscriptionDefinition Core.Text
gsdSubscriptionDefinitionId = Lens.field @"subscriptionDefinitionId"
{-# INLINEABLE gsdSubscriptionDefinitionId #-}
{-# DEPRECATED subscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead"  #-}

instance Core.ToQuery GetSubscriptionDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSubscriptionDefinition where
        toHeaders GetSubscriptionDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetSubscriptionDefinition where
        type Rs GetSubscriptionDefinition =
             GetSubscriptionDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/subscriptions/" Core.<>
                             Core.toText subscriptionDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSubscriptionDefinitionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "LastUpdatedTimestamp"
                     Core.<*> x Core..:? "LatestVersion"
                     Core.<*> x Core..:? "LatestVersionArn"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSubscriptionDefinitionResponse' smart constructor.
data GetSubscriptionDefinitionResponse = GetSubscriptionDefinitionResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the definition.
  , creationTimestamp :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the definition was created.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the definition.
  , lastUpdatedTimestamp :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the definition was last updated.
  , latestVersion :: Core.Maybe Core.Text
    -- ^ The ID of the latest version associated with the definition.
  , latestVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the latest version associated with the definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the definition.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Tag(s) attached to the resource arn.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionDefinitionResponse' value with any optional fields omitted.
mkGetSubscriptionDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSubscriptionDefinitionResponse
mkGetSubscriptionDefinitionResponse responseStatus
  = GetSubscriptionDefinitionResponse'{arn = Core.Nothing,
                                       creationTimestamp = Core.Nothing, id = Core.Nothing,
                                       lastUpdatedTimestamp = Core.Nothing,
                                       latestVersion = Core.Nothing,
                                       latestVersionArn = Core.Nothing, name = Core.Nothing,
                                       tags = Core.Nothing, responseStatus}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsArn :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsArn = Lens.field @"arn"
{-# INLINEABLE gsdrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsCreationTimestamp :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE gsdrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsId :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsId = Lens.field @"id"
{-# INLINEABLE gsdrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsLastUpdatedTimestamp :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE gsdrrsLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsLatestVersion :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE gsdrrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsLatestVersionArn :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# INLINEABLE gsdrrsLatestVersionArn #-}
{-# DEPRECATED latestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsName :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsName = Lens.field @"name"
{-# INLINEABLE gsdrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsTags :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gsdrrsTags = Lens.field @"tags"
{-# INLINEABLE gsdrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsResponseStatus :: Lens.Lens' GetSubscriptionDefinitionResponse Core.Int
gsdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
