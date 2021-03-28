{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetResourceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition, including its creation time and latest version.
module Network.AWS.Greengrass.GetResourceDefinition
    (
    -- * Creating a request
      GetResourceDefinition (..)
    , mkGetResourceDefinition
    -- ** Request lenses
    , grdResourceDefinitionId

    -- * Destructuring the response
    , GetResourceDefinitionResponse (..)
    , mkGetResourceDefinitionResponse
    -- ** Response lenses
    , grdrrsArn
    , grdrrsCreationTimestamp
    , grdrrsId
    , grdrrsLastUpdatedTimestamp
    , grdrrsLatestVersion
    , grdrrsLatestVersionArn
    , grdrrsName
    , grdrrsTags
    , grdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResourceDefinition' smart constructor.
newtype GetResourceDefinition = GetResourceDefinition'
  { resourceDefinitionId :: Core.Text
    -- ^ The ID of the resource definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourceDefinition' value with any optional fields omitted.
mkGetResourceDefinition
    :: Core.Text -- ^ 'resourceDefinitionId'
    -> GetResourceDefinition
mkGetResourceDefinition resourceDefinitionId
  = GetResourceDefinition'{resourceDefinitionId}

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdResourceDefinitionId :: Lens.Lens' GetResourceDefinition Core.Text
grdResourceDefinitionId = Lens.field @"resourceDefinitionId"
{-# INLINEABLE grdResourceDefinitionId #-}
{-# DEPRECATED resourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead"  #-}

instance Core.ToQuery GetResourceDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetResourceDefinition where
        toHeaders GetResourceDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetResourceDefinition where
        type Rs GetResourceDefinition = GetResourceDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/resources/" Core.<>
                             Core.toText resourceDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetResourceDefinitionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "LastUpdatedTimestamp"
                     Core.<*> x Core..:? "LatestVersion"
                     Core.<*> x Core..:? "LatestVersionArn"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetResourceDefinitionResponse' smart constructor.
data GetResourceDefinitionResponse = GetResourceDefinitionResponse'
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

-- | Creates a 'GetResourceDefinitionResponse' value with any optional fields omitted.
mkGetResourceDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetResourceDefinitionResponse
mkGetResourceDefinitionResponse responseStatus
  = GetResourceDefinitionResponse'{arn = Core.Nothing,
                                   creationTimestamp = Core.Nothing, id = Core.Nothing,
                                   lastUpdatedTimestamp = Core.Nothing,
                                   latestVersion = Core.Nothing, latestVersionArn = Core.Nothing,
                                   name = Core.Nothing, tags = Core.Nothing, responseStatus}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsArn :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsArn = Lens.field @"arn"
{-# INLINEABLE grdrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsCreationTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE grdrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsId :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsId = Lens.field @"id"
{-# INLINEABLE grdrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsLastUpdatedTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE grdrrsLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsLatestVersion :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE grdrrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsLatestVersionArn :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# INLINEABLE grdrrsLatestVersionArn #-}
{-# DEPRECATED latestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsName :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsName = Lens.field @"name"
{-# INLINEABLE grdrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsTags :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
grdrrsTags = Lens.field @"tags"
{-# INLINEABLE grdrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsResponseStatus :: Lens.Lens' GetResourceDefinitionResponse Core.Int
grdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
