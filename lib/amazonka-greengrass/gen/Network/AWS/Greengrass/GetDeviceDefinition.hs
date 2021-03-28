{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a device definition.
module Network.AWS.Greengrass.GetDeviceDefinition
    (
    -- * Creating a request
      GetDeviceDefinition (..)
    , mkGetDeviceDefinition
    -- ** Request lenses
    , gddDeviceDefinitionId

    -- * Destructuring the response
    , GetDeviceDefinitionResponse (..)
    , mkGetDeviceDefinitionResponse
    -- ** Response lenses
    , gddrrsArn
    , gddrrsCreationTimestamp
    , gddrrsId
    , gddrrsLastUpdatedTimestamp
    , gddrrsLatestVersion
    , gddrrsLatestVersionArn
    , gddrrsName
    , gddrrsTags
    , gddrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDeviceDefinition' smart constructor.
newtype GetDeviceDefinition = GetDeviceDefinition'
  { deviceDefinitionId :: Core.Text
    -- ^ The ID of the device definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeviceDefinition' value with any optional fields omitted.
mkGetDeviceDefinition
    :: Core.Text -- ^ 'deviceDefinitionId'
    -> GetDeviceDefinition
mkGetDeviceDefinition deviceDefinitionId
  = GetDeviceDefinition'{deviceDefinitionId}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddDeviceDefinitionId :: Lens.Lens' GetDeviceDefinition Core.Text
gddDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# INLINEABLE gddDeviceDefinitionId #-}
{-# DEPRECATED deviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead"  #-}

instance Core.ToQuery GetDeviceDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDeviceDefinition where
        toHeaders GetDeviceDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetDeviceDefinition where
        type Rs GetDeviceDefinition = GetDeviceDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/devices/" Core.<>
                             Core.toText deviceDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDeviceDefinitionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "LastUpdatedTimestamp"
                     Core.<*> x Core..:? "LatestVersion"
                     Core.<*> x Core..:? "LatestVersionArn"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDeviceDefinitionResponse' smart constructor.
data GetDeviceDefinitionResponse = GetDeviceDefinitionResponse'
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

-- | Creates a 'GetDeviceDefinitionResponse' value with any optional fields omitted.
mkGetDeviceDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDeviceDefinitionResponse
mkGetDeviceDefinitionResponse responseStatus
  = GetDeviceDefinitionResponse'{arn = Core.Nothing,
                                 creationTimestamp = Core.Nothing, id = Core.Nothing,
                                 lastUpdatedTimestamp = Core.Nothing, latestVersion = Core.Nothing,
                                 latestVersionArn = Core.Nothing, name = Core.Nothing,
                                 tags = Core.Nothing, responseStatus}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsArn :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsArn = Lens.field @"arn"
{-# INLINEABLE gddrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsCreationTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE gddrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsId :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsId = Lens.field @"id"
{-# INLINEABLE gddrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsLastUpdatedTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE gddrrsLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsLatestVersion :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE gddrrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsLatestVersionArn :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# INLINEABLE gddrrsLatestVersionArn #-}
{-# DEPRECATED latestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsName :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsName = Lens.field @"name"
{-# INLINEABLE gddrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsTags :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gddrrsTags = Lens.field @"tags"
{-# INLINEABLE gddrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsResponseStatus :: Lens.Lens' GetDeviceDefinitionResponse Core.Int
gddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
