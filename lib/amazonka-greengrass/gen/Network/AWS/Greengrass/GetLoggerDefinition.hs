{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetLoggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition.
module Network.AWS.Greengrass.GetLoggerDefinition
    (
    -- * Creating a request
      GetLoggerDefinition (..)
    , mkGetLoggerDefinition
    -- ** Request lenses
    , gldLoggerDefinitionId

    -- * Destructuring the response
    , GetLoggerDefinitionResponse (..)
    , mkGetLoggerDefinitionResponse
    -- ** Response lenses
    , gldrrsArn
    , gldrrsCreationTimestamp
    , gldrrsId
    , gldrrsLastUpdatedTimestamp
    , gldrrsLatestVersion
    , gldrrsLatestVersionArn
    , gldrrsName
    , gldrrsTags
    , gldrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLoggerDefinition' smart constructor.
newtype GetLoggerDefinition = GetLoggerDefinition'
  { loggerDefinitionId :: Core.Text
    -- ^ The ID of the logger definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggerDefinition' value with any optional fields omitted.
mkGetLoggerDefinition
    :: Core.Text -- ^ 'loggerDefinitionId'
    -> GetLoggerDefinition
mkGetLoggerDefinition loggerDefinitionId
  = GetLoggerDefinition'{loggerDefinitionId}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldLoggerDefinitionId :: Lens.Lens' GetLoggerDefinition Core.Text
gldLoggerDefinitionId = Lens.field @"loggerDefinitionId"
{-# INLINEABLE gldLoggerDefinitionId #-}
{-# DEPRECATED loggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead"  #-}

instance Core.ToQuery GetLoggerDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetLoggerDefinition where
        toHeaders GetLoggerDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetLoggerDefinition where
        type Rs GetLoggerDefinition = GetLoggerDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/loggers/" Core.<>
                             Core.toText loggerDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLoggerDefinitionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "LastUpdatedTimestamp"
                     Core.<*> x Core..:? "LatestVersion"
                     Core.<*> x Core..:? "LatestVersionArn"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetLoggerDefinitionResponse' smart constructor.
data GetLoggerDefinitionResponse = GetLoggerDefinitionResponse'
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

-- | Creates a 'GetLoggerDefinitionResponse' value with any optional fields omitted.
mkGetLoggerDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLoggerDefinitionResponse
mkGetLoggerDefinitionResponse responseStatus
  = GetLoggerDefinitionResponse'{arn = Core.Nothing,
                                 creationTimestamp = Core.Nothing, id = Core.Nothing,
                                 lastUpdatedTimestamp = Core.Nothing, latestVersion = Core.Nothing,
                                 latestVersionArn = Core.Nothing, name = Core.Nothing,
                                 tags = Core.Nothing, responseStatus}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsArn :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsArn = Lens.field @"arn"
{-# INLINEABLE gldrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsCreationTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE gldrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsId :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsId = Lens.field @"id"
{-# INLINEABLE gldrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsLastUpdatedTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE gldrrsLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsLatestVersion :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE gldrrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsLatestVersionArn :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# INLINEABLE gldrrsLatestVersionArn #-}
{-# DEPRECATED latestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsName :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsName = Lens.field @"name"
{-# INLINEABLE gldrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsTags :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gldrrsTags = Lens.field @"tags"
{-# INLINEABLE gldrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsResponseStatus :: Lens.Lens' GetLoggerDefinitionResponse Core.Int
gldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
