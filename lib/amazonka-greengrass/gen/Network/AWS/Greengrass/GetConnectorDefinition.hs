{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition.
module Network.AWS.Greengrass.GetConnectorDefinition
    (
    -- * Creating a request
      GetConnectorDefinition (..)
    , mkGetConnectorDefinition
    -- ** Request lenses
    , gcdConnectorDefinitionId

    -- * Destructuring the response
    , GetConnectorDefinitionResponse (..)
    , mkGetConnectorDefinitionResponse
    -- ** Response lenses
    , grsArn
    , grsCreationTimestamp
    , grsId
    , grsLastUpdatedTimestamp
    , grsLatestVersion
    , grsLatestVersionArn
    , grsName
    , grsTags
    , grsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConnectorDefinition' smart constructor.
newtype GetConnectorDefinition = GetConnectorDefinition'
  { connectorDefinitionId :: Core.Text
    -- ^ The ID of the connector definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectorDefinition' value with any optional fields omitted.
mkGetConnectorDefinition
    :: Core.Text -- ^ 'connectorDefinitionId'
    -> GetConnectorDefinition
mkGetConnectorDefinition connectorDefinitionId
  = GetConnectorDefinition'{connectorDefinitionId}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdConnectorDefinitionId :: Lens.Lens' GetConnectorDefinition Core.Text
gcdConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# INLINEABLE gcdConnectorDefinitionId #-}
{-# DEPRECATED connectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead"  #-}

instance Core.ToQuery GetConnectorDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetConnectorDefinition where
        toHeaders GetConnectorDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetConnectorDefinition where
        type Rs GetConnectorDefinition = GetConnectorDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/connectors/" Core.<>
                             Core.toText connectorDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetConnectorDefinitionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "LastUpdatedTimestamp"
                     Core.<*> x Core..:? "LatestVersion"
                     Core.<*> x Core..:? "LatestVersionArn"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetConnectorDefinitionResponse' smart constructor.
data GetConnectorDefinitionResponse = GetConnectorDefinitionResponse'
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

-- | Creates a 'GetConnectorDefinitionResponse' value with any optional fields omitted.
mkGetConnectorDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetConnectorDefinitionResponse
mkGetConnectorDefinitionResponse responseStatus
  = GetConnectorDefinitionResponse'{arn = Core.Nothing,
                                    creationTimestamp = Core.Nothing, id = Core.Nothing,
                                    lastUpdatedTimestamp = Core.Nothing,
                                    latestVersion = Core.Nothing, latestVersionArn = Core.Nothing,
                                    name = Core.Nothing, tags = Core.Nothing, responseStatus}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsArn :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsArn = Lens.field @"arn"
{-# INLINEABLE grsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsCreationTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE grsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsId :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsId = Lens.field @"id"
{-# INLINEABLE grsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLastUpdatedTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE grsLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLatestVersion :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE grsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLatestVersionArn :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsLatestVersionArn = Lens.field @"latestVersionArn"
{-# INLINEABLE grsLatestVersionArn #-}
{-# DEPRECATED latestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsName :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsName = Lens.field @"name"
{-# INLINEABLE grsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsTags :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
grsTags = Lens.field @"tags"
{-# INLINEABLE grsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetConnectorDefinitionResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
