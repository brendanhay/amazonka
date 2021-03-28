{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetConnectorDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition version, including the connectors that the version contains. Connectors are prebuilt modules that interact with local infrastructure, device protocols, AWS, and other cloud services.
module Network.AWS.Greengrass.GetConnectorDefinitionVersion
    (
    -- * Creating a request
      GetConnectorDefinitionVersion (..)
    , mkGetConnectorDefinitionVersion
    -- ** Request lenses
    , gcdvConnectorDefinitionId
    , gcdvConnectorDefinitionVersionId
    , gcdvNextToken

    -- * Destructuring the response
    , GetConnectorDefinitionVersionResponse (..)
    , mkGetConnectorDefinitionVersionResponse
    -- ** Response lenses
    , gcdvrrsArn
    , gcdvrrsCreationTimestamp
    , gcdvrrsDefinition
    , gcdvrrsId
    , gcdvrrsNextToken
    , gcdvrrsVersion
    , gcdvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConnectorDefinitionVersion' smart constructor.
data GetConnectorDefinitionVersion = GetConnectorDefinitionVersion'
  { connectorDefinitionId :: Core.Text
    -- ^ The ID of the connector definition.
  , connectorDefinitionVersionId :: Core.Text
    -- ^ The ID of the connector definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListConnectorDefinitionVersions'' requests. If the version is the last one that was associated with a connector definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectorDefinitionVersion' value with any optional fields omitted.
mkGetConnectorDefinitionVersion
    :: Core.Text -- ^ 'connectorDefinitionId'
    -> Core.Text -- ^ 'connectorDefinitionVersionId'
    -> GetConnectorDefinitionVersion
mkGetConnectorDefinitionVersion connectorDefinitionId
  connectorDefinitionVersionId
  = GetConnectorDefinitionVersion'{connectorDefinitionId,
                                   connectorDefinitionVersionId, nextToken = Core.Nothing}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvConnectorDefinitionId :: Lens.Lens' GetConnectorDefinitionVersion Core.Text
gcdvConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# INLINEABLE gcdvConnectorDefinitionId #-}
{-# DEPRECATED connectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead"  #-}

-- | The ID of the connector definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListConnectorDefinitionVersions'' requests. If the version is the last one that was associated with a connector definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'connectorDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvConnectorDefinitionVersionId :: Lens.Lens' GetConnectorDefinitionVersion Core.Text
gcdvConnectorDefinitionVersionId = Lens.field @"connectorDefinitionVersionId"
{-# INLINEABLE gcdvConnectorDefinitionVersionId #-}
{-# DEPRECATED connectorDefinitionVersionId "Use generic-lens or generic-optics with 'connectorDefinitionVersionId' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvNextToken :: Lens.Lens' GetConnectorDefinitionVersion (Core.Maybe Core.Text)
gcdvNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcdvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetConnectorDefinitionVersion where
        toQuery GetConnectorDefinitionVersion{..}
          = Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders GetConnectorDefinitionVersion where
        toHeaders GetConnectorDefinitionVersion{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetConnectorDefinitionVersion where
        type Rs GetConnectorDefinitionVersion =
             GetConnectorDefinitionVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/connectors/" Core.<>
                             Core.toText connectorDefinitionId
                             Core.<> "/versions/"
                             Core.<> Core.toText connectorDefinitionVersionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetConnectorDefinitionVersionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Definition"
                     Core.<*> x Core..:? "Id"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> x Core..:? "Version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetConnectorDefinitionVersionResponse' smart constructor.
data GetConnectorDefinitionVersionResponse = GetConnectorDefinitionVersionResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the connector definition version.
  , creationTimestamp :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the connector definition version was created.
  , definition :: Core.Maybe Types.ConnectorDefinitionVersion
    -- ^ Information about the connector definition version.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the connector definition version.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , version :: Core.Maybe Core.Text
    -- ^ The version of the connector definition version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectorDefinitionVersionResponse' value with any optional fields omitted.
mkGetConnectorDefinitionVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetConnectorDefinitionVersionResponse
mkGetConnectorDefinitionVersionResponse responseStatus
  = GetConnectorDefinitionVersionResponse'{arn = Core.Nothing,
                                           creationTimestamp = Core.Nothing,
                                           definition = Core.Nothing, id = Core.Nothing,
                                           nextToken = Core.Nothing, version = Core.Nothing,
                                           responseStatus}

-- | The ARN of the connector definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsArn :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsArn = Lens.field @"arn"
{-# INLINEABLE gcdvrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the connector definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsCreationTimestamp :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE gcdvrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | Information about the connector definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsDefinition :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Types.ConnectorDefinitionVersion)
gcdvrrsDefinition = Lens.field @"definition"
{-# INLINEABLE gcdvrrsDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The ID of the connector definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsId :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsId = Lens.field @"id"
{-# INLINEABLE gcdvrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsNextToken :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcdvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The version of the connector definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsVersion :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsVersion = Lens.field @"version"
{-# INLINEABLE gcdvrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsResponseStatus :: Lens.Lens' GetConnectorDefinitionVersionResponse Core.Int
gcdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
