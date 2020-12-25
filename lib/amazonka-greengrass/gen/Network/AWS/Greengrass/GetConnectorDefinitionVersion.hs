{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetConnectorDefinitionVersion (..),
    mkGetConnectorDefinitionVersion,

    -- ** Request lenses
    gcdvConnectorDefinitionId,
    gcdvConnectorDefinitionVersionId,
    gcdvNextToken,

    -- * Destructuring the response
    GetConnectorDefinitionVersionResponse (..),
    mkGetConnectorDefinitionVersionResponse,

    -- ** Response lenses
    gcdvrrsArn,
    gcdvrrsCreationTimestamp,
    gcdvrrsDefinition,
    gcdvrrsId,
    gcdvrrsNextToken,
    gcdvrrsVersion,
    gcdvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConnectorDefinitionVersion' smart constructor.
data GetConnectorDefinitionVersion = GetConnectorDefinitionVersion'
  { -- | The ID of the connector definition.
    connectorDefinitionId :: Core.Text,
    -- | The ID of the connector definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListConnectorDefinitionVersions'' requests. If the version is the last one that was associated with a connector definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
    connectorDefinitionVersionId :: Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectorDefinitionVersion' value with any optional fields omitted.
mkGetConnectorDefinitionVersion ::
  -- | 'connectorDefinitionId'
  Core.Text ->
  -- | 'connectorDefinitionVersionId'
  Core.Text ->
  GetConnectorDefinitionVersion
mkGetConnectorDefinitionVersion
  connectorDefinitionId
  connectorDefinitionVersionId =
    GetConnectorDefinitionVersion'
      { connectorDefinitionId,
        connectorDefinitionVersionId,
        nextToken = Core.Nothing
      }

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvConnectorDefinitionId :: Lens.Lens' GetConnectorDefinitionVersion Core.Text
gcdvConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# DEPRECATED gcdvConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

-- | The ID of the connector definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListConnectorDefinitionVersions'' requests. If the version is the last one that was associated with a connector definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'connectorDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvConnectorDefinitionVersionId :: Lens.Lens' GetConnectorDefinitionVersion Core.Text
gcdvConnectorDefinitionVersionId = Lens.field @"connectorDefinitionVersionId"
{-# DEPRECATED gcdvConnectorDefinitionVersionId "Use generic-lens or generic-optics with 'connectorDefinitionVersionId' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvNextToken :: Lens.Lens' GetConnectorDefinitionVersion (Core.Maybe Core.Text)
gcdvNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetConnectorDefinitionVersion where
  type
    Rs GetConnectorDefinitionVersion =
      GetConnectorDefinitionVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/connectors/"
                Core.<> (Core.toText connectorDefinitionId)
                Core.<> ("/versions/")
                Core.<> (Core.toText connectorDefinitionVersionId)
            ),
        Core._rqQuery = Core.toQueryValue "NextToken" Core.<$> nextToken,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectorDefinitionVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Definition")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetConnectorDefinitionVersionResponse' smart constructor.
data GetConnectorDefinitionVersionResponse = GetConnectorDefinitionVersionResponse'
  { -- | The ARN of the connector definition version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the connector definition version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | Information about the connector definition version.
    definition :: Core.Maybe Types.ConnectorDefinitionVersion,
    -- | The ID of the connector definition version.
    id :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The version of the connector definition version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectorDefinitionVersionResponse' value with any optional fields omitted.
mkGetConnectorDefinitionVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetConnectorDefinitionVersionResponse
mkGetConnectorDefinitionVersionResponse responseStatus =
  GetConnectorDefinitionVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      definition = Core.Nothing,
      id = Core.Nothing,
      nextToken = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the connector definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsArn :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsArn = Lens.field @"arn"
{-# DEPRECATED gcdvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the connector definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsCreationTimestamp :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gcdvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | Information about the connector definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsDefinition :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Types.ConnectorDefinitionVersion)
gcdvrrsDefinition = Lens.field @"definition"
{-# DEPRECATED gcdvrrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ID of the connector definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsId :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsId = Lens.field @"id"
{-# DEPRECATED gcdvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsNextToken :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcdvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The version of the connector definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsVersion :: Lens.Lens' GetConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrrsVersion = Lens.field @"version"
{-# DEPRECATED gcdvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrrsResponseStatus :: Lens.Lens' GetConnectorDefinitionVersionResponse Core.Int
gcdvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcdvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
