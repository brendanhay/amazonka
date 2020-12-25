{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetConnectorDefinition (..),
    mkGetConnectorDefinition,

    -- ** Request lenses
    gcdConnectorDefinitionId,

    -- * Destructuring the response
    GetConnectorDefinitionResponse (..),
    mkGetConnectorDefinitionResponse,

    -- ** Response lenses
    grsArn,
    grsCreationTimestamp,
    grsId,
    grsLastUpdatedTimestamp,
    grsLatestVersion,
    grsLatestVersionArn,
    grsName,
    grsTags,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConnectorDefinition' smart constructor.
newtype GetConnectorDefinition = GetConnectorDefinition'
  { -- | The ID of the connector definition.
    connectorDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectorDefinition' value with any optional fields omitted.
mkGetConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Core.Text ->
  GetConnectorDefinition
mkGetConnectorDefinition connectorDefinitionId =
  GetConnectorDefinition' {connectorDefinitionId}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdConnectorDefinitionId :: Lens.Lens' GetConnectorDefinition Core.Text
gcdConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# DEPRECATED gcdConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

instance Core.AWSRequest GetConnectorDefinition where
  type Rs GetConnectorDefinition = GetConnectorDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/connectors/"
                Core.<> (Core.toText connectorDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectorDefinitionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetConnectorDefinitionResponse' smart constructor.
data GetConnectorDefinitionResponse = GetConnectorDefinitionResponse'
  { -- | The ARN of the definition.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the definition was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the definition.
    id :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last updated.
    lastUpdatedTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Core.Maybe Core.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Core.Maybe Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectorDefinitionResponse' value with any optional fields omitted.
mkGetConnectorDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetConnectorDefinitionResponse
mkGetConnectorDefinitionResponse responseStatus =
  GetConnectorDefinitionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing,
      latestVersion = Core.Nothing,
      latestVersionArn = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsArn :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsArn = Lens.field @"arn"
{-# DEPRECATED grsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsCreationTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED grsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsId :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsId = Lens.field @"id"
{-# DEPRECATED grsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLastUpdatedTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED grsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLatestVersion :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED grsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLatestVersionArn :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED grsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsName :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe Core.Text)
grsName = Lens.field @"name"
{-# DEPRECATED grsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsTags :: Lens.Lens' GetConnectorDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
grsTags = Lens.field @"tags"
{-# DEPRECATED grsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetConnectorDefinitionResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
