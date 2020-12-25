{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetResourceDefinition (..),
    mkGetResourceDefinition,

    -- ** Request lenses
    grdResourceDefinitionId,

    -- * Destructuring the response
    GetResourceDefinitionResponse (..),
    mkGetResourceDefinitionResponse,

    -- ** Response lenses
    grdrrsArn,
    grdrrsCreationTimestamp,
    grdrrsId,
    grdrrsLastUpdatedTimestamp,
    grdrrsLatestVersion,
    grdrrsLatestVersionArn,
    grdrrsName,
    grdrrsTags,
    grdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResourceDefinition' smart constructor.
newtype GetResourceDefinition = GetResourceDefinition'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourceDefinition' value with any optional fields omitted.
mkGetResourceDefinition ::
  -- | 'resourceDefinitionId'
  Core.Text ->
  GetResourceDefinition
mkGetResourceDefinition resourceDefinitionId =
  GetResourceDefinition' {resourceDefinitionId}

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdResourceDefinitionId :: Lens.Lens' GetResourceDefinition Core.Text
grdResourceDefinitionId = Lens.field @"resourceDefinitionId"
{-# DEPRECATED grdResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

instance Core.AWSRequest GetResourceDefinition where
  type Rs GetResourceDefinition = GetResourceDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/resources/"
                Core.<> (Core.toText resourceDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceDefinitionResponse'
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

-- | /See:/ 'mkGetResourceDefinitionResponse' smart constructor.
data GetResourceDefinitionResponse = GetResourceDefinitionResponse'
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

-- | Creates a 'GetResourceDefinitionResponse' value with any optional fields omitted.
mkGetResourceDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetResourceDefinitionResponse
mkGetResourceDefinitionResponse responseStatus =
  GetResourceDefinitionResponse'
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
grdrrsArn :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsArn = Lens.field @"arn"
{-# DEPRECATED grdrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsCreationTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED grdrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsId :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsId = Lens.field @"id"
{-# DEPRECATED grdrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsLastUpdatedTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED grdrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsLatestVersion :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED grdrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsLatestVersionArn :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED grdrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsName :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe Core.Text)
grdrrsName = Lens.field @"name"
{-# DEPRECATED grdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsTags :: Lens.Lens' GetResourceDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
grdrrsTags = Lens.field @"tags"
{-# DEPRECATED grdrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsResponseStatus :: Lens.Lens' GetResourceDefinitionResponse Core.Int
grdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
