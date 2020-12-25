{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Network.AWS.Greengrass.GetCoreDefinition
  ( -- * Creating a request
    GetCoreDefinition (..),
    mkGetCoreDefinition,

    -- ** Request lenses
    gcdCoreDefinitionId,

    -- * Destructuring the response
    GetCoreDefinitionResponse (..),
    mkGetCoreDefinitionResponse,

    -- ** Response lenses
    gcdrrsArn,
    gcdrrsCreationTimestamp,
    gcdrrsId,
    gcdrrsLastUpdatedTimestamp,
    gcdrrsLatestVersion,
    gcdrrsLatestVersionArn,
    gcdrrsName,
    gcdrrsTags,
    gcdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCoreDefinition' smart constructor.
newtype GetCoreDefinition = GetCoreDefinition'
  { -- | The ID of the core definition.
    coreDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCoreDefinition' value with any optional fields omitted.
mkGetCoreDefinition ::
  -- | 'coreDefinitionId'
  Core.Text ->
  GetCoreDefinition
mkGetCoreDefinition coreDefinitionId =
  GetCoreDefinition' {coreDefinitionId}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdCoreDefinitionId :: Lens.Lens' GetCoreDefinition Core.Text
gcdCoreDefinitionId = Lens.field @"coreDefinitionId"
{-# DEPRECATED gcdCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

instance Core.AWSRequest GetCoreDefinition where
  type Rs GetCoreDefinition = GetCoreDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/cores/"
                Core.<> (Core.toText coreDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreDefinitionResponse'
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

-- | /See:/ 'mkGetCoreDefinitionResponse' smart constructor.
data GetCoreDefinitionResponse = GetCoreDefinitionResponse'
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

-- | Creates a 'GetCoreDefinitionResponse' value with any optional fields omitted.
mkGetCoreDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCoreDefinitionResponse
mkGetCoreDefinitionResponse responseStatus =
  GetCoreDefinitionResponse'
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
gcdrrsArn :: Lens.Lens' GetCoreDefinitionResponse (Core.Maybe Core.Text)
gcdrrsArn = Lens.field @"arn"
{-# DEPRECATED gcdrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsCreationTimestamp :: Lens.Lens' GetCoreDefinitionResponse (Core.Maybe Core.Text)
gcdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gcdrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsId :: Lens.Lens' GetCoreDefinitionResponse (Core.Maybe Core.Text)
gcdrrsId = Lens.field @"id"
{-# DEPRECATED gcdrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsLastUpdatedTimestamp :: Lens.Lens' GetCoreDefinitionResponse (Core.Maybe Core.Text)
gcdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED gcdrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsLatestVersion :: Lens.Lens' GetCoreDefinitionResponse (Core.Maybe Core.Text)
gcdrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED gcdrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsLatestVersionArn :: Lens.Lens' GetCoreDefinitionResponse (Core.Maybe Core.Text)
gcdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED gcdrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsName :: Lens.Lens' GetCoreDefinitionResponse (Core.Maybe Core.Text)
gcdrrsName = Lens.field @"name"
{-# DEPRECATED gcdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsTags :: Lens.Lens' GetCoreDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gcdrrsTags = Lens.field @"tags"
{-# DEPRECATED gcdrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsResponseStatus :: Lens.Lens' GetCoreDefinitionResponse Core.Int
gcdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
