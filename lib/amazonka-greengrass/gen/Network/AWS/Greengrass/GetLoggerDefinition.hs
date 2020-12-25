{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetLoggerDefinition (..),
    mkGetLoggerDefinition,

    -- ** Request lenses
    gldLoggerDefinitionId,

    -- * Destructuring the response
    GetLoggerDefinitionResponse (..),
    mkGetLoggerDefinitionResponse,

    -- ** Response lenses
    gldrrsArn,
    gldrrsCreationTimestamp,
    gldrrsId,
    gldrrsLastUpdatedTimestamp,
    gldrrsLatestVersion,
    gldrrsLatestVersionArn,
    gldrrsName,
    gldrrsTags,
    gldrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLoggerDefinition' smart constructor.
newtype GetLoggerDefinition = GetLoggerDefinition'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggerDefinition' value with any optional fields omitted.
mkGetLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Core.Text ->
  GetLoggerDefinition
mkGetLoggerDefinition loggerDefinitionId =
  GetLoggerDefinition' {loggerDefinitionId}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldLoggerDefinitionId :: Lens.Lens' GetLoggerDefinition Core.Text
gldLoggerDefinitionId = Lens.field @"loggerDefinitionId"
{-# DEPRECATED gldLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

instance Core.AWSRequest GetLoggerDefinition where
  type Rs GetLoggerDefinition = GetLoggerDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/loggers/"
                Core.<> (Core.toText loggerDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggerDefinitionResponse'
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

-- | /See:/ 'mkGetLoggerDefinitionResponse' smart constructor.
data GetLoggerDefinitionResponse = GetLoggerDefinitionResponse'
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

-- | Creates a 'GetLoggerDefinitionResponse' value with any optional fields omitted.
mkGetLoggerDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLoggerDefinitionResponse
mkGetLoggerDefinitionResponse responseStatus =
  GetLoggerDefinitionResponse'
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
gldrrsArn :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsArn = Lens.field @"arn"
{-# DEPRECATED gldrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsCreationTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gldrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsId :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsId = Lens.field @"id"
{-# DEPRECATED gldrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsLastUpdatedTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED gldrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsLatestVersion :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED gldrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsLatestVersionArn :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED gldrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsName :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe Core.Text)
gldrrsName = Lens.field @"name"
{-# DEPRECATED gldrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsTags :: Lens.Lens' GetLoggerDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gldrrsTags = Lens.field @"tags"
{-# DEPRECATED gldrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsResponseStatus :: Lens.Lens' GetLoggerDefinitionResponse Core.Int
gldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
