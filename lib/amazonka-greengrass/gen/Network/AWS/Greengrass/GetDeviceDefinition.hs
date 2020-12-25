{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetDeviceDefinition (..),
    mkGetDeviceDefinition,

    -- ** Request lenses
    gddDeviceDefinitionId,

    -- * Destructuring the response
    GetDeviceDefinitionResponse (..),
    mkGetDeviceDefinitionResponse,

    -- ** Response lenses
    gddrrsArn,
    gddrrsCreationTimestamp,
    gddrrsId,
    gddrrsLastUpdatedTimestamp,
    gddrrsLatestVersion,
    gddrrsLatestVersionArn,
    gddrrsName,
    gddrrsTags,
    gddrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDeviceDefinition' smart constructor.
newtype GetDeviceDefinition = GetDeviceDefinition'
  { -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeviceDefinition' value with any optional fields omitted.
mkGetDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Core.Text ->
  GetDeviceDefinition
mkGetDeviceDefinition deviceDefinitionId =
  GetDeviceDefinition' {deviceDefinitionId}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddDeviceDefinitionId :: Lens.Lens' GetDeviceDefinition Core.Text
gddDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# DEPRECATED gddDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

instance Core.AWSRequest GetDeviceDefinition where
  type Rs GetDeviceDefinition = GetDeviceDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/devices/"
                Core.<> (Core.toText deviceDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceDefinitionResponse'
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

-- | /See:/ 'mkGetDeviceDefinitionResponse' smart constructor.
data GetDeviceDefinitionResponse = GetDeviceDefinitionResponse'
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

-- | Creates a 'GetDeviceDefinitionResponse' value with any optional fields omitted.
mkGetDeviceDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDeviceDefinitionResponse
mkGetDeviceDefinitionResponse responseStatus =
  GetDeviceDefinitionResponse'
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
gddrrsArn :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsArn = Lens.field @"arn"
{-# DEPRECATED gddrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsCreationTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gddrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsId :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsId = Lens.field @"id"
{-# DEPRECATED gddrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsLastUpdatedTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED gddrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsLatestVersion :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED gddrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsLatestVersionArn :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED gddrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsName :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe Core.Text)
gddrrsName = Lens.field @"name"
{-# DEPRECATED gddrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsTags :: Lens.Lens' GetDeviceDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gddrrsTags = Lens.field @"tags"
{-# DEPRECATED gddrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsResponseStatus :: Lens.Lens' GetDeviceDefinitionResponse Core.Int
gddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
