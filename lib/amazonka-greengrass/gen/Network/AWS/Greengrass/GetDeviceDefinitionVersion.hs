{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetDeviceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a device definition version.
module Network.AWS.Greengrass.GetDeviceDefinitionVersion
  ( -- * Creating a request
    GetDeviceDefinitionVersion (..),
    mkGetDeviceDefinitionVersion,

    -- ** Request lenses
    gddvDeviceDefinitionVersionId,
    gddvDeviceDefinitionId,
    gddvNextToken,

    -- * Destructuring the response
    GetDeviceDefinitionVersionResponse (..),
    mkGetDeviceDefinitionVersionResponse,

    -- ** Response lenses
    gddvrrsArn,
    gddvrrsCreationTimestamp,
    gddvrrsDefinition,
    gddvrrsId,
    gddvrrsNextToken,
    gddvrrsVersion,
    gddvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDeviceDefinitionVersion' smart constructor.
data GetDeviceDefinitionVersion = GetDeviceDefinitionVersion'
  { -- | The ID of the device definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListDeviceDefinitionVersions'' requests. If the version is the last one that was associated with a device definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
    deviceDefinitionVersionId :: Core.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeviceDefinitionVersion' value with any optional fields omitted.
mkGetDeviceDefinitionVersion ::
  -- | 'deviceDefinitionVersionId'
  Core.Text ->
  -- | 'deviceDefinitionId'
  Core.Text ->
  GetDeviceDefinitionVersion
mkGetDeviceDefinitionVersion
  deviceDefinitionVersionId
  deviceDefinitionId =
    GetDeviceDefinitionVersion'
      { deviceDefinitionVersionId,
        deviceDefinitionId,
        nextToken = Core.Nothing
      }

-- | The ID of the device definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListDeviceDefinitionVersions'' requests. If the version is the last one that was associated with a device definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'deviceDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvDeviceDefinitionVersionId :: Lens.Lens' GetDeviceDefinitionVersion Core.Text
gddvDeviceDefinitionVersionId = Lens.field @"deviceDefinitionVersionId"
{-# DEPRECATED gddvDeviceDefinitionVersionId "Use generic-lens or generic-optics with 'deviceDefinitionVersionId' instead." #-}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvDeviceDefinitionId :: Lens.Lens' GetDeviceDefinitionVersion Core.Text
gddvDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# DEPRECATED gddvDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvNextToken :: Lens.Lens' GetDeviceDefinitionVersion (Core.Maybe Core.Text)
gddvNextToken = Lens.field @"nextToken"
{-# DEPRECATED gddvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetDeviceDefinitionVersion where
  type
    Rs GetDeviceDefinitionVersion =
      GetDeviceDefinitionVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/devices/"
                Core.<> (Core.toText deviceDefinitionId)
                Core.<> ("/versions/")
                Core.<> (Core.toText deviceDefinitionVersionId)
            ),
        Core._rqQuery = Core.toQueryValue "NextToken" Core.<$> nextToken,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceDefinitionVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Definition")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDeviceDefinitionVersionResponse' smart constructor.
data GetDeviceDefinitionVersionResponse = GetDeviceDefinitionVersionResponse'
  { -- | The ARN of the device definition version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the device definition version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | Information about the device definition version.
    definition :: Core.Maybe Types.DeviceDefinitionVersion,
    -- | The ID of the device definition version.
    id :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The version of the device definition version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeviceDefinitionVersionResponse' value with any optional fields omitted.
mkGetDeviceDefinitionVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDeviceDefinitionVersionResponse
mkGetDeviceDefinitionVersionResponse responseStatus =
  GetDeviceDefinitionVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      definition = Core.Nothing,
      id = Core.Nothing,
      nextToken = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the device definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrrsArn :: Lens.Lens' GetDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
gddvrrsArn = Lens.field @"arn"
{-# DEPRECATED gddvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the device definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrrsCreationTimestamp :: Lens.Lens' GetDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
gddvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gddvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | Information about the device definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrrsDefinition :: Lens.Lens' GetDeviceDefinitionVersionResponse (Core.Maybe Types.DeviceDefinitionVersion)
gddvrrsDefinition = Lens.field @"definition"
{-# DEPRECATED gddvrrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ID of the device definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrrsId :: Lens.Lens' GetDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
gddvrrsId = Lens.field @"id"
{-# DEPRECATED gddvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrrsNextToken :: Lens.Lens' GetDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
gddvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gddvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The version of the device definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrrsVersion :: Lens.Lens' GetDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
gddvrrsVersion = Lens.field @"version"
{-# DEPRECATED gddvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrrsResponseStatus :: Lens.Lens' GetDeviceDefinitionVersionResponse Core.Int
gddvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gddvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
