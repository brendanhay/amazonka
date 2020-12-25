{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateDeviceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a device definition that has already been defined.
module Network.AWS.Greengrass.CreateDeviceDefinitionVersion
  ( -- * Creating a request
    CreateDeviceDefinitionVersion (..),
    mkCreateDeviceDefinitionVersion,

    -- ** Request lenses
    cddvDeviceDefinitionId,
    cddvAmznClientToken,
    cddvDevices,

    -- * Destructuring the response
    CreateDeviceDefinitionVersionResponse (..),
    mkCreateDeviceDefinitionVersionResponse,

    -- ** Response lenses
    cddvrrsArn,
    cddvrrsCreationTimestamp,
    cddvrrsId,
    cddvrrsVersion,
    cddvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDeviceDefinitionVersion' smart constructor.
data CreateDeviceDefinitionVersion = CreateDeviceDefinitionVersion'
  { -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | A list of devices in the definition version.
    devices :: Core.Maybe [Types.Device]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeviceDefinitionVersion' value with any optional fields omitted.
mkCreateDeviceDefinitionVersion ::
  -- | 'deviceDefinitionId'
  Core.Text ->
  CreateDeviceDefinitionVersion
mkCreateDeviceDefinitionVersion deviceDefinitionId =
  CreateDeviceDefinitionVersion'
    { deviceDefinitionId,
      amznClientToken = Core.Nothing,
      devices = Core.Nothing
    }

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvDeviceDefinitionId :: Lens.Lens' CreateDeviceDefinitionVersion Core.Text
cddvDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# DEPRECATED cddvDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvAmznClientToken :: Lens.Lens' CreateDeviceDefinitionVersion (Core.Maybe Core.Text)
cddvAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED cddvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | A list of devices in the definition version.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvDevices :: Lens.Lens' CreateDeviceDefinitionVersion (Core.Maybe [Types.Device])
cddvDevices = Lens.field @"devices"
{-# DEPRECATED cddvDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

instance Core.FromJSON CreateDeviceDefinitionVersion where
  toJSON CreateDeviceDefinitionVersion {..} =
    Core.object
      (Core.catMaybes [("Devices" Core..=) Core.<$> devices])

instance Core.AWSRequest CreateDeviceDefinitionVersion where
  type
    Rs CreateDeviceDefinitionVersion =
      CreateDeviceDefinitionVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/devices/"
                Core.<> (Core.toText deviceDefinitionId)
                Core.<> ("/versions")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeviceDefinitionVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDeviceDefinitionVersionResponse' smart constructor.
data CreateDeviceDefinitionVersionResponse = CreateDeviceDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeviceDefinitionVersionResponse' value with any optional fields omitted.
mkCreateDeviceDefinitionVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDeviceDefinitionVersionResponse
mkCreateDeviceDefinitionVersionResponse responseStatus =
  CreateDeviceDefinitionVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrrsArn :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
cddvrrsArn = Lens.field @"arn"
{-# DEPRECATED cddvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrrsCreationTimestamp :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
cddvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED cddvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrrsId :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
cddvrrsId = Lens.field @"id"
{-# DEPRECATED cddvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrrsVersion :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Core.Maybe Core.Text)
cddvrrsVersion = Lens.field @"version"
{-# DEPRECATED cddvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrrsResponseStatus :: Lens.Lens' CreateDeviceDefinitionVersionResponse Core.Int
cddvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cddvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
