{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddWorkingStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as working storage for a gateway. This operation is only supported in the stored volume gateway type. This operation is deprecated in cached volume API version 20120630. Use 'AddUploadBuffer' instead.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add working storage, and one or more disk IDs that you want to configure as working storage.
module Network.AWS.StorageGateway.AddWorkingStorage
  ( -- * Creating a request
    AddWorkingStorage (..),
    mkAddWorkingStorage,

    -- ** Request lenses
    awsGatewayARN,
    awsDiskIds,

    -- * Destructuring the response
    AddWorkingStorageResponse (..),
    mkAddWorkingStorageResponse,

    -- ** Response lenses
    awsrrsGatewayARN,
    awsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'AddWorkingStorageInput$DiskIds'
--
--
--
-- /See:/ 'mkAddWorkingStorage' smart constructor.
data AddWorkingStorage = AddWorkingStorage'
  { gatewayARN :: Types.GatewayARN,
    -- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
    diskIds :: [Types.DiskId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddWorkingStorage' value with any optional fields omitted.
mkAddWorkingStorage ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  AddWorkingStorage
mkAddWorkingStorage gatewayARN =
  AddWorkingStorage' {gatewayARN, diskIds = Core.mempty}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsGatewayARN :: Lens.Lens' AddWorkingStorage Types.GatewayARN
awsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED awsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsDiskIds :: Lens.Lens' AddWorkingStorage [Types.DiskId]
awsDiskIds = Lens.field @"diskIds"
{-# DEPRECATED awsDiskIds "Use generic-lens or generic-optics with 'diskIds' instead." #-}

instance Core.FromJSON AddWorkingStorage where
  toJSON AddWorkingStorage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("DiskIds" Core..= diskIds)
          ]
      )

instance Core.AWSRequest AddWorkingStorage where
  type Rs AddWorkingStorage = AddWorkingStorageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.AddWorkingStorage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AddWorkingStorageResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway for which working storage was configured.
--
-- /See:/ 'mkAddWorkingStorageResponse' smart constructor.
data AddWorkingStorageResponse = AddWorkingStorageResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddWorkingStorageResponse' value with any optional fields omitted.
mkAddWorkingStorageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddWorkingStorageResponse
mkAddWorkingStorageResponse responseStatus =
  AddWorkingStorageResponse'
    { gatewayARN = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsrrsGatewayARN :: Lens.Lens' AddWorkingStorageResponse (Core.Maybe Types.GatewayARN)
awsrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED awsrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsrrsResponseStatus :: Lens.Lens' AddWorkingStorageResponse Core.Int
awsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED awsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
