{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.BatchGetResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current configuration for one or more requested resources. The operation also returns a list of resources that are not processed in the current request. If there are no unprocessed resources, the operation returns an empty unprocessedResourceKeys list.
module Network.AWS.Config.BatchGetResourceConfig
  ( -- * Creating a request
    BatchGetResourceConfig (..),
    mkBatchGetResourceConfig,

    -- ** Request lenses
    bgrcResourceKeys,

    -- * Destructuring the response
    BatchGetResourceConfigResponse (..),
    mkBatchGetResourceConfigResponse,

    -- ** Response lenses
    bgrcrrsBaseConfigurationItems,
    bgrcrrsUnprocessedResourceKeys,
    bgrcrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetResourceConfig' smart constructor.
newtype BatchGetResourceConfig = BatchGetResourceConfig'
  { -- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
    resourceKeys :: Core.NonEmpty Types.ResourceKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetResourceConfig' value with any optional fields omitted.
mkBatchGetResourceConfig ::
  -- | 'resourceKeys'
  Core.NonEmpty Types.ResourceKey ->
  BatchGetResourceConfig
mkBatchGetResourceConfig resourceKeys =
  BatchGetResourceConfig' {resourceKeys}

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrcResourceKeys :: Lens.Lens' BatchGetResourceConfig (Core.NonEmpty Types.ResourceKey)
bgrcResourceKeys = Lens.field @"resourceKeys"
{-# DEPRECATED bgrcResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

instance Core.FromJSON BatchGetResourceConfig where
  toJSON BatchGetResourceConfig {..} =
    Core.object
      (Core.catMaybes [Core.Just ("resourceKeys" Core..= resourceKeys)])

instance Core.AWSRequest BatchGetResourceConfig where
  type Rs BatchGetResourceConfig = BatchGetResourceConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.BatchGetResourceConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetResourceConfigResponse'
            Core.<$> (x Core..:? "baseConfigurationItems")
            Core.<*> (x Core..:? "unprocessedResourceKeys")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchGetResourceConfigResponse' smart constructor.
data BatchGetResourceConfigResponse = BatchGetResourceConfigResponse'
  { -- | A list that contains the current configuration of one or more resources.
    baseConfigurationItems :: Core.Maybe [Types.BaseConfigurationItem],
    -- | A list of resource keys that were not processed with the current response. The unprocessesResourceKeys value is in the same form as ResourceKeys, so the value can be directly provided to a subsequent BatchGetResourceConfig operation. If there are no unprocessed resource keys, the response contains an empty unprocessedResourceKeys list.
    unprocessedResourceKeys :: Core.Maybe (Core.NonEmpty Types.ResourceKey),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetResourceConfigResponse' value with any optional fields omitted.
mkBatchGetResourceConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetResourceConfigResponse
mkBatchGetResourceConfigResponse responseStatus =
  BatchGetResourceConfigResponse'
    { baseConfigurationItems =
        Core.Nothing,
      unprocessedResourceKeys = Core.Nothing,
      responseStatus
    }

-- | A list that contains the current configuration of one or more resources.
--
-- /Note:/ Consider using 'baseConfigurationItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrcrrsBaseConfigurationItems :: Lens.Lens' BatchGetResourceConfigResponse (Core.Maybe [Types.BaseConfigurationItem])
bgrcrrsBaseConfigurationItems = Lens.field @"baseConfigurationItems"
{-# DEPRECATED bgrcrrsBaseConfigurationItems "Use generic-lens or generic-optics with 'baseConfigurationItems' instead." #-}

-- | A list of resource keys that were not processed with the current response. The unprocessesResourceKeys value is in the same form as ResourceKeys, so the value can be directly provided to a subsequent BatchGetResourceConfig operation. If there are no unprocessed resource keys, the response contains an empty unprocessedResourceKeys list.
--
-- /Note:/ Consider using 'unprocessedResourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrcrrsUnprocessedResourceKeys :: Lens.Lens' BatchGetResourceConfigResponse (Core.Maybe (Core.NonEmpty Types.ResourceKey))
bgrcrrsUnprocessedResourceKeys = Lens.field @"unprocessedResourceKeys"
{-# DEPRECATED bgrcrrsUnprocessedResourceKeys "Use generic-lens or generic-optics with 'unprocessedResourceKeys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrcrrsResponseStatus :: Lens.Lens' BatchGetResourceConfigResponse Core.Int
bgrcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgrcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
