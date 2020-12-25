{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateInstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a storage resource type for the first time. You can only associate one type of storage configuration in a single call. This means, for example, that you can't define an instance with multiple S3 buckets for storing chat transcripts.
--
-- This API does not create a resource that doesn't exist. It only associates it to the instance. Ensure that the resource being specified in the storage configuration, like an Amazon S3 bucket, exists when being used for association.
module Network.AWS.Connect.AssociateInstanceStorageConfig
  ( -- * Creating a request
    AssociateInstanceStorageConfig (..),
    mkAssociateInstanceStorageConfig,

    -- ** Request lenses
    aiscInstanceId,
    aiscResourceType,
    aiscStorageConfig,

    -- * Destructuring the response
    AssociateInstanceStorageConfigResponse (..),
    mkAssociateInstanceStorageConfigResponse,

    -- ** Response lenses
    aiscrrsAssociationId,
    aiscrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateInstanceStorageConfig' smart constructor.
data AssociateInstanceStorageConfig = AssociateInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | A valid resource type.
    resourceType :: Types.InstanceStorageResourceType,
    -- | A valid storage type.
    storageConfig :: Types.InstanceStorageConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateInstanceStorageConfig' value with any optional fields omitted.
mkAssociateInstanceStorageConfig ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'resourceType'
  Types.InstanceStorageResourceType ->
  -- | 'storageConfig'
  Types.InstanceStorageConfig ->
  AssociateInstanceStorageConfig
mkAssociateInstanceStorageConfig
  instanceId
  resourceType
  storageConfig =
    AssociateInstanceStorageConfig'
      { instanceId,
        resourceType,
        storageConfig
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscInstanceId :: Lens.Lens' AssociateInstanceStorageConfig Types.InstanceId
aiscInstanceId = Lens.field @"instanceId"
{-# DEPRECATED aiscInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscResourceType :: Lens.Lens' AssociateInstanceStorageConfig Types.InstanceStorageResourceType
aiscResourceType = Lens.field @"resourceType"
{-# DEPRECATED aiscResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscStorageConfig :: Lens.Lens' AssociateInstanceStorageConfig Types.InstanceStorageConfig
aiscStorageConfig = Lens.field @"storageConfig"
{-# DEPRECATED aiscStorageConfig "Use generic-lens or generic-optics with 'storageConfig' instead." #-}

instance Core.FromJSON AssociateInstanceStorageConfig where
  toJSON AssociateInstanceStorageConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceType" Core..= resourceType),
            Core.Just ("StorageConfig" Core..= storageConfig)
          ]
      )

instance Core.AWSRequest AssociateInstanceStorageConfig where
  type
    Rs AssociateInstanceStorageConfig =
      AssociateInstanceStorageConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/storage-config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateInstanceStorageConfigResponse'
            Core.<$> (x Core..:? "AssociationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateInstanceStorageConfigResponse' smart constructor.
data AssociateInstanceStorageConfigResponse = AssociateInstanceStorageConfigResponse'
  { -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Core.Maybe Types.AssociationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateInstanceStorageConfigResponse' value with any optional fields omitted.
mkAssociateInstanceStorageConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateInstanceStorageConfigResponse
mkAssociateInstanceStorageConfigResponse responseStatus =
  AssociateInstanceStorageConfigResponse'
    { associationId =
        Core.Nothing,
      responseStatus
    }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscrrsAssociationId :: Lens.Lens' AssociateInstanceStorageConfigResponse (Core.Maybe Types.AssociationId)
aiscrrsAssociationId = Lens.field @"associationId"
{-# DEPRECATED aiscrrsAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscrrsResponseStatus :: Lens.Lens' AssociateInstanceStorageConfigResponse Core.Int
aiscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aiscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
