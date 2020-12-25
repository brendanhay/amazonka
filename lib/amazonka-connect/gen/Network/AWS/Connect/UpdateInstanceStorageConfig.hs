{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateInstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing configuration for a resource type. This API is idempotent.
module Network.AWS.Connect.UpdateInstanceStorageConfig
  ( -- * Creating a request
    UpdateInstanceStorageConfig (..),
    mkUpdateInstanceStorageConfig,

    -- ** Request lenses
    uiscInstanceId,
    uiscAssociationId,
    uiscResourceType,
    uiscStorageConfig,

    -- * Destructuring the response
    UpdateInstanceStorageConfigResponse (..),
    mkUpdateInstanceStorageConfigResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateInstanceStorageConfig' smart constructor.
data UpdateInstanceStorageConfig = UpdateInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Types.AssociationId,
    -- | A valid resource type.
    resourceType :: Types.InstanceStorageResourceType,
    storageConfig :: Types.InstanceStorageConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInstanceStorageConfig' value with any optional fields omitted.
mkUpdateInstanceStorageConfig ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'associationId'
  Types.AssociationId ->
  -- | 'resourceType'
  Types.InstanceStorageResourceType ->
  -- | 'storageConfig'
  Types.InstanceStorageConfig ->
  UpdateInstanceStorageConfig
mkUpdateInstanceStorageConfig
  instanceId
  associationId
  resourceType
  storageConfig =
    UpdateInstanceStorageConfig'
      { instanceId,
        associationId,
        resourceType,
        storageConfig
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiscInstanceId :: Lens.Lens' UpdateInstanceStorageConfig Types.InstanceId
uiscInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uiscInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiscAssociationId :: Lens.Lens' UpdateInstanceStorageConfig Types.AssociationId
uiscAssociationId = Lens.field @"associationId"
{-# DEPRECATED uiscAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiscResourceType :: Lens.Lens' UpdateInstanceStorageConfig Types.InstanceStorageResourceType
uiscResourceType = Lens.field @"resourceType"
{-# DEPRECATED uiscResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'storageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiscStorageConfig :: Lens.Lens' UpdateInstanceStorageConfig Types.InstanceStorageConfig
uiscStorageConfig = Lens.field @"storageConfig"
{-# DEPRECATED uiscStorageConfig "Use generic-lens or generic-optics with 'storageConfig' instead." #-}

instance Core.FromJSON UpdateInstanceStorageConfig where
  toJSON UpdateInstanceStorageConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("StorageConfig" Core..= storageConfig)]
      )

instance Core.AWSRequest UpdateInstanceStorageConfig where
  type
    Rs UpdateInstanceStorageConfig =
      UpdateInstanceStorageConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/storage-config/")
                Core.<> (Core.toText associationId)
            ),
        Core._rqQuery = Core.toQueryValue "resourceType" resourceType,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull UpdateInstanceStorageConfigResponse'

-- | /See:/ 'mkUpdateInstanceStorageConfigResponse' smart constructor.
data UpdateInstanceStorageConfigResponse = UpdateInstanceStorageConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInstanceStorageConfigResponse' value with any optional fields omitted.
mkUpdateInstanceStorageConfigResponse ::
  UpdateInstanceStorageConfigResponse
mkUpdateInstanceStorageConfigResponse =
  UpdateInstanceStorageConfigResponse'
