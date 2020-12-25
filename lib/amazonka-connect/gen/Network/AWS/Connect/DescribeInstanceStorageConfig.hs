{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeInstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current storage configurations for the specified resource type, association ID, and instance ID.
module Network.AWS.Connect.DescribeInstanceStorageConfig
  ( -- * Creating a request
    DescribeInstanceStorageConfig (..),
    mkDescribeInstanceStorageConfig,

    -- ** Request lenses
    discInstanceId,
    discAssociationId,
    discResourceType,

    -- * Destructuring the response
    DescribeInstanceStorageConfigResponse (..),
    mkDescribeInstanceStorageConfigResponse,

    -- ** Response lenses
    discrrsStorageConfig,
    discrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstanceStorageConfig' smart constructor.
data DescribeInstanceStorageConfig = DescribeInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Types.AssociationId,
    -- | A valid resource type.
    resourceType :: Types.InstanceStorageResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceStorageConfig' value with any optional fields omitted.
mkDescribeInstanceStorageConfig ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'associationId'
  Types.AssociationId ->
  -- | 'resourceType'
  Types.InstanceStorageResourceType ->
  DescribeInstanceStorageConfig
mkDescribeInstanceStorageConfig
  instanceId
  associationId
  resourceType =
    DescribeInstanceStorageConfig'
      { instanceId,
        associationId,
        resourceType
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discInstanceId :: Lens.Lens' DescribeInstanceStorageConfig Types.InstanceId
discInstanceId = Lens.field @"instanceId"
{-# DEPRECATED discInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discAssociationId :: Lens.Lens' DescribeInstanceStorageConfig Types.AssociationId
discAssociationId = Lens.field @"associationId"
{-# DEPRECATED discAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discResourceType :: Lens.Lens' DescribeInstanceStorageConfig Types.InstanceStorageResourceType
discResourceType = Lens.field @"resourceType"
{-# DEPRECATED discResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.AWSRequest DescribeInstanceStorageConfig where
  type
    Rs DescribeInstanceStorageConfig =
      DescribeInstanceStorageConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/storage-config/")
                Core.<> (Core.toText associationId)
            ),
        Core._rqQuery = Core.toQueryValue "resourceType" resourceType,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceStorageConfigResponse'
            Core.<$> (x Core..:? "StorageConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeInstanceStorageConfigResponse' smart constructor.
data DescribeInstanceStorageConfigResponse = DescribeInstanceStorageConfigResponse'
  { -- | A valid storage type.
    storageConfig :: Core.Maybe Types.InstanceStorageConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceStorageConfigResponse' value with any optional fields omitted.
mkDescribeInstanceStorageConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceStorageConfigResponse
mkDescribeInstanceStorageConfigResponse responseStatus =
  DescribeInstanceStorageConfigResponse'
    { storageConfig =
        Core.Nothing,
      responseStatus
    }

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discrrsStorageConfig :: Lens.Lens' DescribeInstanceStorageConfigResponse (Core.Maybe Types.InstanceStorageConfig)
discrrsStorageConfig = Lens.field @"storageConfig"
{-# DEPRECATED discrrsStorageConfig "Use generic-lens or generic-optics with 'storageConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discrrsResponseStatus :: Lens.Lens' DescribeInstanceStorageConfigResponse Core.Int
discrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED discrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
