{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateInstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the storage type configurations for the specified resource type and association ID.
module Network.AWS.Connect.DisassociateInstanceStorageConfig
  ( -- * Creating a request
    DisassociateInstanceStorageConfig (..),
    mkDisassociateInstanceStorageConfig,

    -- ** Request lenses
    discfInstanceId,
    discfAssociationId,
    discfResourceType,

    -- * Destructuring the response
    DisassociateInstanceStorageConfigResponse (..),
    mkDisassociateInstanceStorageConfigResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateInstanceStorageConfig' smart constructor.
data DisassociateInstanceStorageConfig = DisassociateInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Types.AssociationId,
    -- | A valid resource type.
    resourceType :: Types.InstanceStorageResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateInstanceStorageConfig' value with any optional fields omitted.
mkDisassociateInstanceStorageConfig ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'associationId'
  Types.AssociationId ->
  -- | 'resourceType'
  Types.InstanceStorageResourceType ->
  DisassociateInstanceStorageConfig
mkDisassociateInstanceStorageConfig
  instanceId
  associationId
  resourceType =
    DisassociateInstanceStorageConfig'
      { instanceId,
        associationId,
        resourceType
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discfInstanceId :: Lens.Lens' DisassociateInstanceStorageConfig Types.InstanceId
discfInstanceId = Lens.field @"instanceId"
{-# DEPRECATED discfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discfAssociationId :: Lens.Lens' DisassociateInstanceStorageConfig Types.AssociationId
discfAssociationId = Lens.field @"associationId"
{-# DEPRECATED discfAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discfResourceType :: Lens.Lens' DisassociateInstanceStorageConfig Types.InstanceStorageResourceType
discfResourceType = Lens.field @"resourceType"
{-# DEPRECATED discfResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.AWSRequest DisassociateInstanceStorageConfig where
  type
    Rs DisassociateInstanceStorageConfig =
      DisassociateInstanceStorageConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
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
    Response.receiveNull DisassociateInstanceStorageConfigResponse'

-- | /See:/ 'mkDisassociateInstanceStorageConfigResponse' smart constructor.
data DisassociateInstanceStorageConfigResponse = DisassociateInstanceStorageConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateInstanceStorageConfigResponse' value with any optional fields omitted.
mkDisassociateInstanceStorageConfigResponse ::
  DisassociateInstanceStorageConfigResponse
mkDisassociateInstanceStorageConfigResponse =
  DisassociateInstanceStorageConfigResponse'
