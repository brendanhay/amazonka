{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DisassociateInstanceStorageConfig (..)
    , mkDisassociateInstanceStorageConfig
    -- ** Request lenses
    , discfInstanceId
    , discfAssociationId
    , discfResourceType

    -- * Destructuring the response
    , DisassociateInstanceStorageConfigResponse (..)
    , mkDisassociateInstanceStorageConfigResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateInstanceStorageConfig' smart constructor.
data DisassociateInstanceStorageConfig = DisassociateInstanceStorageConfig'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , associationId :: Types.AssociationId
    -- ^ The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
  , resourceType :: Types.InstanceStorageResourceType
    -- ^ A valid resource type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateInstanceStorageConfig' value with any optional fields omitted.
mkDisassociateInstanceStorageConfig
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.AssociationId -- ^ 'associationId'
    -> Types.InstanceStorageResourceType -- ^ 'resourceType'
    -> DisassociateInstanceStorageConfig
mkDisassociateInstanceStorageConfig instanceId associationId
  resourceType
  = DisassociateInstanceStorageConfig'{instanceId, associationId,
                                       resourceType}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discfInstanceId :: Lens.Lens' DisassociateInstanceStorageConfig Types.InstanceId
discfInstanceId = Lens.field @"instanceId"
{-# INLINEABLE discfInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discfAssociationId :: Lens.Lens' DisassociateInstanceStorageConfig Types.AssociationId
discfAssociationId = Lens.field @"associationId"
{-# INLINEABLE discfAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discfResourceType :: Lens.Lens' DisassociateInstanceStorageConfig Types.InstanceStorageResourceType
discfResourceType = Lens.field @"resourceType"
{-# INLINEABLE discfResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.ToQuery DisassociateInstanceStorageConfig where
        toQuery DisassociateInstanceStorageConfig{..}
          = Core.toQueryPair "resourceType" resourceType

instance Core.ToHeaders DisassociateInstanceStorageConfig where
        toHeaders DisassociateInstanceStorageConfig{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DisassociateInstanceStorageConfig where
        type Rs DisassociateInstanceStorageConfig =
             DisassociateInstanceStorageConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<>
                             "/storage-config/"
                             Core.<> Core.toText associationId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DisassociateInstanceStorageConfigResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateInstanceStorageConfigResponse' smart constructor.
data DisassociateInstanceStorageConfigResponse = DisassociateInstanceStorageConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateInstanceStorageConfigResponse' value with any optional fields omitted.
mkDisassociateInstanceStorageConfigResponse
    :: DisassociateInstanceStorageConfigResponse
mkDisassociateInstanceStorageConfigResponse
  = DisassociateInstanceStorageConfigResponse'
