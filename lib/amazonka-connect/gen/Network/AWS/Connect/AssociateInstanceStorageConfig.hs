{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AssociateInstanceStorageConfig (..)
    , mkAssociateInstanceStorageConfig
    -- ** Request lenses
    , aiscInstanceId
    , aiscResourceType
    , aiscStorageConfig

    -- * Destructuring the response
    , AssociateInstanceStorageConfigResponse (..)
    , mkAssociateInstanceStorageConfigResponse
    -- ** Response lenses
    , aiscrrsAssociationId
    , aiscrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateInstanceStorageConfig' smart constructor.
data AssociateInstanceStorageConfig = AssociateInstanceStorageConfig'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , resourceType :: Types.InstanceStorageResourceType
    -- ^ A valid resource type.
  , storageConfig :: Types.InstanceStorageConfig
    -- ^ A valid storage type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateInstanceStorageConfig' value with any optional fields omitted.
mkAssociateInstanceStorageConfig
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.InstanceStorageResourceType -- ^ 'resourceType'
    -> Types.InstanceStorageConfig -- ^ 'storageConfig'
    -> AssociateInstanceStorageConfig
mkAssociateInstanceStorageConfig instanceId resourceType
  storageConfig
  = AssociateInstanceStorageConfig'{instanceId, resourceType,
                                    storageConfig}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscInstanceId :: Lens.Lens' AssociateInstanceStorageConfig Types.InstanceId
aiscInstanceId = Lens.field @"instanceId"
{-# INLINEABLE aiscInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscResourceType :: Lens.Lens' AssociateInstanceStorageConfig Types.InstanceStorageResourceType
aiscResourceType = Lens.field @"resourceType"
{-# INLINEABLE aiscResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscStorageConfig :: Lens.Lens' AssociateInstanceStorageConfig Types.InstanceStorageConfig
aiscStorageConfig = Lens.field @"storageConfig"
{-# INLINEABLE aiscStorageConfig #-}
{-# DEPRECATED storageConfig "Use generic-lens or generic-optics with 'storageConfig' instead"  #-}

instance Core.ToQuery AssociateInstanceStorageConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateInstanceStorageConfig where
        toHeaders AssociateInstanceStorageConfig{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateInstanceStorageConfig where
        toJSON AssociateInstanceStorageConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceType" Core..= resourceType),
                  Core.Just ("StorageConfig" Core..= storageConfig)])

instance Core.AWSRequest AssociateInstanceStorageConfig where
        type Rs AssociateInstanceStorageConfig =
             AssociateInstanceStorageConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<>
                             "/storage-config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AssociateInstanceStorageConfigResponse' Core.<$>
                   (x Core..:? "AssociationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateInstanceStorageConfigResponse' smart constructor.
data AssociateInstanceStorageConfigResponse = AssociateInstanceStorageConfigResponse'
  { associationId :: Core.Maybe Types.AssociationId
    -- ^ The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateInstanceStorageConfigResponse' value with any optional fields omitted.
mkAssociateInstanceStorageConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateInstanceStorageConfigResponse
mkAssociateInstanceStorageConfigResponse responseStatus
  = AssociateInstanceStorageConfigResponse'{associationId =
                                              Core.Nothing,
                                            responseStatus}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscrrsAssociationId :: Lens.Lens' AssociateInstanceStorageConfigResponse (Core.Maybe Types.AssociationId)
aiscrrsAssociationId = Lens.field @"associationId"
{-# INLINEABLE aiscrrsAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscrrsResponseStatus :: Lens.Lens' AssociateInstanceStorageConfigResponse Core.Int
aiscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aiscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
