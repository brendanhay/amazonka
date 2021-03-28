{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeInstanceStorageConfig (..)
    , mkDescribeInstanceStorageConfig
    -- ** Request lenses
    , discInstanceId
    , discAssociationId
    , discResourceType

    -- * Destructuring the response
    , DescribeInstanceStorageConfigResponse (..)
    , mkDescribeInstanceStorageConfigResponse
    -- ** Response lenses
    , discrrsStorageConfig
    , discrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstanceStorageConfig' smart constructor.
data DescribeInstanceStorageConfig = DescribeInstanceStorageConfig'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , associationId :: Types.AssociationId
    -- ^ The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
  , resourceType :: Types.InstanceStorageResourceType
    -- ^ A valid resource type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceStorageConfig' value with any optional fields omitted.
mkDescribeInstanceStorageConfig
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.AssociationId -- ^ 'associationId'
    -> Types.InstanceStorageResourceType -- ^ 'resourceType'
    -> DescribeInstanceStorageConfig
mkDescribeInstanceStorageConfig instanceId associationId
  resourceType
  = DescribeInstanceStorageConfig'{instanceId, associationId,
                                   resourceType}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discInstanceId :: Lens.Lens' DescribeInstanceStorageConfig Types.InstanceId
discInstanceId = Lens.field @"instanceId"
{-# INLINEABLE discInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discAssociationId :: Lens.Lens' DescribeInstanceStorageConfig Types.AssociationId
discAssociationId = Lens.field @"associationId"
{-# INLINEABLE discAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discResourceType :: Lens.Lens' DescribeInstanceStorageConfig Types.InstanceStorageResourceType
discResourceType = Lens.field @"resourceType"
{-# INLINEABLE discResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.ToQuery DescribeInstanceStorageConfig where
        toQuery DescribeInstanceStorageConfig{..}
          = Core.toQueryPair "resourceType" resourceType

instance Core.ToHeaders DescribeInstanceStorageConfig where
        toHeaders DescribeInstanceStorageConfig{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeInstanceStorageConfig where
        type Rs DescribeInstanceStorageConfig =
             DescribeInstanceStorageConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<>
                             "/storage-config/"
                             Core.<> Core.toText associationId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInstanceStorageConfigResponse' Core.<$>
                   (x Core..:? "StorageConfig") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeInstanceStorageConfigResponse' smart constructor.
data DescribeInstanceStorageConfigResponse = DescribeInstanceStorageConfigResponse'
  { storageConfig :: Core.Maybe Types.InstanceStorageConfig
    -- ^ A valid storage type.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceStorageConfigResponse' value with any optional fields omitted.
mkDescribeInstanceStorageConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstanceStorageConfigResponse
mkDescribeInstanceStorageConfigResponse responseStatus
  = DescribeInstanceStorageConfigResponse'{storageConfig =
                                             Core.Nothing,
                                           responseStatus}

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discrrsStorageConfig :: Lens.Lens' DescribeInstanceStorageConfigResponse (Core.Maybe Types.InstanceStorageConfig)
discrrsStorageConfig = Lens.field @"storageConfig"
{-# INLINEABLE discrrsStorageConfig #-}
{-# DEPRECATED storageConfig "Use generic-lens or generic-optics with 'storageConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discrrsResponseStatus :: Lens.Lens' DescribeInstanceStorageConfigResponse Core.Int
discrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE discrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
