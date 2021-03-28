{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records the configuration state for the resource provided in the request. The configuration state of a resource is represented in AWS Config as Configuration Items. Once this API records the configuration item, you can retrieve the list of configuration items for the custom resource type using existing AWS Config APIs. 
module Network.AWS.Config.PutResourceConfig
    (
    -- * Creating a request
      PutResourceConfig (..)
    , mkPutResourceConfig
    -- ** Request lenses
    , prcResourceType
    , prcSchemaVersionId
    , prcResourceId
    , prcConfiguration
    , prcResourceName
    , prcTags

    -- * Destructuring the response
    , PutResourceConfigResponse (..)
    , mkPutResourceConfigResponse
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutResourceConfig' smart constructor.
data PutResourceConfig = PutResourceConfig'
  { resourceType :: Types.ResourceTypeString
    -- ^ The type of the resource. The custom resource type must be registered with AWS CloudFormation. 
  , schemaVersionId :: Types.SchemaVersionId
    -- ^ Version of the schema registered for the ResourceType in AWS CloudFormation.
  , resourceId :: Types.ResourceId
    -- ^ Unique identifier of the resource.
  , configuration :: Types.Configuration
    -- ^ The configuration object of the resource in valid JSON format. It must match the schema registered with AWS CloudFormation.
  , resourceName :: Core.Maybe Types.ResourceName
    -- ^ Name of the resource.
  , tags :: Core.Maybe (Core.HashMap Types.Name Types.Value)
    -- ^ Tags associated with the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourceConfig' value with any optional fields omitted.
mkPutResourceConfig
    :: Types.ResourceTypeString -- ^ 'resourceType'
    -> Types.SchemaVersionId -- ^ 'schemaVersionId'
    -> Types.ResourceId -- ^ 'resourceId'
    -> Types.Configuration -- ^ 'configuration'
    -> PutResourceConfig
mkPutResourceConfig resourceType schemaVersionId resourceId
  configuration
  = PutResourceConfig'{resourceType, schemaVersionId, resourceId,
                       configuration, resourceName = Core.Nothing, tags = Core.Nothing}

-- | The type of the resource. The custom resource type must be registered with AWS CloudFormation. 
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcResourceType :: Lens.Lens' PutResourceConfig Types.ResourceTypeString
prcResourceType = Lens.field @"resourceType"
{-# INLINEABLE prcResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Version of the schema registered for the ResourceType in AWS CloudFormation.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcSchemaVersionId :: Lens.Lens' PutResourceConfig Types.SchemaVersionId
prcSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE prcSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | Unique identifier of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcResourceId :: Lens.Lens' PutResourceConfig Types.ResourceId
prcResourceId = Lens.field @"resourceId"
{-# INLINEABLE prcResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The configuration object of the resource in valid JSON format. It must match the schema registered with AWS CloudFormation.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcConfiguration :: Lens.Lens' PutResourceConfig Types.Configuration
prcConfiguration = Lens.field @"configuration"
{-# INLINEABLE prcConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | Name of the resource.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcResourceName :: Lens.Lens' PutResourceConfig (Core.Maybe Types.ResourceName)
prcResourceName = Lens.field @"resourceName"
{-# INLINEABLE prcResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | Tags associated with the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcTags :: Lens.Lens' PutResourceConfig (Core.Maybe (Core.HashMap Types.Name Types.Value))
prcTags = Lens.field @"tags"
{-# INLINEABLE prcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery PutResourceConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutResourceConfig where
        toHeaders PutResourceConfig{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.PutResourceConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutResourceConfig where
        toJSON PutResourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceType" Core..= resourceType),
                  Core.Just ("SchemaVersionId" Core..= schemaVersionId),
                  Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("Configuration" Core..= configuration),
                  ("ResourceName" Core..=) Core.<$> resourceName,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest PutResourceConfig where
        type Rs PutResourceConfig = PutResourceConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutResourceConfigResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutResourceConfigResponse' smart constructor.
data PutResourceConfigResponse = PutResourceConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourceConfigResponse' value with any optional fields omitted.
mkPutResourceConfigResponse
    :: PutResourceConfigResponse
mkPutResourceConfigResponse = PutResourceConfigResponse'
