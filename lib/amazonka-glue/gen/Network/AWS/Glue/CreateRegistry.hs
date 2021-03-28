{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new registry which may be used to hold a collection of schemas.
module Network.AWS.Glue.CreateRegistry
    (
    -- * Creating a request
      CreateRegistry (..)
    , mkCreateRegistry
    -- ** Request lenses
    , crRegistryName
    , crDescription
    , crTags

    -- * Destructuring the response
    , CreateRegistryResponse (..)
    , mkCreateRegistryResponse
    -- ** Response lenses
    , crrrsDescription
    , crrrsRegistryArn
    , crrrsRegistryName
    , crrrsTags
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRegistry' smart constructor.
data CreateRegistry = CreateRegistry'
  { registryName :: Types.RegistryName
    -- ^ Name of the registry to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the registry. If description is not provided, there will not be any default value for this.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ AWS tags that contain a key value pair and may be searched by console, command line, or API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRegistry' value with any optional fields omitted.
mkCreateRegistry
    :: Types.RegistryName -- ^ 'registryName'
    -> CreateRegistry
mkCreateRegistry registryName
  = CreateRegistry'{registryName, description = Core.Nothing,
                    tags = Core.Nothing}

-- | Name of the registry to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRegistryName :: Lens.Lens' CreateRegistry Types.RegistryName
crRegistryName = Lens.field @"registryName"
{-# INLINEABLE crRegistryName #-}
{-# DEPRECATED registryName "Use generic-lens or generic-optics with 'registryName' instead"  #-}

-- | A description of the registry. If description is not provided, there will not be any default value for this.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' CreateRegistry (Core.Maybe Types.Description)
crDescription = Lens.field @"description"
{-# INLINEABLE crDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | AWS tags that contain a key value pair and may be searched by console, command line, or API.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRegistry (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
crTags = Lens.field @"tags"
{-# INLINEABLE crTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateRegistry where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRegistry where
        toHeaders CreateRegistry{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CreateRegistry") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRegistry where
        toJSON CreateRegistry{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegistryName" Core..= registryName),
                  ("Description" Core..=) Core.<$> description,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateRegistry where
        type Rs CreateRegistry = CreateRegistryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRegistryResponse' Core.<$>
                   (x Core..:? "Description") Core.<*> x Core..:? "RegistryArn"
                     Core.<*> x Core..:? "RegistryName"
                     Core.<*> x Core..:? "Tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRegistryResponse' smart constructor.
data CreateRegistryResponse = CreateRegistryResponse'
  { description :: Core.Maybe Types.Description
    -- ^ A description of the registry.
  , registryArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) of the newly created registry.
  , registryName :: Core.Maybe Types.RegistryName
    -- ^ The name of the registry.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags for the registry.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRegistryResponse' value with any optional fields omitted.
mkCreateRegistryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRegistryResponse
mkCreateRegistryResponse responseStatus
  = CreateRegistryResponse'{description = Core.Nothing,
                            registryArn = Core.Nothing, registryName = Core.Nothing,
                            tags = Core.Nothing, responseStatus}

-- | A description of the registry.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsDescription :: Lens.Lens' CreateRegistryResponse (Core.Maybe Types.Description)
crrrsDescription = Lens.field @"description"
{-# INLINEABLE crrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The Amazon Resource Name (ARN) of the newly created registry.
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsRegistryArn :: Lens.Lens' CreateRegistryResponse (Core.Maybe Types.GlueResourceArn)
crrrsRegistryArn = Lens.field @"registryArn"
{-# INLINEABLE crrrsRegistryArn #-}
{-# DEPRECATED registryArn "Use generic-lens or generic-optics with 'registryArn' instead"  #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsRegistryName :: Lens.Lens' CreateRegistryResponse (Core.Maybe Types.RegistryName)
crrrsRegistryName = Lens.field @"registryName"
{-# INLINEABLE crrrsRegistryName #-}
{-# DEPRECATED registryName "Use generic-lens or generic-optics with 'registryName' instead"  #-}

-- | The tags for the registry.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsTags :: Lens.Lens' CreateRegistryResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
crrrsTags = Lens.field @"tags"
{-# INLINEABLE crrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRegistryResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
