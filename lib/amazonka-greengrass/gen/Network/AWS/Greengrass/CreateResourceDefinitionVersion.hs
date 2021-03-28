{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateResourceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a resource definition that has already been defined.
module Network.AWS.Greengrass.CreateResourceDefinitionVersion
    (
    -- * Creating a request
      CreateResourceDefinitionVersion (..)
    , mkCreateResourceDefinitionVersion
    -- ** Request lenses
    , crdvResourceDefinitionId
    , crdvAmznClientToken
    , crdvResources

    -- * Destructuring the response
    , CreateResourceDefinitionVersionResponse (..)
    , mkCreateResourceDefinitionVersionResponse
    -- ** Response lenses
    , crdvrrsArn
    , crdvrrsCreationTimestamp
    , crdvrrsId
    , crdvrrsVersion
    , crdvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateResourceDefinitionVersion' smart constructor.
data CreateResourceDefinitionVersion = CreateResourceDefinitionVersion'
  { resourceDefinitionId :: Core.Text
    -- ^ The ID of the resource definition.
  , amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , resources :: Core.Maybe [Types.Resource]
    -- ^ A list of resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceDefinitionVersion' value with any optional fields omitted.
mkCreateResourceDefinitionVersion
    :: Core.Text -- ^ 'resourceDefinitionId'
    -> CreateResourceDefinitionVersion
mkCreateResourceDefinitionVersion resourceDefinitionId
  = CreateResourceDefinitionVersion'{resourceDefinitionId,
                                     amznClientToken = Core.Nothing, resources = Core.Nothing}

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvResourceDefinitionId :: Lens.Lens' CreateResourceDefinitionVersion Core.Text
crdvResourceDefinitionId = Lens.field @"resourceDefinitionId"
{-# INLINEABLE crdvResourceDefinitionId #-}
{-# DEPRECATED resourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead"  #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvAmznClientToken :: Lens.Lens' CreateResourceDefinitionVersion (Core.Maybe Core.Text)
crdvAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE crdvAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | A list of resources.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvResources :: Lens.Lens' CreateResourceDefinitionVersion (Core.Maybe [Types.Resource])
crdvResources = Lens.field @"resources"
{-# INLINEABLE crdvResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

instance Core.ToQuery CreateResourceDefinitionVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateResourceDefinitionVersion where
        toHeaders CreateResourceDefinitionVersion{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateResourceDefinitionVersion where
        toJSON CreateResourceDefinitionVersion{..}
          = Core.object
              (Core.catMaybes [("Resources" Core..=) Core.<$> resources])

instance Core.AWSRequest CreateResourceDefinitionVersion where
        type Rs CreateResourceDefinitionVersion =
             CreateResourceDefinitionVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/greengrass/definition/resources/" Core.<>
                             Core.toText resourceDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateResourceDefinitionVersionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "Version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateResourceDefinitionVersionResponse' smart constructor.
data CreateResourceDefinitionVersionResponse = CreateResourceDefinitionVersionResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the version.
  , creationTimestamp :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the version was created.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the parent definition that the version is associated with.
  , version :: Core.Maybe Core.Text
    -- ^ The ID of the version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceDefinitionVersionResponse' value with any optional fields omitted.
mkCreateResourceDefinitionVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateResourceDefinitionVersionResponse
mkCreateResourceDefinitionVersionResponse responseStatus
  = CreateResourceDefinitionVersionResponse'{arn = Core.Nothing,
                                             creationTimestamp = Core.Nothing, id = Core.Nothing,
                                             version = Core.Nothing, responseStatus}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrrsArn :: Lens.Lens' CreateResourceDefinitionVersionResponse (Core.Maybe Core.Text)
crdvrrsArn = Lens.field @"arn"
{-# INLINEABLE crdvrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrrsCreationTimestamp :: Lens.Lens' CreateResourceDefinitionVersionResponse (Core.Maybe Core.Text)
crdvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE crdvrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrrsId :: Lens.Lens' CreateResourceDefinitionVersionResponse (Core.Maybe Core.Text)
crdvrrsId = Lens.field @"id"
{-# INLINEABLE crdvrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrrsVersion :: Lens.Lens' CreateResourceDefinitionVersionResponse (Core.Maybe Core.Text)
crdvrrsVersion = Lens.field @"version"
{-# INLINEABLE crdvrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrrsResponseStatus :: Lens.Lens' CreateResourceDefinitionVersionResponse Core.Int
crdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
