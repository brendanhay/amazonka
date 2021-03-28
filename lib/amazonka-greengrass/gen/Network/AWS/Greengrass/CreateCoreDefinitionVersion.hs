{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateCoreDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a core definition that has already been defined. Greengrass groups must each contain exactly one Greengrass core.
module Network.AWS.Greengrass.CreateCoreDefinitionVersion
    (
    -- * Creating a request
      CreateCoreDefinitionVersion (..)
    , mkCreateCoreDefinitionVersion
    -- ** Request lenses
    , ccdvfCoreDefinitionId
    , ccdvfAmznClientToken
    , ccdvfCores

    -- * Destructuring the response
    , CreateCoreDefinitionVersionResponse (..)
    , mkCreateCoreDefinitionVersionResponse
    -- ** Response lenses
    , crsArn
    , crsCreationTimestamp
    , crsId
    , crsVersion
    , crsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCoreDefinitionVersion' smart constructor.
data CreateCoreDefinitionVersion = CreateCoreDefinitionVersion'
  { coreDefinitionId :: Core.Text
    -- ^ The ID of the core definition.
  , amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , cores :: Core.Maybe [Types.Core]
    -- ^ A list of cores in the core definition version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCoreDefinitionVersion' value with any optional fields omitted.
mkCreateCoreDefinitionVersion
    :: Core.Text -- ^ 'coreDefinitionId'
    -> CreateCoreDefinitionVersion
mkCreateCoreDefinitionVersion coreDefinitionId
  = CreateCoreDefinitionVersion'{coreDefinitionId,
                                 amznClientToken = Core.Nothing, cores = Core.Nothing}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvfCoreDefinitionId :: Lens.Lens' CreateCoreDefinitionVersion Core.Text
ccdvfCoreDefinitionId = Lens.field @"coreDefinitionId"
{-# INLINEABLE ccdvfCoreDefinitionId #-}
{-# DEPRECATED coreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead"  #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvfAmznClientToken :: Lens.Lens' CreateCoreDefinitionVersion (Core.Maybe Core.Text)
ccdvfAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE ccdvfAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | A list of cores in the core definition version.
--
-- /Note:/ Consider using 'cores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvfCores :: Lens.Lens' CreateCoreDefinitionVersion (Core.Maybe [Types.Core])
ccdvfCores = Lens.field @"cores"
{-# INLINEABLE ccdvfCores #-}
{-# DEPRECATED cores "Use generic-lens or generic-optics with 'cores' instead"  #-}

instance Core.ToQuery CreateCoreDefinitionVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCoreDefinitionVersion where
        toHeaders CreateCoreDefinitionVersion{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCoreDefinitionVersion where
        toJSON CreateCoreDefinitionVersion{..}
          = Core.object (Core.catMaybes [("Cores" Core..=) Core.<$> cores])

instance Core.AWSRequest CreateCoreDefinitionVersion where
        type Rs CreateCoreDefinitionVersion =
             CreateCoreDefinitionVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/greengrass/definition/cores/" Core.<>
                             Core.toText coreDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCoreDefinitionVersionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "Version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCoreDefinitionVersionResponse' smart constructor.
data CreateCoreDefinitionVersionResponse = CreateCoreDefinitionVersionResponse'
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

-- | Creates a 'CreateCoreDefinitionVersionResponse' value with any optional fields omitted.
mkCreateCoreDefinitionVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCoreDefinitionVersionResponse
mkCreateCoreDefinitionVersionResponse responseStatus
  = CreateCoreDefinitionVersionResponse'{arn = Core.Nothing,
                                         creationTimestamp = Core.Nothing, id = Core.Nothing,
                                         version = Core.Nothing, responseStatus}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsArn :: Lens.Lens' CreateCoreDefinitionVersionResponse (Core.Maybe Core.Text)
crsArn = Lens.field @"arn"
{-# INLINEABLE crsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCreationTimestamp :: Lens.Lens' CreateCoreDefinitionVersionResponse (Core.Maybe Core.Text)
crsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE crsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsId :: Lens.Lens' CreateCoreDefinitionVersionResponse (Core.Maybe Core.Text)
crsId = Lens.field @"id"
{-# INLINEABLE crsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsVersion :: Lens.Lens' CreateCoreDefinitionVersionResponse (Core.Maybe Core.Text)
crsVersion = Lens.field @"version"
{-# INLINEABLE crsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateCoreDefinitionVersionResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
