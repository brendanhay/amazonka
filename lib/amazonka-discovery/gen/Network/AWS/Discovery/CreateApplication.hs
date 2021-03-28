{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.CreateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application with the given name and description.
module Network.AWS.Discovery.CreateApplication
    (
    -- * Creating a request
      CreateApplication (..)
    , mkCreateApplication
    -- ** Request lenses
    , caName
    , caDescription

    -- * Destructuring the response
    , CreateApplicationResponse (..)
    , mkCreateApplicationResponse
    -- ** Response lenses
    , carrsConfigurationId
    , carrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { name :: Core.Text
    -- ^ Name of the application to be created.
  , description :: Core.Maybe Core.Text
    -- ^ Description of the application to be created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplication' value with any optional fields omitted.
mkCreateApplication
    :: Core.Text -- ^ 'name'
    -> CreateApplication
mkCreateApplication name
  = CreateApplication'{name, description = Core.Nothing}

-- | Name of the application to be created.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateApplication Core.Text
caName = Lens.field @"name"
{-# INLINEABLE caName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Description of the application to be created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caDescription = Lens.field @"description"
{-# INLINEABLE caDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery CreateApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApplication where
        toHeaders CreateApplication{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.CreateApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateApplication where
        toJSON CreateApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("description" Core..=) Core.<$> description])

instance Core.AWSRequest CreateApplication where
        type Rs CreateApplication = CreateApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateApplicationResponse' Core.<$>
                   (x Core..:? "configurationId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { configurationId :: Core.Maybe Core.Text
    -- ^ Configuration ID of an application to be created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplicationResponse' value with any optional fields omitted.
mkCreateApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateApplicationResponse
mkCreateApplicationResponse responseStatus
  = CreateApplicationResponse'{configurationId = Core.Nothing,
                               responseStatus}

-- | Configuration ID of an application to be created.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsConfigurationId :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsConfigurationId = Lens.field @"configurationId"
{-# INLINEABLE carrsConfigurationId #-}
{-# DEPRECATED configurationId "Use generic-lens or generic-optics with 'configurationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateApplicationResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
