{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata about an application.
module Network.AWS.Discovery.UpdateApplication
    (
    -- * Creating a request
      UpdateApplication (..)
    , mkUpdateApplication
    -- ** Request lenses
    , uaConfigurationId
    , uaDescription
    , uaName

    -- * Destructuring the response
    , UpdateApplicationResponse (..)
    , mkUpdateApplicationResponse
    -- ** Response lenses
    , uarrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { configurationId :: Types.ConfigurationId
    -- ^ Configuration ID of the application to be updated.
  , description :: Core.Maybe Core.Text
    -- ^ New description of the application to be updated.
  , name :: Core.Maybe Core.Text
    -- ^ New name of the application to be updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplication' value with any optional fields omitted.
mkUpdateApplication
    :: Types.ConfigurationId -- ^ 'configurationId'
    -> UpdateApplication
mkUpdateApplication configurationId
  = UpdateApplication'{configurationId, description = Core.Nothing,
                       name = Core.Nothing}

-- | Configuration ID of the application to be updated.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaConfigurationId :: Lens.Lens' UpdateApplication Types.ConfigurationId
uaConfigurationId = Lens.field @"configurationId"
{-# INLINEABLE uaConfigurationId #-}
{-# DEPRECATED configurationId "Use generic-lens or generic-optics with 'configurationId' instead"  #-}

-- | New description of the application to be updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
uaDescription = Lens.field @"description"
{-# INLINEABLE uaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | New name of the application to be updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
uaName = Lens.field @"name"
{-# INLINEABLE uaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApplication where
        toHeaders UpdateApplication{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.UpdateApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApplication where
        toJSON UpdateApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("configurationId" Core..= configurationId),
                  ("description" Core..=) Core.<$> description,
                  ("name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateApplication where
        type Rs UpdateApplication = UpdateApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateApplicationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApplicationResponse' smart constructor.
newtype UpdateApplicationResponse = UpdateApplicationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationResponse' value with any optional fields omitted.
mkUpdateApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateApplicationResponse
mkUpdateApplicationResponse responseStatus
  = UpdateApplicationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateApplicationResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
