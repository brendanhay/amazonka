{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DeleteApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of applications and their associations with configuration items.
module Network.AWS.Discovery.DeleteApplications
    (
    -- * Creating a request
      DeleteApplications (..)
    , mkDeleteApplications
    -- ** Request lenses
    , daConfigurationIds

    -- * Destructuring the response
    , DeleteApplicationsResponse (..)
    , mkDeleteApplicationsResponse
    -- ** Response lenses
    , darrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApplications' smart constructor.
newtype DeleteApplications = DeleteApplications'
  { configurationIds :: [Types.ApplicationId]
    -- ^ Configuration ID of an application to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplications' value with any optional fields omitted.
mkDeleteApplications
    :: DeleteApplications
mkDeleteApplications
  = DeleteApplications'{configurationIds = Core.mempty}

-- | Configuration ID of an application to be deleted.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daConfigurationIds :: Lens.Lens' DeleteApplications [Types.ApplicationId]
daConfigurationIds = Lens.field @"configurationIds"
{-# INLINEABLE daConfigurationIds #-}
{-# DEPRECATED configurationIds "Use generic-lens or generic-optics with 'configurationIds' instead"  #-}

instance Core.ToQuery DeleteApplications where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApplications where
        toHeaders DeleteApplications{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.DeleteApplications")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteApplications where
        toJSON DeleteApplications{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("configurationIds" Core..= configurationIds)])

instance Core.AWSRequest DeleteApplications where
        type Rs DeleteApplications = DeleteApplicationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteApplicationsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApplicationsResponse' smart constructor.
newtype DeleteApplicationsResponse = DeleteApplicationsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationsResponse' value with any optional fields omitted.
mkDeleteApplicationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteApplicationsResponse
mkDeleteApplicationsResponse responseStatus
  = DeleteApplicationsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteApplicationsResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
