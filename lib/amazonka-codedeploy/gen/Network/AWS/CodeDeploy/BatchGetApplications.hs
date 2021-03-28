{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more applications. The maximum number of applications that can be returned is 100.
module Network.AWS.CodeDeploy.BatchGetApplications
    (
    -- * Creating a request
      BatchGetApplications (..)
    , mkBatchGetApplications
    -- ** Request lenses
    , bgaApplicationNames

    -- * Destructuring the response
    , BatchGetApplicationsResponse (..)
    , mkBatchGetApplicationsResponse
    -- ** Response lenses
    , bgarrsApplicationsInfo
    , bgarrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetApplications@ operation.
--
-- /See:/ 'mkBatchGetApplications' smart constructor.
newtype BatchGetApplications = BatchGetApplications'
  { applicationNames :: [Types.ApplicationName]
    -- ^ A list of application names separated by spaces. The maximum number of application names you can specify is 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetApplications' value with any optional fields omitted.
mkBatchGetApplications
    :: BatchGetApplications
mkBatchGetApplications
  = BatchGetApplications'{applicationNames = Core.mempty}

-- | A list of application names separated by spaces. The maximum number of application names you can specify is 100.
--
-- /Note:/ Consider using 'applicationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgaApplicationNames :: Lens.Lens' BatchGetApplications [Types.ApplicationName]
bgaApplicationNames = Lens.field @"applicationNames"
{-# INLINEABLE bgaApplicationNames #-}
{-# DEPRECATED applicationNames "Use generic-lens or generic-optics with 'applicationNames' instead"  #-}

instance Core.ToQuery BatchGetApplications where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetApplications where
        toHeaders BatchGetApplications{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.BatchGetApplications")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetApplications where
        toJSON BatchGetApplications{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("applicationNames" Core..= applicationNames)])

instance Core.AWSRequest BatchGetApplications where
        type Rs BatchGetApplications = BatchGetApplicationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetApplicationsResponse' Core.<$>
                   (x Core..:? "applicationsInfo") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @BatchGetApplications@ operation.
--
-- /See:/ 'mkBatchGetApplicationsResponse' smart constructor.
data BatchGetApplicationsResponse = BatchGetApplicationsResponse'
  { applicationsInfo :: Core.Maybe [Types.ApplicationInfo]
    -- ^ Information about the applications.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetApplicationsResponse' value with any optional fields omitted.
mkBatchGetApplicationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetApplicationsResponse
mkBatchGetApplicationsResponse responseStatus
  = BatchGetApplicationsResponse'{applicationsInfo = Core.Nothing,
                                  responseStatus}

-- | Information about the applications.
--
-- /Note:/ Consider using 'applicationsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrsApplicationsInfo :: Lens.Lens' BatchGetApplicationsResponse (Core.Maybe [Types.ApplicationInfo])
bgarrsApplicationsInfo = Lens.field @"applicationsInfo"
{-# INLINEABLE bgarrsApplicationsInfo #-}
{-# DEPRECATED applicationsInfo "Use generic-lens or generic-optics with 'applicationsInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrsResponseStatus :: Lens.Lens' BatchGetApplicationsResponse Core.Int
bgarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
