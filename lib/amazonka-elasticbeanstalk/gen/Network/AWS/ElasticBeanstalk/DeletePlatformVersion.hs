{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeletePlatformVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of a custom platform.
module Network.AWS.ElasticBeanstalk.DeletePlatformVersion
    (
    -- * Creating a request
      DeletePlatformVersion (..)
    , mkDeletePlatformVersion
    -- ** Request lenses
    , dpvPlatformArn

    -- * Destructuring the response
    , DeletePlatformVersionResponse (..)
    , mkDeletePlatformVersionResponse
    -- ** Response lenses
    , dpvrrsPlatformSummary
    , dpvrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePlatformVersion' smart constructor.
newtype DeletePlatformVersion = DeletePlatformVersion'
  { platformArn :: Core.Maybe Types.PlatformArn
    -- ^ The ARN of the version of the custom platform.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePlatformVersion' value with any optional fields omitted.
mkDeletePlatformVersion
    :: DeletePlatformVersion
mkDeletePlatformVersion
  = DeletePlatformVersion'{platformArn = Core.Nothing}

-- | The ARN of the version of the custom platform.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvPlatformArn :: Lens.Lens' DeletePlatformVersion (Core.Maybe Types.PlatformArn)
dpvPlatformArn = Lens.field @"platformArn"
{-# INLINEABLE dpvPlatformArn #-}
{-# DEPRECATED platformArn "Use generic-lens or generic-optics with 'platformArn' instead"  #-}

instance Core.ToQuery DeletePlatformVersion where
        toQuery DeletePlatformVersion{..}
          = Core.toQueryPair "Action" ("DeletePlatformVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PlatformArn") platformArn

instance Core.ToHeaders DeletePlatformVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeletePlatformVersion where
        type Rs DeletePlatformVersion = DeletePlatformVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DeletePlatformVersionResult"
              (\ s h x ->
                 DeletePlatformVersionResponse' Core.<$>
                   (x Core..@? "PlatformSummary") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePlatformVersionResponse' smart constructor.
data DeletePlatformVersionResponse = DeletePlatformVersionResponse'
  { platformSummary :: Core.Maybe Types.PlatformSummary
    -- ^ Detailed information about the version of the custom platform.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePlatformVersionResponse' value with any optional fields omitted.
mkDeletePlatformVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeletePlatformVersionResponse
mkDeletePlatformVersionResponse responseStatus
  = DeletePlatformVersionResponse'{platformSummary = Core.Nothing,
                                   responseStatus}

-- | Detailed information about the version of the custom platform.
--
-- /Note:/ Consider using 'platformSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrrsPlatformSummary :: Lens.Lens' DeletePlatformVersionResponse (Core.Maybe Types.PlatformSummary)
dpvrrsPlatformSummary = Lens.field @"platformSummary"
{-# INLINEABLE dpvrrsPlatformSummary #-}
{-# DEPRECATED platformSummary "Use generic-lens or generic-optics with 'platformSummary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrrsResponseStatus :: Lens.Lens' DeletePlatformVersionResponse Core.Int
dpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
