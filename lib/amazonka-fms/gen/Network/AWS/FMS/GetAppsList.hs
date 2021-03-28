{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetAppsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager applications list.
module Network.AWS.FMS.GetAppsList
    (
    -- * Creating a request
      GetAppsList (..)
    , mkGetAppsList
    -- ** Request lenses
    , galListId
    , galDefaultList

    -- * Destructuring the response
    , GetAppsListResponse (..)
    , mkGetAppsListResponse
    -- ** Response lenses
    , galrrsAppsList
    , galrrsAppsListArn
    , galrrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAppsList' smart constructor.
data GetAppsList = GetAppsList'
  { listId :: Types.ListId
    -- ^ The ID of the AWS Firewall Manager applications list that you want the details for.
  , defaultList :: Core.Maybe Core.Bool
    -- ^ Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppsList' value with any optional fields omitted.
mkGetAppsList
    :: Types.ListId -- ^ 'listId'
    -> GetAppsList
mkGetAppsList listId
  = GetAppsList'{listId, defaultList = Core.Nothing}

-- | The ID of the AWS Firewall Manager applications list that you want the details for.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galListId :: Lens.Lens' GetAppsList Types.ListId
galListId = Lens.field @"listId"
{-# INLINEABLE galListId #-}
{-# DEPRECATED listId "Use generic-lens or generic-optics with 'listId' instead"  #-}

-- | Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
--
-- /Note:/ Consider using 'defaultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galDefaultList :: Lens.Lens' GetAppsList (Core.Maybe Core.Bool)
galDefaultList = Lens.field @"defaultList"
{-# INLINEABLE galDefaultList #-}
{-# DEPRECATED defaultList "Use generic-lens or generic-optics with 'defaultList' instead"  #-}

instance Core.ToQuery GetAppsList where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAppsList where
        toHeaders GetAppsList{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.GetAppsList") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAppsList where
        toJSON GetAppsList{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ListId" Core..= listId),
                  ("DefaultList" Core..=) Core.<$> defaultList])

instance Core.AWSRequest GetAppsList where
        type Rs GetAppsList = GetAppsListResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAppsListResponse' Core.<$>
                   (x Core..:? "AppsList") Core.<*> x Core..:? "AppsListArn" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAppsListResponse' smart constructor.
data GetAppsListResponse = GetAppsListResponse'
  { appsList :: Core.Maybe Types.AppsListData
    -- ^ Information about the specified AWS Firewall Manager applications list.
  , appsListArn :: Core.Maybe Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the applications list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAppsListResponse' value with any optional fields omitted.
mkGetAppsListResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAppsListResponse
mkGetAppsListResponse responseStatus
  = GetAppsListResponse'{appsList = Core.Nothing,
                         appsListArn = Core.Nothing, responseStatus}

-- | Information about the specified AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galrrsAppsList :: Lens.Lens' GetAppsListResponse (Core.Maybe Types.AppsListData)
galrrsAppsList = Lens.field @"appsList"
{-# INLINEABLE galrrsAppsList #-}
{-# DEPRECATED appsList "Use generic-lens or generic-optics with 'appsList' instead"  #-}

-- | The Amazon Resource Name (ARN) of the applications list.
--
-- /Note:/ Consider using 'appsListArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galrrsAppsListArn :: Lens.Lens' GetAppsListResponse (Core.Maybe Types.ResourceArn)
galrrsAppsListArn = Lens.field @"appsListArn"
{-# INLINEABLE galrrsAppsListArn #-}
{-# DEPRECATED appsListArn "Use generic-lens or generic-optics with 'appsListArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galrrsResponseStatus :: Lens.Lens' GetAppsListResponse Core.Int
galrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE galrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
