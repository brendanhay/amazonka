{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeleteAppsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager applications list.
module Network.AWS.FMS.DeleteAppsList
    (
    -- * Creating a request
      DeleteAppsList (..)
    , mkDeleteAppsList
    -- ** Request lenses
    , dalListId

    -- * Destructuring the response
    , DeleteAppsListResponse (..)
    , mkDeleteAppsListResponse
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAppsList' smart constructor.
newtype DeleteAppsList = DeleteAppsList'
  { listId :: Types.ListId
    -- ^ The ID of the applications list that you want to delete. You can retrieve this ID from @PutAppsList@ , @ListAppsLists@ , and @GetAppsList@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppsList' value with any optional fields omitted.
mkDeleteAppsList
    :: Types.ListId -- ^ 'listId'
    -> DeleteAppsList
mkDeleteAppsList listId = DeleteAppsList'{listId}

-- | The ID of the applications list that you want to delete. You can retrieve this ID from @PutAppsList@ , @ListAppsLists@ , and @GetAppsList@ .
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalListId :: Lens.Lens' DeleteAppsList Types.ListId
dalListId = Lens.field @"listId"
{-# INLINEABLE dalListId #-}
{-# DEPRECATED listId "Use generic-lens or generic-optics with 'listId' instead"  #-}

instance Core.ToQuery DeleteAppsList where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAppsList where
        toHeaders DeleteAppsList{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.DeleteAppsList")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAppsList where
        toJSON DeleteAppsList{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ListId" Core..= listId)])

instance Core.AWSRequest DeleteAppsList where
        type Rs DeleteAppsList = DeleteAppsListResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAppsListResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAppsListResponse' smart constructor.
data DeleteAppsListResponse = DeleteAppsListResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppsListResponse' value with any optional fields omitted.
mkDeleteAppsListResponse
    :: DeleteAppsListResponse
mkDeleteAppsListResponse = DeleteAppsListResponse'
