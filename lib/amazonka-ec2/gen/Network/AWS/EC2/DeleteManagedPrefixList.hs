{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified managed prefix list. You must first remove all references to the prefix list in your resources.
module Network.AWS.EC2.DeleteManagedPrefixList
    (
    -- * Creating a request
      DeleteManagedPrefixList (..)
    , mkDeleteManagedPrefixList
    -- ** Request lenses
    , dmplPrefixListId
    , dmplDryRun

    -- * Destructuring the response
    , DeleteManagedPrefixListResponse (..)
    , mkDeleteManagedPrefixListResponse
    -- ** Response lenses
    , dmplrrsPrefixList
    , dmplrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteManagedPrefixList' smart constructor.
data DeleteManagedPrefixList = DeleteManagedPrefixList'
  { prefixListId :: Types.PrefixListResourceId
    -- ^ The ID of the prefix list.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteManagedPrefixList' value with any optional fields omitted.
mkDeleteManagedPrefixList
    :: Types.PrefixListResourceId -- ^ 'prefixListId'
    -> DeleteManagedPrefixList
mkDeleteManagedPrefixList prefixListId
  = DeleteManagedPrefixList'{prefixListId, dryRun = Core.Nothing}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplPrefixListId :: Lens.Lens' DeleteManagedPrefixList Types.PrefixListResourceId
dmplPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE dmplPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplDryRun :: Lens.Lens' DeleteManagedPrefixList (Core.Maybe Core.Bool)
dmplDryRun = Lens.field @"dryRun"
{-# INLINEABLE dmplDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteManagedPrefixList where
        toQuery DeleteManagedPrefixList{..}
          = Core.toQueryPair "Action"
              ("DeleteManagedPrefixList" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PrefixListId" prefixListId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteManagedPrefixList where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteManagedPrefixList where
        type Rs DeleteManagedPrefixList = DeleteManagedPrefixListResponse
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
          = Response.receiveXML
              (\ s h x ->
                 DeleteManagedPrefixListResponse' Core.<$>
                   (x Core..@? "prefixList") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteManagedPrefixListResponse' smart constructor.
data DeleteManagedPrefixListResponse = DeleteManagedPrefixListResponse'
  { prefixList :: Core.Maybe Types.ManagedPrefixList
    -- ^ Information about the prefix list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteManagedPrefixListResponse' value with any optional fields omitted.
mkDeleteManagedPrefixListResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteManagedPrefixListResponse
mkDeleteManagedPrefixListResponse responseStatus
  = DeleteManagedPrefixListResponse'{prefixList = Core.Nothing,
                                     responseStatus}

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplrrsPrefixList :: Lens.Lens' DeleteManagedPrefixListResponse (Core.Maybe Types.ManagedPrefixList)
dmplrrsPrefixList = Lens.field @"prefixList"
{-# INLINEABLE dmplrrsPrefixList #-}
{-# DEPRECATED prefixList "Use generic-lens or generic-optics with 'prefixList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplrrsResponseStatus :: Lens.Lens' DeleteManagedPrefixListResponse Core.Int
dmplrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmplrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
