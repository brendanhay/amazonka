{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteManagedPrefixList (..),
    mkDeleteManagedPrefixList,

    -- ** Request lenses
    dmplPrefixListId,
    dmplDryRun,

    -- * Destructuring the response
    DeleteManagedPrefixListResponse (..),
    mkDeleteManagedPrefixListResponse,

    -- ** Response lenses
    dmplrrsPrefixList,
    dmplrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteManagedPrefixList' smart constructor.
data DeleteManagedPrefixList = DeleteManagedPrefixList'
  { -- | The ID of the prefix list.
    prefixListId :: Types.PrefixListResourceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteManagedPrefixList' value with any optional fields omitted.
mkDeleteManagedPrefixList ::
  -- | 'prefixListId'
  Types.PrefixListResourceId ->
  DeleteManagedPrefixList
mkDeleteManagedPrefixList prefixListId =
  DeleteManagedPrefixList' {prefixListId, dryRun = Core.Nothing}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplPrefixListId :: Lens.Lens' DeleteManagedPrefixList Types.PrefixListResourceId
dmplPrefixListId = Lens.field @"prefixListId"
{-# DEPRECATED dmplPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplDryRun :: Lens.Lens' DeleteManagedPrefixList (Core.Maybe Core.Bool)
dmplDryRun = Lens.field @"dryRun"
{-# DEPRECATED dmplDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteManagedPrefixList where
  type Rs DeleteManagedPrefixList = DeleteManagedPrefixListResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteManagedPrefixList")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "PrefixListId" prefixListId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteManagedPrefixListResponse'
            Core.<$> (x Core..@? "prefixList") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteManagedPrefixListResponse' smart constructor.
data DeleteManagedPrefixListResponse = DeleteManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Core.Maybe Types.ManagedPrefixList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteManagedPrefixListResponse' value with any optional fields omitted.
mkDeleteManagedPrefixListResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteManagedPrefixListResponse
mkDeleteManagedPrefixListResponse responseStatus =
  DeleteManagedPrefixListResponse'
    { prefixList = Core.Nothing,
      responseStatus
    }

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplrrsPrefixList :: Lens.Lens' DeleteManagedPrefixListResponse (Core.Maybe Types.ManagedPrefixList)
dmplrrsPrefixList = Lens.field @"prefixList"
{-# DEPRECATED dmplrrsPrefixList "Use generic-lens or generic-optics with 'prefixList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplrrsResponseStatus :: Lens.Lens' DeleteManagedPrefixListResponse Core.Int
dmplrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmplrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
