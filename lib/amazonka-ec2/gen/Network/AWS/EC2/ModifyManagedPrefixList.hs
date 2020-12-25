{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified managed prefix list.
--
-- Adding or removing entries in a prefix list creates a new version of the prefix list. Changing the name of the prefix list does not affect the version.
-- If you specify a current version number that does not match the true current version number, the request fails.
module Network.AWS.EC2.ModifyManagedPrefixList
  ( -- * Creating a request
    ModifyManagedPrefixList (..),
    mkModifyManagedPrefixList,

    -- ** Request lenses
    mmplPrefixListId,
    mmplAddEntries,
    mmplCurrentVersion,
    mmplDryRun,
    mmplPrefixListName,
    mmplRemoveEntries,

    -- * Destructuring the response
    ModifyManagedPrefixListResponse (..),
    mkModifyManagedPrefixListResponse,

    -- ** Response lenses
    mmplrrsPrefixList,
    mmplrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyManagedPrefixList' smart constructor.
data ModifyManagedPrefixList = ModifyManagedPrefixList'
  { -- | The ID of the prefix list.
    prefixListId :: Types.PrefixListResourceId,
    -- | One or more entries to add to the prefix list.
    addEntries :: Core.Maybe [Types.AddPrefixListEntry],
    -- | The current version of the prefix list.
    currentVersion :: Core.Maybe Core.Integer,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | A name for the prefix list.
    prefixListName :: Core.Maybe Types.String,
    -- | One or more entries to remove from the prefix list.
    removeEntries :: Core.Maybe [Types.RemovePrefixListEntry]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyManagedPrefixList' value with any optional fields omitted.
mkModifyManagedPrefixList ::
  -- | 'prefixListId'
  Types.PrefixListResourceId ->
  ModifyManagedPrefixList
mkModifyManagedPrefixList prefixListId =
  ModifyManagedPrefixList'
    { prefixListId,
      addEntries = Core.Nothing,
      currentVersion = Core.Nothing,
      dryRun = Core.Nothing,
      prefixListName = Core.Nothing,
      removeEntries = Core.Nothing
    }

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplPrefixListId :: Lens.Lens' ModifyManagedPrefixList Types.PrefixListResourceId
mmplPrefixListId = Lens.field @"prefixListId"
{-# DEPRECATED mmplPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | One or more entries to add to the prefix list.
--
-- /Note:/ Consider using 'addEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplAddEntries :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe [Types.AddPrefixListEntry])
mmplAddEntries = Lens.field @"addEntries"
{-# DEPRECATED mmplAddEntries "Use generic-lens or generic-optics with 'addEntries' instead." #-}

-- | The current version of the prefix list.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplCurrentVersion :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe Core.Integer)
mmplCurrentVersion = Lens.field @"currentVersion"
{-# DEPRECATED mmplCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplDryRun :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe Core.Bool)
mmplDryRun = Lens.field @"dryRun"
{-# DEPRECATED mmplDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | A name for the prefix list.
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplPrefixListName :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe Types.String)
mmplPrefixListName = Lens.field @"prefixListName"
{-# DEPRECATED mmplPrefixListName "Use generic-lens or generic-optics with 'prefixListName' instead." #-}

-- | One or more entries to remove from the prefix list.
--
-- /Note:/ Consider using 'removeEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplRemoveEntries :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe [Types.RemovePrefixListEntry])
mmplRemoveEntries = Lens.field @"removeEntries"
{-# DEPRECATED mmplRemoveEntries "Use generic-lens or generic-optics with 'removeEntries' instead." #-}

instance Core.AWSRequest ModifyManagedPrefixList where
  type Rs ModifyManagedPrefixList = ModifyManagedPrefixListResponse
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
            ( Core.pure ("Action", "ModifyManagedPrefixList")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "PrefixListId" prefixListId)
                Core.<> (Core.toQueryList "AddEntry" Core.<$> addEntries)
                Core.<> (Core.toQueryValue "CurrentVersion" Core.<$> currentVersion)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "PrefixListName" Core.<$> prefixListName)
                Core.<> (Core.toQueryList "RemoveEntry" Core.<$> removeEntries)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyManagedPrefixListResponse'
            Core.<$> (x Core..@? "prefixList") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyManagedPrefixListResponse' smart constructor.
data ModifyManagedPrefixListResponse = ModifyManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Core.Maybe Types.ManagedPrefixList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyManagedPrefixListResponse' value with any optional fields omitted.
mkModifyManagedPrefixListResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyManagedPrefixListResponse
mkModifyManagedPrefixListResponse responseStatus =
  ModifyManagedPrefixListResponse'
    { prefixList = Core.Nothing,
      responseStatus
    }

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplrrsPrefixList :: Lens.Lens' ModifyManagedPrefixListResponse (Core.Maybe Types.ManagedPrefixList)
mmplrrsPrefixList = Lens.field @"prefixList"
{-# DEPRECATED mmplrrsPrefixList "Use generic-lens or generic-optics with 'prefixList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplrrsResponseStatus :: Lens.Lens' ModifyManagedPrefixListResponse Core.Int
mmplrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mmplrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
