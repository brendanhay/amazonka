{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyManagedPrefixList (..)
    , mkModifyManagedPrefixList
    -- ** Request lenses
    , mmplPrefixListId
    , mmplAddEntries
    , mmplCurrentVersion
    , mmplDryRun
    , mmplPrefixListName
    , mmplRemoveEntries

    -- * Destructuring the response
    , ModifyManagedPrefixListResponse (..)
    , mkModifyManagedPrefixListResponse
    -- ** Response lenses
    , mmplrrsPrefixList
    , mmplrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyManagedPrefixList' smart constructor.
data ModifyManagedPrefixList = ModifyManagedPrefixList'
  { prefixListId :: Types.PrefixListResourceId
    -- ^ The ID of the prefix list.
  , addEntries :: Core.Maybe [Types.AddPrefixListEntry]
    -- ^ One or more entries to add to the prefix list.
  , currentVersion :: Core.Maybe Core.Integer
    -- ^ The current version of the prefix list.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , prefixListName :: Core.Maybe Core.Text
    -- ^ A name for the prefix list.
  , removeEntries :: Core.Maybe [Types.RemovePrefixListEntry]
    -- ^ One or more entries to remove from the prefix list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyManagedPrefixList' value with any optional fields omitted.
mkModifyManagedPrefixList
    :: Types.PrefixListResourceId -- ^ 'prefixListId'
    -> ModifyManagedPrefixList
mkModifyManagedPrefixList prefixListId
  = ModifyManagedPrefixList'{prefixListId, addEntries = Core.Nothing,
                             currentVersion = Core.Nothing, dryRun = Core.Nothing,
                             prefixListName = Core.Nothing, removeEntries = Core.Nothing}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplPrefixListId :: Lens.Lens' ModifyManagedPrefixList Types.PrefixListResourceId
mmplPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE mmplPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | One or more entries to add to the prefix list.
--
-- /Note:/ Consider using 'addEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplAddEntries :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe [Types.AddPrefixListEntry])
mmplAddEntries = Lens.field @"addEntries"
{-# INLINEABLE mmplAddEntries #-}
{-# DEPRECATED addEntries "Use generic-lens or generic-optics with 'addEntries' instead"  #-}

-- | The current version of the prefix list.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplCurrentVersion :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe Core.Integer)
mmplCurrentVersion = Lens.field @"currentVersion"
{-# INLINEABLE mmplCurrentVersion #-}
{-# DEPRECATED currentVersion "Use generic-lens or generic-optics with 'currentVersion' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplDryRun :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe Core.Bool)
mmplDryRun = Lens.field @"dryRun"
{-# INLINEABLE mmplDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | A name for the prefix list.
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplPrefixListName :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe Core.Text)
mmplPrefixListName = Lens.field @"prefixListName"
{-# INLINEABLE mmplPrefixListName #-}
{-# DEPRECATED prefixListName "Use generic-lens or generic-optics with 'prefixListName' instead"  #-}

-- | One or more entries to remove from the prefix list.
--
-- /Note:/ Consider using 'removeEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplRemoveEntries :: Lens.Lens' ModifyManagedPrefixList (Core.Maybe [Types.RemovePrefixListEntry])
mmplRemoveEntries = Lens.field @"removeEntries"
{-# INLINEABLE mmplRemoveEntries #-}
{-# DEPRECATED removeEntries "Use generic-lens or generic-optics with 'removeEntries' instead"  #-}

instance Core.ToQuery ModifyManagedPrefixList where
        toQuery ModifyManagedPrefixList{..}
          = Core.toQueryPair "Action"
              ("ModifyManagedPrefixList" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PrefixListId" prefixListId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AddEntry") addEntries
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CurrentVersion")
                currentVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrefixListName")
                prefixListName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RemoveEntry")
                removeEntries

instance Core.ToHeaders ModifyManagedPrefixList where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyManagedPrefixList where
        type Rs ModifyManagedPrefixList = ModifyManagedPrefixListResponse
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
                 ModifyManagedPrefixListResponse' Core.<$>
                   (x Core..@? "prefixList") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyManagedPrefixListResponse' smart constructor.
data ModifyManagedPrefixListResponse = ModifyManagedPrefixListResponse'
  { prefixList :: Core.Maybe Types.ManagedPrefixList
    -- ^ Information about the prefix list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyManagedPrefixListResponse' value with any optional fields omitted.
mkModifyManagedPrefixListResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyManagedPrefixListResponse
mkModifyManagedPrefixListResponse responseStatus
  = ModifyManagedPrefixListResponse'{prefixList = Core.Nothing,
                                     responseStatus}

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplrrsPrefixList :: Lens.Lens' ModifyManagedPrefixListResponse (Core.Maybe Types.ManagedPrefixList)
mmplrrsPrefixList = Lens.field @"prefixList"
{-# INLINEABLE mmplrrsPrefixList #-}
{-# DEPRECATED prefixList "Use generic-lens or generic-optics with 'prefixList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplrrsResponseStatus :: Lens.Lens' ModifyManagedPrefixListResponse Core.Int
mmplrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mmplrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
