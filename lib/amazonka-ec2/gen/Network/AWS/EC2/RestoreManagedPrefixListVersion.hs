{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RestoreManagedPrefixListVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the entries from a previous version of a managed prefix list to a new version of the prefix list.
module Network.AWS.EC2.RestoreManagedPrefixListVersion
    (
    -- * Creating a request
      RestoreManagedPrefixListVersion (..)
    , mkRestoreManagedPrefixListVersion
    -- ** Request lenses
    , rmplvPrefixListId
    , rmplvPreviousVersion
    , rmplvCurrentVersion
    , rmplvDryRun

    -- * Destructuring the response
    , RestoreManagedPrefixListVersionResponse (..)
    , mkRestoreManagedPrefixListVersionResponse
    -- ** Response lenses
    , rmplvrrsPrefixList
    , rmplvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreManagedPrefixListVersion' smart constructor.
data RestoreManagedPrefixListVersion = RestoreManagedPrefixListVersion'
  { prefixListId :: Types.PrefixListResourceId
    -- ^ The ID of the prefix list.
  , previousVersion :: Core.Integer
    -- ^ The version to restore.
  , currentVersion :: Core.Integer
    -- ^ The current version number for the prefix list.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreManagedPrefixListVersion' value with any optional fields omitted.
mkRestoreManagedPrefixListVersion
    :: Types.PrefixListResourceId -- ^ 'prefixListId'
    -> Core.Integer -- ^ 'previousVersion'
    -> Core.Integer -- ^ 'currentVersion'
    -> RestoreManagedPrefixListVersion
mkRestoreManagedPrefixListVersion prefixListId previousVersion
  currentVersion
  = RestoreManagedPrefixListVersion'{prefixListId, previousVersion,
                                     currentVersion, dryRun = Core.Nothing}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvPrefixListId :: Lens.Lens' RestoreManagedPrefixListVersion Types.PrefixListResourceId
rmplvPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE rmplvPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | The version to restore.
--
-- /Note:/ Consider using 'previousVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvPreviousVersion :: Lens.Lens' RestoreManagedPrefixListVersion Core.Integer
rmplvPreviousVersion = Lens.field @"previousVersion"
{-# INLINEABLE rmplvPreviousVersion #-}
{-# DEPRECATED previousVersion "Use generic-lens or generic-optics with 'previousVersion' instead"  #-}

-- | The current version number for the prefix list.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvCurrentVersion :: Lens.Lens' RestoreManagedPrefixListVersion Core.Integer
rmplvCurrentVersion = Lens.field @"currentVersion"
{-# INLINEABLE rmplvCurrentVersion #-}
{-# DEPRECATED currentVersion "Use generic-lens or generic-optics with 'currentVersion' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvDryRun :: Lens.Lens' RestoreManagedPrefixListVersion (Core.Maybe Core.Bool)
rmplvDryRun = Lens.field @"dryRun"
{-# INLINEABLE rmplvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery RestoreManagedPrefixListVersion where
        toQuery RestoreManagedPrefixListVersion{..}
          = Core.toQueryPair "Action"
              ("RestoreManagedPrefixListVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PrefixListId" prefixListId
              Core.<> Core.toQueryPair "PreviousVersion" previousVersion
              Core.<> Core.toQueryPair "CurrentVersion" currentVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders RestoreManagedPrefixListVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestoreManagedPrefixListVersion where
        type Rs RestoreManagedPrefixListVersion =
             RestoreManagedPrefixListVersionResponse
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
                 RestoreManagedPrefixListVersionResponse' Core.<$>
                   (x Core..@? "prefixList") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreManagedPrefixListVersionResponse' smart constructor.
data RestoreManagedPrefixListVersionResponse = RestoreManagedPrefixListVersionResponse'
  { prefixList :: Core.Maybe Types.ManagedPrefixList
    -- ^ Information about the prefix list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreManagedPrefixListVersionResponse' value with any optional fields omitted.
mkRestoreManagedPrefixListVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreManagedPrefixListVersionResponse
mkRestoreManagedPrefixListVersionResponse responseStatus
  = RestoreManagedPrefixListVersionResponse'{prefixList =
                                               Core.Nothing,
                                             responseStatus}

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvrrsPrefixList :: Lens.Lens' RestoreManagedPrefixListVersionResponse (Core.Maybe Types.ManagedPrefixList)
rmplvrrsPrefixList = Lens.field @"prefixList"
{-# INLINEABLE rmplvrrsPrefixList #-}
{-# DEPRECATED prefixList "Use generic-lens or generic-optics with 'prefixList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvrrsResponseStatus :: Lens.Lens' RestoreManagedPrefixListVersionResponse Core.Int
rmplvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rmplvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
