{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RestoreManagedPrefixListVersion (..),
    mkRestoreManagedPrefixListVersion,

    -- ** Request lenses
    rmplvPrefixListId,
    rmplvPreviousVersion,
    rmplvCurrentVersion,
    rmplvDryRun,

    -- * Destructuring the response
    RestoreManagedPrefixListVersionResponse (..),
    mkRestoreManagedPrefixListVersionResponse,

    -- ** Response lenses
    rmplvrrsPrefixList,
    rmplvrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreManagedPrefixListVersion' smart constructor.
data RestoreManagedPrefixListVersion = RestoreManagedPrefixListVersion'
  { -- | The ID of the prefix list.
    prefixListId :: Types.PrefixListResourceId,
    -- | The version to restore.
    previousVersion :: Core.Integer,
    -- | The current version number for the prefix list.
    currentVersion :: Core.Integer,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreManagedPrefixListVersion' value with any optional fields omitted.
mkRestoreManagedPrefixListVersion ::
  -- | 'prefixListId'
  Types.PrefixListResourceId ->
  -- | 'previousVersion'
  Core.Integer ->
  -- | 'currentVersion'
  Core.Integer ->
  RestoreManagedPrefixListVersion
mkRestoreManagedPrefixListVersion
  prefixListId
  previousVersion
  currentVersion =
    RestoreManagedPrefixListVersion'
      { prefixListId,
        previousVersion,
        currentVersion,
        dryRun = Core.Nothing
      }

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvPrefixListId :: Lens.Lens' RestoreManagedPrefixListVersion Types.PrefixListResourceId
rmplvPrefixListId = Lens.field @"prefixListId"
{-# DEPRECATED rmplvPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | The version to restore.
--
-- /Note:/ Consider using 'previousVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvPreviousVersion :: Lens.Lens' RestoreManagedPrefixListVersion Core.Integer
rmplvPreviousVersion = Lens.field @"previousVersion"
{-# DEPRECATED rmplvPreviousVersion "Use generic-lens or generic-optics with 'previousVersion' instead." #-}

-- | The current version number for the prefix list.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvCurrentVersion :: Lens.Lens' RestoreManagedPrefixListVersion Core.Integer
rmplvCurrentVersion = Lens.field @"currentVersion"
{-# DEPRECATED rmplvCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvDryRun :: Lens.Lens' RestoreManagedPrefixListVersion (Core.Maybe Core.Bool)
rmplvDryRun = Lens.field @"dryRun"
{-# DEPRECATED rmplvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest RestoreManagedPrefixListVersion where
  type
    Rs RestoreManagedPrefixListVersion =
      RestoreManagedPrefixListVersionResponse
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
            ( Core.pure ("Action", "RestoreManagedPrefixListVersion")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "PrefixListId" prefixListId)
                Core.<> (Core.toQueryValue "PreviousVersion" previousVersion)
                Core.<> (Core.toQueryValue "CurrentVersion" currentVersion)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          RestoreManagedPrefixListVersionResponse'
            Core.<$> (x Core..@? "prefixList") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreManagedPrefixListVersionResponse' smart constructor.
data RestoreManagedPrefixListVersionResponse = RestoreManagedPrefixListVersionResponse'
  { -- | Information about the prefix list.
    prefixList :: Core.Maybe Types.ManagedPrefixList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreManagedPrefixListVersionResponse' value with any optional fields omitted.
mkRestoreManagedPrefixListVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreManagedPrefixListVersionResponse
mkRestoreManagedPrefixListVersionResponse responseStatus =
  RestoreManagedPrefixListVersionResponse'
    { prefixList =
        Core.Nothing,
      responseStatus
    }

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvrrsPrefixList :: Lens.Lens' RestoreManagedPrefixListVersionResponse (Core.Maybe Types.ManagedPrefixList)
rmplvrrsPrefixList = Lens.field @"prefixList"
{-# DEPRECATED rmplvrrsPrefixList "Use generic-lens or generic-optics with 'prefixList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvrrsResponseStatus :: Lens.Lens' RestoreManagedPrefixListVersionResponse Core.Int
rmplvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rmplvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
