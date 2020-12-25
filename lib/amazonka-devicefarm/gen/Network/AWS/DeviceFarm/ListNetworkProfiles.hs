{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListNetworkProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of available network profiles.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListNetworkProfiles
  ( -- * Creating a request
    ListNetworkProfiles (..),
    mkListNetworkProfiles,

    -- ** Request lenses
    lnpArn,
    lnpNextToken,
    lnpType,

    -- * Destructuring the response
    ListNetworkProfilesResponse (..),
    mkListNetworkProfilesResponse,

    -- ** Response lenses
    lnprrsNetworkProfiles,
    lnprrsNextToken,
    lnprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListNetworkProfiles' smart constructor.
data ListNetworkProfiles = ListNetworkProfiles'
  { -- | The Amazon Resource Name (ARN) of the project for which you want to list network profiles.
    arn :: Types.Arn,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The type of network profile to return information about. Valid values are listed here.
    type' :: Core.Maybe Types.NetworkProfileType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListNetworkProfiles' value with any optional fields omitted.
mkListNetworkProfiles ::
  -- | 'arn'
  Types.Arn ->
  ListNetworkProfiles
mkListNetworkProfiles arn =
  ListNetworkProfiles'
    { arn,
      nextToken = Core.Nothing,
      type' = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project for which you want to list network profiles.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnpArn :: Lens.Lens' ListNetworkProfiles Types.Arn
lnpArn = Lens.field @"arn"
{-# DEPRECATED lnpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnpNextToken :: Lens.Lens' ListNetworkProfiles (Core.Maybe Types.PaginationToken)
lnpNextToken = Lens.field @"nextToken"
{-# DEPRECATED lnpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of network profile to return information about. Valid values are listed here.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnpType :: Lens.Lens' ListNetworkProfiles (Core.Maybe Types.NetworkProfileType)
lnpType = Lens.field @"type'"
{-# DEPRECATED lnpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ListNetworkProfiles where
  toJSON ListNetworkProfiles {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("arn" Core..= arn),
            ("nextToken" Core..=) Core.<$> nextToken,
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.AWSRequest ListNetworkProfiles where
  type Rs ListNetworkProfiles = ListNetworkProfilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.ListNetworkProfiles")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNetworkProfilesResponse'
            Core.<$> (x Core..:? "networkProfiles")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListNetworkProfiles where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"networkProfiles" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListNetworkProfilesResponse' smart constructor.
data ListNetworkProfilesResponse = ListNetworkProfilesResponse'
  { -- | A list of the available network profiles.
    networkProfiles :: Core.Maybe [Types.NetworkProfile],
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListNetworkProfilesResponse' value with any optional fields omitted.
mkListNetworkProfilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListNetworkProfilesResponse
mkListNetworkProfilesResponse responseStatus =
  ListNetworkProfilesResponse'
    { networkProfiles = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of the available network profiles.
--
-- /Note:/ Consider using 'networkProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnprrsNetworkProfiles :: Lens.Lens' ListNetworkProfilesResponse (Core.Maybe [Types.NetworkProfile])
lnprrsNetworkProfiles = Lens.field @"networkProfiles"
{-# DEPRECATED lnprrsNetworkProfiles "Use generic-lens or generic-optics with 'networkProfiles' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnprrsNextToken :: Lens.Lens' ListNetworkProfilesResponse (Core.Maybe Types.PaginationToken)
lnprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lnprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnprrsResponseStatus :: Lens.Lens' ListNetworkProfilesResponse Core.Int
lnprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lnprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
