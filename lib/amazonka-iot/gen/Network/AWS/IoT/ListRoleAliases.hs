{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListRoleAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the role aliases registered in your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListRoleAliases
  ( -- * Creating a request
    ListRoleAliases (..),
    mkListRoleAliases,

    -- ** Request lenses
    lraAscendingOrder,
    lraMarker,
    lraPageSize,

    -- * Destructuring the response
    ListRoleAliasesResponse (..),
    mkListRoleAliasesResponse,

    -- ** Response lenses
    lrarrsNextMarker,
    lrarrsRoleAliases,
    lrarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRoleAliases' smart constructor.
data ListRoleAliases = ListRoleAliases'
  { -- | Return the list of role aliases in ascending alphabetical order.
    ascendingOrder :: Core.Maybe Core.Bool,
    -- | A marker used to get the next set of results.
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of results to return at one time.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoleAliases' value with any optional fields omitted.
mkListRoleAliases ::
  ListRoleAliases
mkListRoleAliases =
  ListRoleAliases'
    { ascendingOrder = Core.Nothing,
      marker = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | Return the list of role aliases in ascending alphabetical order.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraAscendingOrder :: Lens.Lens' ListRoleAliases (Core.Maybe Core.Bool)
lraAscendingOrder = Lens.field @"ascendingOrder"
{-# DEPRECATED lraAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraMarker :: Lens.Lens' ListRoleAliases (Core.Maybe Types.Marker)
lraMarker = Lens.field @"marker"
{-# DEPRECATED lraMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraPageSize :: Lens.Lens' ListRoleAliases (Core.Maybe Core.Natural)
lraPageSize = Lens.field @"pageSize"
{-# DEPRECATED lraPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.AWSRequest ListRoleAliases where
  type Rs ListRoleAliases = ListRoleAliasesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/role-aliases",
        Core._rqQuery =
          Core.toQueryValue "isAscendingOrder" Core.<$> ascendingOrder
            Core.<> (Core.toQueryValue "marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "pageSize" Core.<$> pageSize),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoleAliasesResponse'
            Core.<$> (x Core..:? "nextMarker")
            Core.<*> (x Core..:? "roleAliases")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListRoleAliases where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"roleAliases" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListRoleAliasesResponse' smart constructor.
data ListRoleAliasesResponse = ListRoleAliasesResponse'
  { -- | A marker used to get the next set of results.
    nextMarker :: Core.Maybe Types.Marker,
    -- | The role aliases.
    roleAliases :: Core.Maybe [Types.RoleAlias],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoleAliasesResponse' value with any optional fields omitted.
mkListRoleAliasesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRoleAliasesResponse
mkListRoleAliasesResponse responseStatus =
  ListRoleAliasesResponse'
    { nextMarker = Core.Nothing,
      roleAliases = Core.Nothing,
      responseStatus
    }

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrarrsNextMarker :: Lens.Lens' ListRoleAliasesResponse (Core.Maybe Types.Marker)
lrarrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lrarrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The role aliases.
--
-- /Note:/ Consider using 'roleAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrarrsRoleAliases :: Lens.Lens' ListRoleAliasesResponse (Core.Maybe [Types.RoleAlias])
lrarrsRoleAliases = Lens.field @"roleAliases"
{-# DEPRECATED lrarrsRoleAliases "Use generic-lens or generic-optics with 'roleAliases' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrarrsResponseStatus :: Lens.Lens' ListRoleAliasesResponse Core.Int
lrarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
