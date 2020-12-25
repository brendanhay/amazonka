{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListActionTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all AWS CodePipeline action types associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListActionTypes
  ( -- * Creating a request
    ListActionTypes (..),
    mkListActionTypes,

    -- ** Request lenses
    latActionOwnerFilter,
    latNextToken,

    -- * Destructuring the response
    ListActionTypesResponse (..),
    mkListActionTypesResponse,

    -- ** Response lenses
    latrrsActionTypes,
    latrrsNextToken,
    latrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListActionTypes@ action.
--
-- /See:/ 'mkListActionTypes' smart constructor.
data ListActionTypes = ListActionTypes'
  { -- | Filters the list of action types to those created by a specified entity.
    actionOwnerFilter :: Core.Maybe Types.ActionOwner,
    -- | An identifier that was returned from the previous list action types call, which can be used to return the next set of action types in the list.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListActionTypes' value with any optional fields omitted.
mkListActionTypes ::
  ListActionTypes
mkListActionTypes =
  ListActionTypes'
    { actionOwnerFilter = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filters the list of action types to those created by a specified entity.
--
-- /Note:/ Consider using 'actionOwnerFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latActionOwnerFilter :: Lens.Lens' ListActionTypes (Core.Maybe Types.ActionOwner)
latActionOwnerFilter = Lens.field @"actionOwnerFilter"
{-# DEPRECATED latActionOwnerFilter "Use generic-lens or generic-optics with 'actionOwnerFilter' instead." #-}

-- | An identifier that was returned from the previous list action types call, which can be used to return the next set of action types in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latNextToken :: Lens.Lens' ListActionTypes (Core.Maybe Types.NextToken)
latNextToken = Lens.field @"nextToken"
{-# DEPRECATED latNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListActionTypes where
  toJSON ListActionTypes {..} =
    Core.object
      ( Core.catMaybes
          [ ("actionOwnerFilter" Core..=) Core.<$> actionOwnerFilter,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListActionTypes where
  type Rs ListActionTypes = ListActionTypesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodePipeline_20150709.ListActionTypes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActionTypesResponse'
            Core.<$> (x Core..:? "actionTypes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListActionTypes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"actionTypes") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the output of a @ListActionTypes@ action.
--
-- /See:/ 'mkListActionTypesResponse' smart constructor.
data ListActionTypesResponse = ListActionTypesResponse'
  { -- | Provides details of the action types.
    actionTypes :: [Types.ActionType],
    -- | If the amount of returned information is significantly large, an identifier is also returned. It can be used in a subsequent list action types call to return the next set of action types in the list.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListActionTypesResponse' value with any optional fields omitted.
mkListActionTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListActionTypesResponse
mkListActionTypesResponse responseStatus =
  ListActionTypesResponse'
    { actionTypes = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Provides details of the action types.
--
-- /Note:/ Consider using 'actionTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsActionTypes :: Lens.Lens' ListActionTypesResponse [Types.ActionType]
latrrsActionTypes = Lens.field @"actionTypes"
{-# DEPRECATED latrrsActionTypes "Use generic-lens or generic-optics with 'actionTypes' instead." #-}

-- | If the amount of returned information is significantly large, an identifier is also returned. It can be used in a subsequent list action types call to return the next set of action types in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsNextToken :: Lens.Lens' ListActionTypesResponse (Core.Maybe Types.NextToken)
latrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED latrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsResponseStatus :: Lens.Lens' ListActionTypesResponse Core.Int
latrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED latrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
