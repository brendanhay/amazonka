{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetActionsForAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all of the budget actions for an account.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionsForAccount
  ( -- * Creating a request
    DescribeBudgetActionsForAccount (..),
    mkDescribeBudgetActionsForAccount,

    -- ** Request lenses
    dbafaAccountId,
    dbafaMaxResults,
    dbafaNextToken,

    -- * Destructuring the response
    DescribeBudgetActionsForAccountResponse (..),
    mkDescribeBudgetActionsForAccountResponse,

    -- ** Response lenses
    dbafarrsActions,
    dbafarrsNextToken,
    dbafarrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBudgetActionsForAccount' smart constructor.
data DescribeBudgetActionsForAccount = DescribeBudgetActionsForAccount'
  { accountId :: Types.AccountId,
    maxResults :: Core.Maybe Core.Natural,
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudgetActionsForAccount' value with any optional fields omitted.
mkDescribeBudgetActionsForAccount ::
  -- | 'accountId'
  Types.AccountId ->
  DescribeBudgetActionsForAccount
mkDescribeBudgetActionsForAccount accountId =
  DescribeBudgetActionsForAccount'
    { accountId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafaAccountId :: Lens.Lens' DescribeBudgetActionsForAccount Types.AccountId
dbafaAccountId = Lens.field @"accountId"
{-# DEPRECATED dbafaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafaMaxResults :: Lens.Lens' DescribeBudgetActionsForAccount (Core.Maybe Core.Natural)
dbafaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dbafaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafaNextToken :: Lens.Lens' DescribeBudgetActionsForAccount (Core.Maybe Types.NextToken)
dbafaNextToken = Lens.field @"nextToken"
{-# DEPRECATED dbafaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeBudgetActionsForAccount where
  toJSON DescribeBudgetActionsForAccount {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeBudgetActionsForAccount where
  type
    Rs DescribeBudgetActionsForAccount =
      DescribeBudgetActionsForAccountResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSBudgetServiceGateway.DescribeBudgetActionsForAccount"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionsForAccountResponse'
            Core.<$> (x Core..:? "Actions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeBudgetActionsForAccount where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"actions") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeBudgetActionsForAccountResponse' smart constructor.
data DescribeBudgetActionsForAccountResponse = DescribeBudgetActionsForAccountResponse'
  { -- | A list of the budget action resources information.
    actions :: [Types.Action],
    nextToken :: Core.Maybe Types.GenericString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudgetActionsForAccountResponse' value with any optional fields omitted.
mkDescribeBudgetActionsForAccountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBudgetActionsForAccountResponse
mkDescribeBudgetActionsForAccountResponse responseStatus =
  DescribeBudgetActionsForAccountResponse'
    { actions = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of the budget action resources information.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafarrsActions :: Lens.Lens' DescribeBudgetActionsForAccountResponse [Types.Action]
dbafarrsActions = Lens.field @"actions"
{-# DEPRECATED dbafarrsActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafarrsNextToken :: Lens.Lens' DescribeBudgetActionsForAccountResponse (Core.Maybe Types.GenericString)
dbafarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dbafarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafarrsResponseStatus :: Lens.Lens' DescribeBudgetActionsForAccountResponse Core.Int
dbafarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbafarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
