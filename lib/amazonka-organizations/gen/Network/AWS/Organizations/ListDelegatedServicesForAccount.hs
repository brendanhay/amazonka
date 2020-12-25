{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListDelegatedServicesForAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the AWS services for which the specified account is a delegated administrator.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListDelegatedServicesForAccount
  ( -- * Creating a request
    ListDelegatedServicesForAccount (..),
    mkListDelegatedServicesForAccount,

    -- ** Request lenses
    ldsfaAccountId,
    ldsfaMaxResults,
    ldsfaNextToken,

    -- * Destructuring the response
    ListDelegatedServicesForAccountResponse (..),
    mkListDelegatedServicesForAccountResponse,

    -- ** Response lenses
    ldsfarrsDelegatedServices,
    ldsfarrsNextToken,
    ldsfarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDelegatedServicesForAccount' smart constructor.
data ListDelegatedServicesForAccount = ListDelegatedServicesForAccount'
  { -- | The account ID number of a delegated administrator account in the organization.
    accountId :: Types.AccountId,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDelegatedServicesForAccount' value with any optional fields omitted.
mkListDelegatedServicesForAccount ::
  -- | 'accountId'
  Types.AccountId ->
  ListDelegatedServicesForAccount
mkListDelegatedServicesForAccount accountId =
  ListDelegatedServicesForAccount'
    { accountId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The account ID number of a delegated administrator account in the organization.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfaAccountId :: Lens.Lens' ListDelegatedServicesForAccount Types.AccountId
ldsfaAccountId = Lens.field @"accountId"
{-# DEPRECATED ldsfaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfaMaxResults :: Lens.Lens' ListDelegatedServicesForAccount (Core.Maybe Core.Natural)
ldsfaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldsfaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfaNextToken :: Lens.Lens' ListDelegatedServicesForAccount (Core.Maybe Types.NextToken)
ldsfaNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldsfaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListDelegatedServicesForAccount where
  toJSON ListDelegatedServicesForAccount {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListDelegatedServicesForAccount where
  type
    Rs ListDelegatedServicesForAccount =
      ListDelegatedServicesForAccountResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrganizationsV20161128.ListDelegatedServicesForAccount"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDelegatedServicesForAccountResponse'
            Core.<$> (x Core..:? "DelegatedServices")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDelegatedServicesForAccount where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"delegatedServices" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDelegatedServicesForAccountResponse' smart constructor.
data ListDelegatedServicesForAccountResponse = ListDelegatedServicesForAccountResponse'
  { -- | The services for which the account is a delegated administrator.
    delegatedServices :: Core.Maybe [Types.DelegatedService],
    -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDelegatedServicesForAccountResponse' value with any optional fields omitted.
mkListDelegatedServicesForAccountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDelegatedServicesForAccountResponse
mkListDelegatedServicesForAccountResponse responseStatus =
  ListDelegatedServicesForAccountResponse'
    { delegatedServices =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The services for which the account is a delegated administrator.
--
-- /Note:/ Consider using 'delegatedServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfarrsDelegatedServices :: Lens.Lens' ListDelegatedServicesForAccountResponse (Core.Maybe [Types.DelegatedService])
ldsfarrsDelegatedServices = Lens.field @"delegatedServices"
{-# DEPRECATED ldsfarrsDelegatedServices "Use generic-lens or generic-optics with 'delegatedServices' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfarrsNextToken :: Lens.Lens' ListDelegatedServicesForAccountResponse (Core.Maybe Types.NextToken)
ldsfarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldsfarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfarrsResponseStatus :: Lens.Lens' ListDelegatedServicesForAccountResponse Core.Int
ldsfarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldsfarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
