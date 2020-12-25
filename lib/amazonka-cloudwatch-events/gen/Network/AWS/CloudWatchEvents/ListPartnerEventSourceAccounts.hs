{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListPartnerEventSourceAccounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to display the AWS account ID that a particular partner event source name is associated with. This operation is not used by AWS customers.
module Network.AWS.CloudWatchEvents.ListPartnerEventSourceAccounts
  ( -- * Creating a request
    ListPartnerEventSourceAccounts (..),
    mkListPartnerEventSourceAccounts,

    -- ** Request lenses
    lpesaEventSourceName,
    lpesaLimit,
    lpesaNextToken,

    -- * Destructuring the response
    ListPartnerEventSourceAccountsResponse (..),
    mkListPartnerEventSourceAccountsResponse,

    -- ** Response lenses
    lpesarrsNextToken,
    lpesarrsPartnerEventSourceAccounts,
    lpesarrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPartnerEventSourceAccounts' smart constructor.
data ListPartnerEventSourceAccounts = ListPartnerEventSourceAccounts'
  { -- | The name of the partner event source to display account information about.
    eventSourceName :: Types.EventSourceName,
    -- | Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
    limit :: Core.Maybe Core.Natural,
    -- | The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPartnerEventSourceAccounts' value with any optional fields omitted.
mkListPartnerEventSourceAccounts ::
  -- | 'eventSourceName'
  Types.EventSourceName ->
  ListPartnerEventSourceAccounts
mkListPartnerEventSourceAccounts eventSourceName =
  ListPartnerEventSourceAccounts'
    { eventSourceName,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the partner event source to display account information about.
--
-- /Note:/ Consider using 'eventSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesaEventSourceName :: Lens.Lens' ListPartnerEventSourceAccounts Types.EventSourceName
lpesaEventSourceName = Lens.field @"eventSourceName"
{-# DEPRECATED lpesaEventSourceName "Use generic-lens or generic-optics with 'eventSourceName' instead." #-}

-- | Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesaLimit :: Lens.Lens' ListPartnerEventSourceAccounts (Core.Maybe Core.Natural)
lpesaLimit = Lens.field @"limit"
{-# DEPRECATED lpesaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesaNextToken :: Lens.Lens' ListPartnerEventSourceAccounts (Core.Maybe Types.NextToken)
lpesaNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpesaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListPartnerEventSourceAccounts where
  toJSON ListPartnerEventSourceAccounts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EventSourceName" Core..= eventSourceName),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListPartnerEventSourceAccounts where
  type
    Rs ListPartnerEventSourceAccounts =
      ListPartnerEventSourceAccountsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSEvents.ListPartnerEventSourceAccounts")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPartnerEventSourceAccountsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PartnerEventSourceAccounts")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListPartnerEventSourceAccountsResponse' smart constructor.
data ListPartnerEventSourceAccountsResponse = ListPartnerEventSourceAccountsResponse'
  { -- | A token you can use in a subsequent operation to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The list of partner event sources returned by the operation.
    partnerEventSourceAccounts :: Core.Maybe [Types.PartnerEventSourceAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListPartnerEventSourceAccountsResponse' value with any optional fields omitted.
mkListPartnerEventSourceAccountsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPartnerEventSourceAccountsResponse
mkListPartnerEventSourceAccountsResponse responseStatus =
  ListPartnerEventSourceAccountsResponse'
    { nextToken = Core.Nothing,
      partnerEventSourceAccounts = Core.Nothing,
      responseStatus
    }

-- | A token you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesarrsNextToken :: Lens.Lens' ListPartnerEventSourceAccountsResponse (Core.Maybe Types.NextToken)
lpesarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpesarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of partner event sources returned by the operation.
--
-- /Note:/ Consider using 'partnerEventSourceAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesarrsPartnerEventSourceAccounts :: Lens.Lens' ListPartnerEventSourceAccountsResponse (Core.Maybe [Types.PartnerEventSourceAccount])
lpesarrsPartnerEventSourceAccounts = Lens.field @"partnerEventSourceAccounts"
{-# DEPRECATED lpesarrsPartnerEventSourceAccounts "Use generic-lens or generic-optics with 'partnerEventSourceAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesarrsResponseStatus :: Lens.Lens' ListPartnerEventSourceAccountsResponse Core.Int
lpesarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpesarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
