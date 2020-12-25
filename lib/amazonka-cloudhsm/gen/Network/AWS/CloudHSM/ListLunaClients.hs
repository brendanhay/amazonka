{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ListLunaClients
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Lists all of the clients.
-- This operation supports pagination with the use of the @NextToken@ member. If more results are available, the @NextToken@ member of the response contains a token that you pass in the next call to @ListLunaClients@ to retrieve the next set of items.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSM.ListLunaClients
  ( -- * Creating a request
    ListLunaClients (..),
    mkListLunaClients,

    -- ** Request lenses
    llcNextToken,

    -- * Destructuring the response
    ListLunaClientsResponse (..),
    mkListLunaClientsResponse,

    -- ** Response lenses
    llcrrsClientList,
    llcrrsNextToken,
    llcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLunaClients' smart constructor.
newtype ListLunaClients = ListLunaClients'
  { -- | The @NextToken@ value from a previous call to @ListLunaClients@ . Pass null if this is the first call.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListLunaClients' value with any optional fields omitted.
mkListLunaClients ::
  ListLunaClients
mkListLunaClients = ListLunaClients' {nextToken = Core.Nothing}

-- | The @NextToken@ value from a previous call to @ListLunaClients@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcNextToken :: Lens.Lens' ListLunaClients (Core.Maybe Types.PaginationToken)
llcNextToken = Lens.field @"nextToken"
{-# DEPRECATED llcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListLunaClients where
  toJSON ListLunaClients {..} =
    Core.object
      (Core.catMaybes [("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListLunaClients where
  type Rs ListLunaClients = ListLunaClientsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CloudHsmFrontendService.ListLunaClients")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLunaClientsResponse'
            Core.<$> (x Core..:? "ClientList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLunaClients where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"clientList") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListLunaClientsResponse' smart constructor.
data ListLunaClientsResponse = ListLunaClientsResponse'
  { -- | The list of clients.
    clientList :: [Types.ClientArn],
    -- | If not null, more results are available. Pass this to @ListLunaClients@ to retrieve the next set of items.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLunaClientsResponse' value with any optional fields omitted.
mkListLunaClientsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLunaClientsResponse
mkListLunaClientsResponse responseStatus =
  ListLunaClientsResponse'
    { clientList = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of clients.
--
-- /Note:/ Consider using 'clientList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrrsClientList :: Lens.Lens' ListLunaClientsResponse [Types.ClientArn]
llcrrsClientList = Lens.field @"clientList"
{-# DEPRECATED llcrrsClientList "Use generic-lens or generic-optics with 'clientList' instead." #-}

-- | If not null, more results are available. Pass this to @ListLunaClients@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrrsNextToken :: Lens.Lens' ListLunaClientsResponse (Core.Maybe Types.PaginationToken)
llcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED llcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrrsResponseStatus :: Lens.Lens' ListLunaClientsResponse Core.Int
llcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED llcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
