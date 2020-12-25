{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeletePendingAggregationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes pending authorization requests for a specified aggregator account in a specified region.
module Network.AWS.Config.DeletePendingAggregationRequest
  ( -- * Creating a request
    DeletePendingAggregationRequest (..),
    mkDeletePendingAggregationRequest,

    -- ** Request lenses
    dparRequesterAccountId,
    dparRequesterAwsRegion,

    -- * Destructuring the response
    DeletePendingAggregationRequestResponse (..),
    mkDeletePendingAggregationRequestResponse,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePendingAggregationRequest' smart constructor.
data DeletePendingAggregationRequest = DeletePendingAggregationRequest'
  { -- | The 12-digit account ID of the account requesting to aggregate data.
    requesterAccountId :: Types.RequesterAccountId,
    -- | The region requesting to aggregate data.
    requesterAwsRegion :: Types.RequesterAwsRegion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePendingAggregationRequest' value with any optional fields omitted.
mkDeletePendingAggregationRequest ::
  -- | 'requesterAccountId'
  Types.RequesterAccountId ->
  -- | 'requesterAwsRegion'
  Types.RequesterAwsRegion ->
  DeletePendingAggregationRequest
mkDeletePendingAggregationRequest
  requesterAccountId
  requesterAwsRegion =
    DeletePendingAggregationRequest'
      { requesterAccountId,
        requesterAwsRegion
      }

-- | The 12-digit account ID of the account requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparRequesterAccountId :: Lens.Lens' DeletePendingAggregationRequest Types.RequesterAccountId
dparRequesterAccountId = Lens.field @"requesterAccountId"
{-# DEPRECATED dparRequesterAccountId "Use generic-lens or generic-optics with 'requesterAccountId' instead." #-}

-- | The region requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAwsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparRequesterAwsRegion :: Lens.Lens' DeletePendingAggregationRequest Types.RequesterAwsRegion
dparRequesterAwsRegion = Lens.field @"requesterAwsRegion"
{-# DEPRECATED dparRequesterAwsRegion "Use generic-lens or generic-optics with 'requesterAwsRegion' instead." #-}

instance Core.FromJSON DeletePendingAggregationRequest where
  toJSON DeletePendingAggregationRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RequesterAccountId" Core..= requesterAccountId),
            Core.Just ("RequesterAwsRegion" Core..= requesterAwsRegion)
          ]
      )

instance Core.AWSRequest DeletePendingAggregationRequest where
  type
    Rs DeletePendingAggregationRequest =
      DeletePendingAggregationRequestResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DeletePendingAggregationRequest"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull DeletePendingAggregationRequestResponse'

-- | /See:/ 'mkDeletePendingAggregationRequestResponse' smart constructor.
data DeletePendingAggregationRequestResponse = DeletePendingAggregationRequestResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePendingAggregationRequestResponse' value with any optional fields omitted.
mkDeletePendingAggregationRequestResponse ::
  DeletePendingAggregationRequestResponse
mkDeletePendingAggregationRequestResponse =
  DeletePendingAggregationRequestResponse'
