{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteAggregationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the authorization granted to the specified configuration aggregator account in a specified region.
module Network.AWS.Config.DeleteAggregationAuthorization
  ( -- * Creating a request
    DeleteAggregationAuthorization (..),
    mkDeleteAggregationAuthorization,

    -- ** Request lenses
    daaAuthorizedAccountId,
    daaAuthorizedAwsRegion,

    -- * Destructuring the response
    DeleteAggregationAuthorizationResponse (..),
    mkDeleteAggregationAuthorizationResponse,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAggregationAuthorization' smart constructor.
data DeleteAggregationAuthorization = DeleteAggregationAuthorization'
  { -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Types.AccountId,
    -- | The region authorized to collect aggregated data.
    authorizedAwsRegion :: Types.AuthorizedAwsRegion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAggregationAuthorization' value with any optional fields omitted.
mkDeleteAggregationAuthorization ::
  -- | 'authorizedAccountId'
  Types.AccountId ->
  -- | 'authorizedAwsRegion'
  Types.AuthorizedAwsRegion ->
  DeleteAggregationAuthorization
mkDeleteAggregationAuthorization
  authorizedAccountId
  authorizedAwsRegion =
    DeleteAggregationAuthorization'
      { authorizedAccountId,
        authorizedAwsRegion
      }

-- | The 12-digit account ID of the account authorized to aggregate data.
--
-- /Note:/ Consider using 'authorizedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAuthorizedAccountId :: Lens.Lens' DeleteAggregationAuthorization Types.AccountId
daaAuthorizedAccountId = Lens.field @"authorizedAccountId"
{-# DEPRECATED daaAuthorizedAccountId "Use generic-lens or generic-optics with 'authorizedAccountId' instead." #-}

-- | The region authorized to collect aggregated data.
--
-- /Note:/ Consider using 'authorizedAwsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAuthorizedAwsRegion :: Lens.Lens' DeleteAggregationAuthorization Types.AuthorizedAwsRegion
daaAuthorizedAwsRegion = Lens.field @"authorizedAwsRegion"
{-# DEPRECATED daaAuthorizedAwsRegion "Use generic-lens or generic-optics with 'authorizedAwsRegion' instead." #-}

instance Core.FromJSON DeleteAggregationAuthorization where
  toJSON DeleteAggregationAuthorization {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AuthorizedAccountId" Core..= authorizedAccountId),
            Core.Just ("AuthorizedAwsRegion" Core..= authorizedAwsRegion)
          ]
      )

instance Core.AWSRequest DeleteAggregationAuthorization where
  type
    Rs DeleteAggregationAuthorization =
      DeleteAggregationAuthorizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DeleteAggregationAuthorization"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull DeleteAggregationAuthorizationResponse'

-- | /See:/ 'mkDeleteAggregationAuthorizationResponse' smart constructor.
data DeleteAggregationAuthorizationResponse = DeleteAggregationAuthorizationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAggregationAuthorizationResponse' value with any optional fields omitted.
mkDeleteAggregationAuthorizationResponse ::
  DeleteAggregationAuthorizationResponse
mkDeleteAggregationAuthorizationResponse =
  DeleteAggregationAuthorizationResponse'
