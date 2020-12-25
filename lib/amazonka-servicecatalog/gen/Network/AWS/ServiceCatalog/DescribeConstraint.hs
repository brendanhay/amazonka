{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified constraint.
module Network.AWS.ServiceCatalog.DescribeConstraint
  ( -- * Creating a request
    DescribeConstraint (..),
    mkDescribeConstraint,

    -- ** Request lenses
    dId,
    dAcceptLanguage,

    -- * Destructuring the response
    DescribeConstraintResponse (..),
    mkDescribeConstraintResponse,

    -- ** Response lenses
    drsConstraintDetail,
    drsConstraintParameters,
    drsStatus,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeConstraint' smart constructor.
data DescribeConstraint = DescribeConstraint'
  { -- | The identifier of the constraint.
    id :: Types.Id,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Core.Maybe Types.AcceptLanguage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConstraint' value with any optional fields omitted.
mkDescribeConstraint ::
  -- | 'id'
  Types.Id ->
  DescribeConstraint
mkDescribeConstraint id =
  DescribeConstraint' {id, acceptLanguage = Core.Nothing}

-- | The identifier of the constraint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DescribeConstraint Types.Id
dId = Lens.field @"id"
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAcceptLanguage :: Lens.Lens' DescribeConstraint (Core.Maybe Types.AcceptLanguage)
dAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON DescribeConstraint where
  toJSON DescribeConstraint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest DescribeConstraint where
  type Rs DescribeConstraint = DescribeConstraintResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.DescribeConstraint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConstraintResponse'
            Core.<$> (x Core..:? "ConstraintDetail")
            Core.<*> (x Core..:? "ConstraintParameters")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeConstraintResponse' smart constructor.
data DescribeConstraintResponse = DescribeConstraintResponse'
  { -- | Information about the constraint.
    constraintDetail :: Core.Maybe Types.ConstraintDetail,
    -- | The constraint parameters.
    constraintParameters :: Core.Maybe Types.ConstraintParameters,
    -- | The status of the current request.
    status :: Core.Maybe Types.RequestStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConstraintResponse' value with any optional fields omitted.
mkDescribeConstraintResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConstraintResponse
mkDescribeConstraintResponse responseStatus =
  DescribeConstraintResponse'
    { constraintDetail = Core.Nothing,
      constraintParameters = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Information about the constraint.
--
-- /Note:/ Consider using 'constraintDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsConstraintDetail :: Lens.Lens' DescribeConstraintResponse (Core.Maybe Types.ConstraintDetail)
drsConstraintDetail = Lens.field @"constraintDetail"
{-# DEPRECATED drsConstraintDetail "Use generic-lens or generic-optics with 'constraintDetail' instead." #-}

-- | The constraint parameters.
--
-- /Note:/ Consider using 'constraintParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsConstraintParameters :: Lens.Lens' DescribeConstraintResponse (Core.Maybe Types.ConstraintParameters)
drsConstraintParameters = Lens.field @"constraintParameters"
{-# DEPRECATED drsConstraintParameters "Use generic-lens or generic-optics with 'constraintParameters' instead." #-}

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStatus :: Lens.Lens' DescribeConstraintResponse (Core.Maybe Types.RequestStatus)
drsStatus = Lens.field @"status"
{-# DEPRECATED drsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeConstraintResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
