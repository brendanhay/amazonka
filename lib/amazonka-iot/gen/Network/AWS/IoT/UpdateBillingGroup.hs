{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the billing group.
module Network.AWS.IoT.UpdateBillingGroup
  ( -- * Creating a request
    UpdateBillingGroup (..),
    mkUpdateBillingGroup,

    -- ** Request lenses
    ubgBillingGroupName,
    ubgBillingGroupProperties,
    ubgExpectedVersion,

    -- * Destructuring the response
    UpdateBillingGroupResponse (..),
    mkUpdateBillingGroupResponse,

    -- ** Response lenses
    ubgrrsVersion,
    ubgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateBillingGroup' smart constructor.
data UpdateBillingGroup = UpdateBillingGroup'
  { -- | The name of the billing group.
    billingGroupName :: Types.BillingGroupName,
    -- | The properties of the billing group.
    billingGroupProperties :: Types.BillingGroupProperties,
    -- | The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @UpdateBillingGroup@ request is rejected with a @VersionConflictException@ .
    expectedVersion :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBillingGroup' value with any optional fields omitted.
mkUpdateBillingGroup ::
  -- | 'billingGroupName'
  Types.BillingGroupName ->
  -- | 'billingGroupProperties'
  Types.BillingGroupProperties ->
  UpdateBillingGroup
mkUpdateBillingGroup billingGroupName billingGroupProperties =
  UpdateBillingGroup'
    { billingGroupName,
      billingGroupProperties,
      expectedVersion = Core.Nothing
    }

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgBillingGroupName :: Lens.Lens' UpdateBillingGroup Types.BillingGroupName
ubgBillingGroupName = Lens.field @"billingGroupName"
{-# DEPRECATED ubgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The properties of the billing group.
--
-- /Note:/ Consider using 'billingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgBillingGroupProperties :: Lens.Lens' UpdateBillingGroup Types.BillingGroupProperties
ubgBillingGroupProperties = Lens.field @"billingGroupProperties"
{-# DEPRECATED ubgBillingGroupProperties "Use generic-lens or generic-optics with 'billingGroupProperties' instead." #-}

-- | The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @UpdateBillingGroup@ request is rejected with a @VersionConflictException@ .
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgExpectedVersion :: Lens.Lens' UpdateBillingGroup (Core.Maybe Core.Integer)
ubgExpectedVersion = Lens.field @"expectedVersion"
{-# DEPRECATED ubgExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

instance Core.FromJSON UpdateBillingGroup where
  toJSON UpdateBillingGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("billingGroupProperties" Core..= billingGroupProperties),
            ("expectedVersion" Core..=) Core.<$> expectedVersion
          ]
      )

instance Core.AWSRequest UpdateBillingGroup where
  type Rs UpdateBillingGroup = UpdateBillingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ("/billing-groups/" Core.<> (Core.toText billingGroupName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBillingGroupResponse'
            Core.<$> (x Core..:? "version") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateBillingGroupResponse' smart constructor.
data UpdateBillingGroupResponse = UpdateBillingGroupResponse'
  { -- | The latest version of the billing group.
    version :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBillingGroupResponse' value with any optional fields omitted.
mkUpdateBillingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateBillingGroupResponse
mkUpdateBillingGroupResponse responseStatus =
  UpdateBillingGroupResponse'
    { version = Core.Nothing,
      responseStatus
    }

-- | The latest version of the billing group.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgrrsVersion :: Lens.Lens' UpdateBillingGroupResponse (Core.Maybe Core.Integer)
ubgrrsVersion = Lens.field @"version"
{-# DEPRECATED ubgrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgrrsResponseStatus :: Lens.Lens' UpdateBillingGroupResponse Core.Int
ubgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ubgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
