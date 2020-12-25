{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a DB instance).
module Network.AWS.RDS.ApplyPendingMaintenanceAction
  ( -- * Creating a request
    ApplyPendingMaintenanceAction (..),
    mkApplyPendingMaintenanceAction,

    -- ** Request lenses
    apmaResourceIdentifier,
    apmaApplyAction,
    apmaOptInType,

    -- * Destructuring the response
    ApplyPendingMaintenanceActionResponse (..),
    mkApplyPendingMaintenanceActionResponse,

    -- ** Response lenses
    apmarrsResourcePendingMaintenanceActions,
    apmarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkApplyPendingMaintenanceAction' smart constructor.
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
  { -- | The RDS Amazon Resource Name (ARN) of the resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
    resourceIdentifier :: Types.String,
    -- | The pending maintenance action to apply to this resource.
    --
    -- Valid values: @system-update@ , @db-upgrade@ , @hardware-maintenance@ , @ca-certificate-rotation@
    applyAction :: Types.String,
    -- | A value that specifies the type of opt-in request, or undoes an opt-in request. An opt-in request of type @immediate@ can't be undone.
    --
    -- Valid values:
    --
    --     * @immediate@ - Apply the maintenance action immediately.
    --
    --
    --     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.
    --
    --
    --     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
    optInType :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplyPendingMaintenanceAction' value with any optional fields omitted.
mkApplyPendingMaintenanceAction ::
  -- | 'resourceIdentifier'
  Types.String ->
  -- | 'applyAction'
  Types.String ->
  -- | 'optInType'
  Types.String ->
  ApplyPendingMaintenanceAction
mkApplyPendingMaintenanceAction
  resourceIdentifier
  applyAction
  optInType =
    ApplyPendingMaintenanceAction'
      { resourceIdentifier,
        applyAction,
        optInType
      }

-- | The RDS Amazon Resource Name (ARN) of the resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaResourceIdentifier :: Lens.Lens' ApplyPendingMaintenanceAction Types.String
apmaResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED apmaResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | The pending maintenance action to apply to this resource.
--
-- Valid values: @system-update@ , @db-upgrade@ , @hardware-maintenance@ , @ca-certificate-rotation@
--
-- /Note:/ Consider using 'applyAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaApplyAction :: Lens.Lens' ApplyPendingMaintenanceAction Types.String
apmaApplyAction = Lens.field @"applyAction"
{-# DEPRECATED apmaApplyAction "Use generic-lens or generic-optics with 'applyAction' instead." #-}

-- | A value that specifies the type of opt-in request, or undoes an opt-in request. An opt-in request of type @immediate@ can't be undone.
--
-- Valid values:
--
--     * @immediate@ - Apply the maintenance action immediately.
--
--
--     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.
--
--
--     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
--
--
--
-- /Note:/ Consider using 'optInType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaOptInType :: Lens.Lens' ApplyPendingMaintenanceAction Types.String
apmaOptInType = Lens.field @"optInType"
{-# DEPRECATED apmaOptInType "Use generic-lens or generic-optics with 'optInType' instead." #-}

instance Core.AWSRequest ApplyPendingMaintenanceAction where
  type
    Rs ApplyPendingMaintenanceAction =
      ApplyPendingMaintenanceActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ApplyPendingMaintenanceAction")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "ResourceIdentifier" resourceIdentifier)
                Core.<> (Core.toQueryValue "ApplyAction" applyAction)
                Core.<> (Core.toQueryValue "OptInType" optInType)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ApplyPendingMaintenanceActionResult"
      ( \s h x ->
          ApplyPendingMaintenanceActionResponse'
            Core.<$> (x Core..@? "ResourcePendingMaintenanceActions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkApplyPendingMaintenanceActionResponse' smart constructor.
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
  { resourcePendingMaintenanceActions :: Core.Maybe Types.ResourcePendingMaintenanceActions,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ApplyPendingMaintenanceActionResponse' value with any optional fields omitted.
mkApplyPendingMaintenanceActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ApplyPendingMaintenanceActionResponse
mkApplyPendingMaintenanceActionResponse responseStatus =
  ApplyPendingMaintenanceActionResponse'
    { resourcePendingMaintenanceActions =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourcePendingMaintenanceActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmarrsResourcePendingMaintenanceActions :: Lens.Lens' ApplyPendingMaintenanceActionResponse (Core.Maybe Types.ResourcePendingMaintenanceActions)
apmarrsResourcePendingMaintenanceActions = Lens.field @"resourcePendingMaintenanceActions"
{-# DEPRECATED apmarrsResourcePendingMaintenanceActions "Use generic-lens or generic-optics with 'resourcePendingMaintenanceActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmarrsResponseStatus :: Lens.Lens' ApplyPendingMaintenanceActionResponse Core.Int
apmarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED apmarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
