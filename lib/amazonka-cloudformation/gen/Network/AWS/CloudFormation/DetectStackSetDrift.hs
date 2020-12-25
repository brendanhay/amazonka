{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DetectStackSetDrift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detect drift on a stack set. When CloudFormation performs drift detection on a stack set, it performs drift detection on the stack associated with each stack instance in the stack set. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html How CloudFormation Performs Drift Detection on a Stack Set> .
--
-- @DetectStackSetDrift@ returns the @OperationId@ of the stack set drift detection operation. Use this operation id with @'DescribeStackSetOperation' @ to monitor the progress of the drift detection operation. The drift detection operation may take some time, depending on the number of stack instances included in the stack set, as well as the number of resources included in each stack.
-- Once the operation has completed, use the following actions to return drift information:
--
--     * Use @'DescribeStackSet' @ to return detailed informaiton about the stack set, including detailed information about the last /completed/ drift operation performed on the stack set. (Information about drift operations that are in progress is not included.)
--
--
--     * Use @'ListStackInstances' @ to return a list of stack instances belonging to the stack set, including the drift status and last drift time checked of each instance.
--
--
--     * Use @'DescribeStackInstance' @ to return detailed information about a specific stack instance, including its drift status and last drift time checked.
--
--
-- For more information on performing a drift detection operation on a stack set, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> .
-- You can only run a single drift detection operation on a given stack set at one time.
-- To stop a drift detection stack set operation, use @'StopStackSetOperation' @ .
module Network.AWS.CloudFormation.DetectStackSetDrift
  ( -- * Creating a request
    DetectStackSetDrift (..),
    mkDetectStackSetDrift,

    -- ** Request lenses
    dssdStackSetName,
    dssdOperationId,
    dssdOperationPreferences,

    -- * Destructuring the response
    DetectStackSetDriftResponse (..),
    mkDetectStackSetDriftResponse,

    -- ** Response lenses
    dssdrrsOperationId,
    dssdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectStackSetDrift' smart constructor.
data DetectStackSetDrift = DetectStackSetDrift'
  { -- | The name of the stack set on which to perform the drift detection operation.
    stackSetName :: Types.StackSetName,
    -- | /The ID of the stack set operation./
    operationId :: Core.Maybe Types.OperationId,
    operationPreferences :: Core.Maybe Types.StackSetOperationPreferences
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectStackSetDrift' value with any optional fields omitted.
mkDetectStackSetDrift ::
  -- | 'stackSetName'
  Types.StackSetName ->
  DetectStackSetDrift
mkDetectStackSetDrift stackSetName =
  DetectStackSetDrift'
    { stackSetName,
      operationId = Core.Nothing,
      operationPreferences = Core.Nothing
    }

-- | The name of the stack set on which to perform the drift detection operation.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdStackSetName :: Lens.Lens' DetectStackSetDrift Types.StackSetName
dssdStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED dssdStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | /The ID of the stack set operation./
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdOperationId :: Lens.Lens' DetectStackSetDrift (Core.Maybe Types.OperationId)
dssdOperationId = Lens.field @"operationId"
{-# DEPRECATED dssdOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdOperationPreferences :: Lens.Lens' DetectStackSetDrift (Core.Maybe Types.StackSetOperationPreferences)
dssdOperationPreferences = Lens.field @"operationPreferences"
{-# DEPRECATED dssdOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

instance Core.AWSRequest DetectStackSetDrift where
  type Rs DetectStackSetDrift = DetectStackSetDriftResponse
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
            ( Core.pure ("Action", "DetectStackSetDrift")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
                Core.<> (Core.toQueryValue "OperationId" Core.<$> operationId)
                Core.<> ( Core.toQueryValue "OperationPreferences"
                            Core.<$> operationPreferences
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DetectStackSetDriftResult"
      ( \s h x ->
          DetectStackSetDriftResponse'
            Core.<$> (x Core..@? "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetectStackSetDriftResponse' smart constructor.
data DetectStackSetDriftResponse = DetectStackSetDriftResponse'
  { -- | The ID of the drift detection stack set operation.
    --
    -- you can use this operation id with @'DescribeStackSetOperation' @ to monitor the progress of the drift detection operation.
    operationId :: Core.Maybe Types.ClientRequestToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectStackSetDriftResponse' value with any optional fields omitted.
mkDetectStackSetDriftResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetectStackSetDriftResponse
mkDetectStackSetDriftResponse responseStatus =
  DetectStackSetDriftResponse'
    { operationId = Core.Nothing,
      responseStatus
    }

-- | The ID of the drift detection stack set operation.
--
-- you can use this operation id with @'DescribeStackSetOperation' @ to monitor the progress of the drift detection operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdrrsOperationId :: Lens.Lens' DetectStackSetDriftResponse (Core.Maybe Types.ClientRequestToken)
dssdrrsOperationId = Lens.field @"operationId"
{-# DEPRECATED dssdrrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdrrsResponseStatus :: Lens.Lens' DetectStackSetDriftResponse Core.Int
dssdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dssdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
