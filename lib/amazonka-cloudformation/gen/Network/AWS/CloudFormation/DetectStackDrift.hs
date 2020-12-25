{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DetectStackDrift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For each resource in the stack that supports drift detection, AWS CloudFormation compares the actual configuration of the resource with its expected template configuration. Only resource properties explicitly defined in the stack template are checked for drift. A stack is considered to have drifted if one or more of its resources differ from their expected template configurations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- Use @DetectStackDrift@ to detect drift on all supported resources for a given stack, or 'DetectStackResourceDrift' to detect drift on individual resources.
-- For a list of stack resources that currently support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
-- @DetectStackDrift@ can take up to several minutes, depending on the number of resources contained within the stack. Use 'DescribeStackDriftDetectionStatus' to monitor the progress of a detect stack drift operation. Once the drift detection operation has completed, use 'DescribeStackResourceDrifts' to return drift information about the stack and its resources.
-- When detecting drift on a stack, AWS CloudFormation does not detect drift on any nested stacks belonging to that stack. Perform @DetectStackDrift@ directly on the nested stack itself.
module Network.AWS.CloudFormation.DetectStackDrift
  ( -- * Creating a request
    DetectStackDrift (..),
    mkDetectStackDrift,

    -- ** Request lenses
    dsdStackName,
    dsdLogicalResourceIds,

    -- * Destructuring the response
    DetectStackDriftResponse (..),
    mkDetectStackDriftResponse,

    -- ** Response lenses
    dsdrrsStackDriftDetectionId,
    dsdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectStackDrift' smart constructor.
data DetectStackDrift = DetectStackDrift'
  { -- | The name of the stack for which you want to detect drift.
    stackName :: Types.StackName,
    -- | The logical names of any resources you want to use as filters.
    logicalResourceIds :: Core.Maybe (Core.NonEmpty Types.LogicalResourceId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectStackDrift' value with any optional fields omitted.
mkDetectStackDrift ::
  -- | 'stackName'
  Types.StackName ->
  DetectStackDrift
mkDetectStackDrift stackName =
  DetectStackDrift' {stackName, logicalResourceIds = Core.Nothing}

-- | The name of the stack for which you want to detect drift.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdStackName :: Lens.Lens' DetectStackDrift Types.StackName
dsdStackName = Lens.field @"stackName"
{-# DEPRECATED dsdStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The logical names of any resources you want to use as filters.
--
-- /Note:/ Consider using 'logicalResourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdLogicalResourceIds :: Lens.Lens' DetectStackDrift (Core.Maybe (Core.NonEmpty Types.LogicalResourceId))
dsdLogicalResourceIds = Lens.field @"logicalResourceIds"
{-# DEPRECATED dsdLogicalResourceIds "Use generic-lens or generic-optics with 'logicalResourceIds' instead." #-}

instance Core.AWSRequest DetectStackDrift where
  type Rs DetectStackDrift = DetectStackDriftResponse
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
            ( Core.pure ("Action", "DetectStackDrift")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackName" stackName)
                Core.<> ( Core.toQueryValue
                            "LogicalResourceIds"
                            (Core.toQueryList "member" Core.<$> logicalResourceIds)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DetectStackDriftResult"
      ( \s h x ->
          DetectStackDriftResponse'
            Core.<$> (x Core..@ "StackDriftDetectionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetectStackDriftResponse' smart constructor.
data DetectStackDriftResponse = DetectStackDriftResponse'
  { -- | The ID of the drift detection results of this operation.
    --
    -- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
    stackDriftDetectionId :: Types.StackDriftDetectionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectStackDriftResponse' value with any optional fields omitted.
mkDetectStackDriftResponse ::
  -- | 'stackDriftDetectionId'
  Types.StackDriftDetectionId ->
  -- | 'responseStatus'
  Core.Int ->
  DetectStackDriftResponse
mkDetectStackDriftResponse stackDriftDetectionId responseStatus =
  DetectStackDriftResponse' {stackDriftDetectionId, responseStatus}

-- | The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
--
-- /Note:/ Consider using 'stackDriftDetectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrrsStackDriftDetectionId :: Lens.Lens' DetectStackDriftResponse Types.StackDriftDetectionId
dsdrrsStackDriftDetectionId = Lens.field @"stackDriftDetectionId"
{-# DEPRECATED dsdrrsStackDriftDetectionId "Use generic-lens or generic-optics with 'stackDriftDetectionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrrsResponseStatus :: Lens.Lens' DetectStackDriftResponse Core.Int
dsdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
