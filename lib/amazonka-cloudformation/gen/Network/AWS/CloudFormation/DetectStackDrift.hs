{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dsdLogicalResourceIds,
    dsdStackName,

    -- * Destructuring the response
    DetectStackDriftResponse (..),
    mkDetectStackDriftResponse,

    -- ** Response lenses
    dsdrsResponseStatus,
    dsdrsStackDriftDetectionId,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectStackDrift' smart constructor.
data DetectStackDrift = DetectStackDrift'
  { logicalResourceIds ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    stackName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectStackDrift' with the minimum fields required to make a request.
--
-- * 'logicalResourceIds' - The logical names of any resources you want to use as filters.
-- * 'stackName' - The name of the stack for which you want to detect drift.
mkDetectStackDrift ::
  -- | 'stackName'
  Lude.Text ->
  DetectStackDrift
mkDetectStackDrift pStackName_ =
  DetectStackDrift'
    { logicalResourceIds = Lude.Nothing,
      stackName = pStackName_
    }

-- | The logical names of any resources you want to use as filters.
--
-- /Note:/ Consider using 'logicalResourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdLogicalResourceIds :: Lens.Lens' DetectStackDrift (Lude.Maybe (Lude.NonEmpty Lude.Text))
dsdLogicalResourceIds = Lens.lens (logicalResourceIds :: DetectStackDrift -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {logicalResourceIds = a} :: DetectStackDrift)
{-# DEPRECATED dsdLogicalResourceIds "Use generic-lens or generic-optics with 'logicalResourceIds' instead." #-}

-- | The name of the stack for which you want to detect drift.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdStackName :: Lens.Lens' DetectStackDrift Lude.Text
dsdStackName = Lens.lens (stackName :: DetectStackDrift -> Lude.Text) (\s a -> s {stackName = a} :: DetectStackDrift)
{-# DEPRECATED dsdStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest DetectStackDrift where
  type Rs DetectStackDrift = DetectStackDriftResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DetectStackDriftResult"
      ( \s h x ->
          DetectStackDriftResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "StackDriftDetectionId")
      )

instance Lude.ToHeaders DetectStackDrift where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetectStackDrift where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectStackDrift where
  toQuery DetectStackDrift' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetectStackDrift" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "LogicalResourceIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> logicalResourceIds),
        "StackName" Lude.=: stackName
      ]

-- | /See:/ 'mkDetectStackDriftResponse' smart constructor.
data DetectStackDriftResponse = DetectStackDriftResponse'
  { responseStatus ::
      Lude.Int,
    stackDriftDetectionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectStackDriftResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stackDriftDetectionId' - The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
mkDetectStackDriftResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'stackDriftDetectionId'
  Lude.Text ->
  DetectStackDriftResponse
mkDetectStackDriftResponse pResponseStatus_ pStackDriftDetectionId_ =
  DetectStackDriftResponse'
    { responseStatus = pResponseStatus_,
      stackDriftDetectionId = pStackDriftDetectionId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrsResponseStatus :: Lens.Lens' DetectStackDriftResponse Lude.Int
dsdrsResponseStatus = Lens.lens (responseStatus :: DetectStackDriftResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectStackDriftResponse)
{-# DEPRECATED dsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
--
-- /Note:/ Consider using 'stackDriftDetectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrsStackDriftDetectionId :: Lens.Lens' DetectStackDriftResponse Lude.Text
dsdrsStackDriftDetectionId = Lens.lens (stackDriftDetectionId :: DetectStackDriftResponse -> Lude.Text) (\s a -> s {stackDriftDetectionId = a} :: DetectStackDriftResponse)
{-# DEPRECATED dsdrsStackDriftDetectionId "Use generic-lens or generic-optics with 'stackDriftDetectionId' instead." #-}
