{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DetectStackResourceDrift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about whether a resource's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. This information includes actual and expected property values for resources in which AWS CloudFormation detects drift. Only resource properties explicitly defined in the stack template are checked for drift. For more information about stack and resource drift, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- Use @DetectStackResourceDrift@ to detect drift on individual resources, or 'DetectStackDrift' to detect drift on all resources in a given stack that support drift detection.
-- Resources that do not currently support drift detection cannot be checked. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
module Network.AWS.CloudFormation.DetectStackResourceDrift
  ( -- * Creating a request
    DetectStackResourceDrift (..),
    mkDetectStackResourceDrift,

    -- ** Request lenses
    detStackName,
    detLogicalResourceId,

    -- * Destructuring the response
    DetectStackResourceDriftResponse (..),
    mkDetectStackResourceDriftResponse,

    -- ** Response lenses
    dsrdrsResponseStatus,
    dsrdrsStackResourceDrift,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectStackResourceDrift' smart constructor.
data DetectStackResourceDrift = DetectStackResourceDrift'
  { stackName ::
      Lude.Text,
    logicalResourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectStackResourceDrift' with the minimum fields required to make a request.
--
-- * 'logicalResourceId' - The logical name of the resource for which to return drift information.
-- * 'stackName' - The name of the stack to which the resource belongs.
mkDetectStackResourceDrift ::
  -- | 'stackName'
  Lude.Text ->
  -- | 'logicalResourceId'
  Lude.Text ->
  DetectStackResourceDrift
mkDetectStackResourceDrift pStackName_ pLogicalResourceId_ =
  DetectStackResourceDrift'
    { stackName = pStackName_,
      logicalResourceId = pLogicalResourceId_
    }

-- | The name of the stack to which the resource belongs.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detStackName :: Lens.Lens' DetectStackResourceDrift Lude.Text
detStackName = Lens.lens (stackName :: DetectStackResourceDrift -> Lude.Text) (\s a -> s {stackName = a} :: DetectStackResourceDrift)
{-# DEPRECATED detStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The logical name of the resource for which to return drift information.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detLogicalResourceId :: Lens.Lens' DetectStackResourceDrift Lude.Text
detLogicalResourceId = Lens.lens (logicalResourceId :: DetectStackResourceDrift -> Lude.Text) (\s a -> s {logicalResourceId = a} :: DetectStackResourceDrift)
{-# DEPRECATED detLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

instance Lude.AWSRequest DetectStackResourceDrift where
  type Rs DetectStackResourceDrift = DetectStackResourceDriftResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DetectStackResourceDriftResult"
      ( \s h x ->
          DetectStackResourceDriftResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "StackResourceDrift")
      )

instance Lude.ToHeaders DetectStackResourceDrift where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetectStackResourceDrift where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectStackResourceDrift where
  toQuery DetectStackResourceDrift' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetectStackResourceDrift" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "StackName" Lude.=: stackName,
        "LogicalResourceId" Lude.=: logicalResourceId
      ]

-- | /See:/ 'mkDetectStackResourceDriftResponse' smart constructor.
data DetectStackResourceDriftResponse = DetectStackResourceDriftResponse'
  { responseStatus ::
      Lude.Int,
    stackResourceDrift ::
      StackResourceDrift
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectStackResourceDriftResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stackResourceDrift' - Information about whether the resource's actual configuration has drifted from its expected template configuration, including actual and expected property values and any differences detected.
mkDetectStackResourceDriftResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'stackResourceDrift'
  StackResourceDrift ->
  DetectStackResourceDriftResponse
mkDetectStackResourceDriftResponse
  pResponseStatus_
  pStackResourceDrift_ =
    DetectStackResourceDriftResponse'
      { responseStatus =
          pResponseStatus_,
        stackResourceDrift = pStackResourceDrift_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdrsResponseStatus :: Lens.Lens' DetectStackResourceDriftResponse Lude.Int
dsrdrsResponseStatus = Lens.lens (responseStatus :: DetectStackResourceDriftResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectStackResourceDriftResponse)
{-# DEPRECATED dsrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about whether the resource's actual configuration has drifted from its expected template configuration, including actual and expected property values and any differences detected.
--
-- /Note:/ Consider using 'stackResourceDrift' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdrsStackResourceDrift :: Lens.Lens' DetectStackResourceDriftResponse StackResourceDrift
dsrdrsStackResourceDrift = Lens.lens (stackResourceDrift :: DetectStackResourceDriftResponse -> StackResourceDrift) (\s a -> s {stackResourceDrift = a} :: DetectStackResourceDriftResponse)
{-# DEPRECATED dsrdrsStackResourceDrift "Use generic-lens or generic-optics with 'stackResourceDrift' instead." #-}
