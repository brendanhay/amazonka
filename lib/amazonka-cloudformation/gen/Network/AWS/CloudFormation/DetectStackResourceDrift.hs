{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dsrdLogicalResourceId,
    dsrdStackName,

    -- * Destructuring the response
    DetectStackResourceDriftResponse (..),
    mkDetectStackResourceDriftResponse,

    -- ** Response lenses
    dsrdfrsStackResourceDrift,
    dsrdfrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectStackResourceDrift' smart constructor.
data DetectStackResourceDrift = DetectStackResourceDrift'
  { -- | The logical name of the resource for which to return drift information.
    logicalResourceId :: Lude.Text,
    -- | The name of the stack to which the resource belongs.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectStackResourceDrift' with the minimum fields required to make a request.
--
-- * 'logicalResourceId' - The logical name of the resource for which to return drift information.
-- * 'stackName' - The name of the stack to which the resource belongs.
mkDetectStackResourceDrift ::
  -- | 'logicalResourceId'
  Lude.Text ->
  -- | 'stackName'
  Lude.Text ->
  DetectStackResourceDrift
mkDetectStackResourceDrift pLogicalResourceId_ pStackName_ =
  DetectStackResourceDrift'
    { logicalResourceId =
        pLogicalResourceId_,
      stackName = pStackName_
    }

-- | The logical name of the resource for which to return drift information.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdLogicalResourceId :: Lens.Lens' DetectStackResourceDrift Lude.Text
dsrdLogicalResourceId = Lens.lens (logicalResourceId :: DetectStackResourceDrift -> Lude.Text) (\s a -> s {logicalResourceId = a} :: DetectStackResourceDrift)
{-# DEPRECATED dsrdLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The name of the stack to which the resource belongs.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdStackName :: Lens.Lens' DetectStackResourceDrift Lude.Text
dsrdStackName = Lens.lens (stackName :: DetectStackResourceDrift -> Lude.Text) (\s a -> s {stackName = a} :: DetectStackResourceDrift)
{-# DEPRECATED dsrdStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest DetectStackResourceDrift where
  type Rs DetectStackResourceDrift = DetectStackResourceDriftResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DetectStackResourceDriftResult"
      ( \s h x ->
          DetectStackResourceDriftResponse'
            Lude.<$> (x Lude..@ "StackResourceDrift")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
        "LogicalResourceId" Lude.=: logicalResourceId,
        "StackName" Lude.=: stackName
      ]

-- | /See:/ 'mkDetectStackResourceDriftResponse' smart constructor.
data DetectStackResourceDriftResponse = DetectStackResourceDriftResponse'
  { -- | Information about whether the resource's actual configuration has drifted from its expected template configuration, including actual and expected property values and any differences detected.
    stackResourceDrift :: StackResourceDrift,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectStackResourceDriftResponse' with the minimum fields required to make a request.
--
-- * 'stackResourceDrift' - Information about whether the resource's actual configuration has drifted from its expected template configuration, including actual and expected property values and any differences detected.
-- * 'responseStatus' - The response status code.
mkDetectStackResourceDriftResponse ::
  -- | 'stackResourceDrift'
  StackResourceDrift ->
  -- | 'responseStatus'
  Lude.Int ->
  DetectStackResourceDriftResponse
mkDetectStackResourceDriftResponse
  pStackResourceDrift_
  pResponseStatus_ =
    DetectStackResourceDriftResponse'
      { stackResourceDrift =
          pStackResourceDrift_,
        responseStatus = pResponseStatus_
      }

-- | Information about whether the resource's actual configuration has drifted from its expected template configuration, including actual and expected property values and any differences detected.
--
-- /Note:/ Consider using 'stackResourceDrift' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdfrsStackResourceDrift :: Lens.Lens' DetectStackResourceDriftResponse StackResourceDrift
dsrdfrsStackResourceDrift = Lens.lens (stackResourceDrift :: DetectStackResourceDriftResponse -> StackResourceDrift) (\s a -> s {stackResourceDrift = a} :: DetectStackResourceDriftResponse)
{-# DEPRECATED dsrdfrsStackResourceDrift "Use generic-lens or generic-optics with 'stackResourceDrift' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdfrsResponseStatus :: Lens.Lens' DetectStackResourceDriftResponse Lude.Int
dsrdfrsResponseStatus = Lens.lens (responseStatus :: DetectStackResourceDriftResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectStackResourceDriftResponse)
{-# DEPRECATED dsrdfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
