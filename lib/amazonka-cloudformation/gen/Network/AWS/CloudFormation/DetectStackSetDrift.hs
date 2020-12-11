{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dssdOperationPreferences,
    dssdOperationId,
    dssdStackSetName,

    -- * Destructuring the response
    DetectStackSetDriftResponse (..),
    mkDetectStackSetDriftResponse,

    -- ** Response lenses
    dssdrsOperationId,
    dssdrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectStackSetDrift' smart constructor.
data DetectStackSetDrift = DetectStackSetDrift'
  { operationPreferences ::
      Lude.Maybe StackSetOperationPreferences,
    operationId :: Lude.Maybe Lude.Text,
    stackSetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectStackSetDrift' with the minimum fields required to make a request.
--
-- * 'operationId' - /The ID of the stack set operation./
-- * 'operationPreferences' - Undocumented field.
-- * 'stackSetName' - The name of the stack set on which to perform the drift detection operation.
mkDetectStackSetDrift ::
  -- | 'stackSetName'
  Lude.Text ->
  DetectStackSetDrift
mkDetectStackSetDrift pStackSetName_ =
  DetectStackSetDrift'
    { operationPreferences = Lude.Nothing,
      operationId = Lude.Nothing,
      stackSetName = pStackSetName_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdOperationPreferences :: Lens.Lens' DetectStackSetDrift (Lude.Maybe StackSetOperationPreferences)
dssdOperationPreferences = Lens.lens (operationPreferences :: DetectStackSetDrift -> Lude.Maybe StackSetOperationPreferences) (\s a -> s {operationPreferences = a} :: DetectStackSetDrift)
{-# DEPRECATED dssdOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

-- | /The ID of the stack set operation./
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdOperationId :: Lens.Lens' DetectStackSetDrift (Lude.Maybe Lude.Text)
dssdOperationId = Lens.lens (operationId :: DetectStackSetDrift -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: DetectStackSetDrift)
{-# DEPRECATED dssdOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The name of the stack set on which to perform the drift detection operation.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdStackSetName :: Lens.Lens' DetectStackSetDrift Lude.Text
dssdStackSetName = Lens.lens (stackSetName :: DetectStackSetDrift -> Lude.Text) (\s a -> s {stackSetName = a} :: DetectStackSetDrift)
{-# DEPRECATED dssdStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

instance Lude.AWSRequest DetectStackSetDrift where
  type Rs DetectStackSetDrift = DetectStackSetDriftResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DetectStackSetDriftResult"
      ( \s h x ->
          DetectStackSetDriftResponse'
            Lude.<$> (x Lude..@? "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectStackSetDrift where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetectStackSetDrift where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectStackSetDrift where
  toQuery DetectStackSetDrift' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetectStackSetDrift" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "OperationPreferences" Lude.=: operationPreferences,
        "OperationId" Lude.=: operationId,
        "StackSetName" Lude.=: stackSetName
      ]

-- | /See:/ 'mkDetectStackSetDriftResponse' smart constructor.
data DetectStackSetDriftResponse = DetectStackSetDriftResponse'
  { operationId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectStackSetDriftResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - The ID of the drift detection stack set operation.
--
-- you can use this operation id with @'DescribeStackSetOperation' @ to monitor the progress of the drift detection operation.
-- * 'responseStatus' - The response status code.
mkDetectStackSetDriftResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectStackSetDriftResponse
mkDetectStackSetDriftResponse pResponseStatus_ =
  DetectStackSetDriftResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the drift detection stack set operation.
--
-- you can use this operation id with @'DescribeStackSetOperation' @ to monitor the progress of the drift detection operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdrsOperationId :: Lens.Lens' DetectStackSetDriftResponse (Lude.Maybe Lude.Text)
dssdrsOperationId = Lens.lens (operationId :: DetectStackSetDriftResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: DetectStackSetDriftResponse)
{-# DEPRECATED dssdrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssdrsResponseStatus :: Lens.Lens' DetectStackSetDriftResponse Lude.Int
dssdrsResponseStatus = Lens.lens (responseStatus :: DetectStackSetDriftResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectStackSetDriftResponse)
{-# DEPRECATED dssdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
