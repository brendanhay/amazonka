{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackSetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set operation.
module Network.AWS.CloudFormation.DescribeStackSetOperation
  ( -- * Creating a request
    DescribeStackSetOperation (..),
    mkDescribeStackSetOperation,

    -- ** Request lenses
    dssoOperationId,
    dssoStackSetName,

    -- * Destructuring the response
    DescribeStackSetOperationResponse (..),
    mkDescribeStackSetOperationResponse,

    -- ** Response lenses
    dssorsStackSetOperation,
    dssorsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStackSetOperation' smart constructor.
data DescribeStackSetOperation = DescribeStackSetOperation'
  { -- | The unique ID of the stack set operation.
    operationId :: Lude.Text,
    -- | The name or the unique stack ID of the stack set for the stack operation.
    stackSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackSetOperation' with the minimum fields required to make a request.
--
-- * 'operationId' - The unique ID of the stack set operation.
-- * 'stackSetName' - The name or the unique stack ID of the stack set for the stack operation.
mkDescribeStackSetOperation ::
  -- | 'operationId'
  Lude.Text ->
  -- | 'stackSetName'
  Lude.Text ->
  DescribeStackSetOperation
mkDescribeStackSetOperation pOperationId_ pStackSetName_ =
  DescribeStackSetOperation'
    { operationId = pOperationId_,
      stackSetName = pStackSetName_
    }

-- | The unique ID of the stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssoOperationId :: Lens.Lens' DescribeStackSetOperation Lude.Text
dssoOperationId = Lens.lens (operationId :: DescribeStackSetOperation -> Lude.Text) (\s a -> s {operationId = a} :: DescribeStackSetOperation)
{-# DEPRECATED dssoOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The name or the unique stack ID of the stack set for the stack operation.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssoStackSetName :: Lens.Lens' DescribeStackSetOperation Lude.Text
dssoStackSetName = Lens.lens (stackSetName :: DescribeStackSetOperation -> Lude.Text) (\s a -> s {stackSetName = a} :: DescribeStackSetOperation)
{-# DEPRECATED dssoStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

instance Lude.AWSRequest DescribeStackSetOperation where
  type
    Rs DescribeStackSetOperation =
      DescribeStackSetOperationResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeStackSetOperationResult"
      ( \s h x ->
          DescribeStackSetOperationResponse'
            Lude.<$> (x Lude..@? "StackSetOperation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStackSetOperation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStackSetOperation where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStackSetOperation where
  toQuery DescribeStackSetOperation' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeStackSetOperation" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "OperationId" Lude.=: operationId,
        "StackSetName" Lude.=: stackSetName
      ]

-- | /See:/ 'mkDescribeStackSetOperationResponse' smart constructor.
data DescribeStackSetOperationResponse = DescribeStackSetOperationResponse'
  { -- | The specified stack set operation.
    stackSetOperation :: Lude.Maybe StackSetOperation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackSetOperationResponse' with the minimum fields required to make a request.
--
-- * 'stackSetOperation' - The specified stack set operation.
-- * 'responseStatus' - The response status code.
mkDescribeStackSetOperationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStackSetOperationResponse
mkDescribeStackSetOperationResponse pResponseStatus_ =
  DescribeStackSetOperationResponse'
    { stackSetOperation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The specified stack set operation.
--
-- /Note:/ Consider using 'stackSetOperation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssorsStackSetOperation :: Lens.Lens' DescribeStackSetOperationResponse (Lude.Maybe StackSetOperation)
dssorsStackSetOperation = Lens.lens (stackSetOperation :: DescribeStackSetOperationResponse -> Lude.Maybe StackSetOperation) (\s a -> s {stackSetOperation = a} :: DescribeStackSetOperationResponse)
{-# DEPRECATED dssorsStackSetOperation "Use generic-lens or generic-optics with 'stackSetOperation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssorsResponseStatus :: Lens.Lens' DescribeStackSetOperationResponse Lude.Int
dssorsResponseStatus = Lens.lens (responseStatus :: DescribeStackSetOperationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStackSetOperationResponse)
{-# DEPRECATED dssorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
