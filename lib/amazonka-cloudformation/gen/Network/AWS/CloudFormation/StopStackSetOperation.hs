{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.StopStackSetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an in-progress operation on a stack set and its associated stack instances.
module Network.AWS.CloudFormation.StopStackSetOperation
  ( -- * Creating a request
    StopStackSetOperation (..),
    mkStopStackSetOperation,

    -- ** Request lenses
    sssoOperationId,
    sssoStackSetName,

    -- * Destructuring the response
    StopStackSetOperationResponse (..),
    mkStopStackSetOperationResponse,

    -- ** Response lenses
    sssorsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopStackSetOperation' smart constructor.
data StopStackSetOperation = StopStackSetOperation'
  { -- | The ID of the stack operation.
    operationId :: Lude.Text,
    -- | The name or unique ID of the stack set that you want to stop the operation for.
    stackSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopStackSetOperation' with the minimum fields required to make a request.
--
-- * 'operationId' - The ID of the stack operation.
-- * 'stackSetName' - The name or unique ID of the stack set that you want to stop the operation for.
mkStopStackSetOperation ::
  -- | 'operationId'
  Lude.Text ->
  -- | 'stackSetName'
  Lude.Text ->
  StopStackSetOperation
mkStopStackSetOperation pOperationId_ pStackSetName_ =
  StopStackSetOperation'
    { operationId = pOperationId_,
      stackSetName = pStackSetName_
    }

-- | The ID of the stack operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssoOperationId :: Lens.Lens' StopStackSetOperation Lude.Text
sssoOperationId = Lens.lens (operationId :: StopStackSetOperation -> Lude.Text) (\s a -> s {operationId = a} :: StopStackSetOperation)
{-# DEPRECATED sssoOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The name or unique ID of the stack set that you want to stop the operation for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssoStackSetName :: Lens.Lens' StopStackSetOperation Lude.Text
sssoStackSetName = Lens.lens (stackSetName :: StopStackSetOperation -> Lude.Text) (\s a -> s {stackSetName = a} :: StopStackSetOperation)
{-# DEPRECATED sssoStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

instance Lude.AWSRequest StopStackSetOperation where
  type Rs StopStackSetOperation = StopStackSetOperationResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "StopStackSetOperationResult"
      ( \s h x ->
          StopStackSetOperationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopStackSetOperation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StopStackSetOperation where
  toPath = Lude.const "/"

instance Lude.ToQuery StopStackSetOperation where
  toQuery StopStackSetOperation' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StopStackSetOperation" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "OperationId" Lude.=: operationId,
        "StackSetName" Lude.=: stackSetName
      ]

-- | /See:/ 'mkStopStackSetOperationResponse' smart constructor.
newtype StopStackSetOperationResponse = StopStackSetOperationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopStackSetOperationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopStackSetOperationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopStackSetOperationResponse
mkStopStackSetOperationResponse pResponseStatus_ =
  StopStackSetOperationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssorsResponseStatus :: Lens.Lens' StopStackSetOperationResponse Lude.Int
sssorsResponseStatus = Lens.lens (responseStatus :: StopStackSetOperationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopStackSetOperationResponse)
{-# DEPRECATED sssorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
