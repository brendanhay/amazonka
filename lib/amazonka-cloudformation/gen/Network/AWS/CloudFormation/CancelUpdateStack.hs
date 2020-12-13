{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.CancelUpdateStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an update on the specified stack. If the call completes successfully, the stack rolls back the update and reverts to the previous stack configuration.
module Network.AWS.CloudFormation.CancelUpdateStack
  ( -- * Creating a request
    CancelUpdateStack (..),
    mkCancelUpdateStack,

    -- ** Request lenses
    cusClientRequestToken,
    cusStackName,

    -- * Destructuring the response
    CancelUpdateStackResponse (..),
    mkCancelUpdateStackResponse,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'CancelUpdateStack' action.
--
-- /See:/ 'mkCancelUpdateStack' smart constructor.
data CancelUpdateStack = CancelUpdateStack'
  { -- | A unique identifier for this @CancelUpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to cancel an update on a stack with the same name. You might retry @CancelUpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | The name or the unique stack ID that is associated with the stack.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelUpdateStack' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique identifier for this @CancelUpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to cancel an update on a stack with the same name. You might retry @CancelUpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
-- * 'stackName' - The name or the unique stack ID that is associated with the stack.
mkCancelUpdateStack ::
  -- | 'stackName'
  Lude.Text ->
  CancelUpdateStack
mkCancelUpdateStack pStackName_ =
  CancelUpdateStack'
    { clientRequestToken = Lude.Nothing,
      stackName = pStackName_
    }

-- | A unique identifier for this @CancelUpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to cancel an update on a stack with the same name. You might retry @CancelUpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusClientRequestToken :: Lens.Lens' CancelUpdateStack (Lude.Maybe Lude.Text)
cusClientRequestToken = Lens.lens (clientRequestToken :: CancelUpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CancelUpdateStack)
{-# DEPRECATED cusClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The name or the unique stack ID that is associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusStackName :: Lens.Lens' CancelUpdateStack Lude.Text
cusStackName = Lens.lens (stackName :: CancelUpdateStack -> Lude.Text) (\s a -> s {stackName = a} :: CancelUpdateStack)
{-# DEPRECATED cusStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest CancelUpdateStack where
  type Rs CancelUpdateStack = CancelUpdateStackResponse
  request = Req.postQuery cloudFormationService
  response = Res.receiveNull CancelUpdateStackResponse'

instance Lude.ToHeaders CancelUpdateStack where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelUpdateStack where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelUpdateStack where
  toQuery CancelUpdateStack' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelUpdateStack" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "ClientRequestToken" Lude.=: clientRequestToken,
        "StackName" Lude.=: stackName
      ]

-- | /See:/ 'mkCancelUpdateStackResponse' smart constructor.
data CancelUpdateStackResponse = CancelUpdateStackResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelUpdateStackResponse' with the minimum fields required to make a request.
mkCancelUpdateStackResponse ::
  CancelUpdateStackResponse
mkCancelUpdateStackResponse = CancelUpdateStackResponse'
