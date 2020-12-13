{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateLambdaFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the Lambda function from the drop-down options available in the relevant contact flow blocks.
module Network.AWS.Connect.DisassociateLambdaFunction
  ( -- * Creating a request
    DisassociateLambdaFunction (..),
    mkDisassociateLambdaFunction,

    -- ** Request lenses
    dlfInstanceId,
    dlfFunctionARN,

    -- * Destructuring the response
    DisassociateLambdaFunctionResponse (..),
    mkDisassociateLambdaFunctionResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateLambdaFunction' smart constructor.
data DisassociateLambdaFunction = DisassociateLambdaFunction'
  { -- | The identifier of the Amazon Connect instance..
    instanceId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the Lambda function being disassociated.
    functionARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateLambdaFunction' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance..
-- * 'functionARN' - The Amazon Resource Name (ARN) of the Lambda function being disassociated.
mkDisassociateLambdaFunction ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'functionARN'
  Lude.Text ->
  DisassociateLambdaFunction
mkDisassociateLambdaFunction pInstanceId_ pFunctionARN_ =
  DisassociateLambdaFunction'
    { instanceId = pInstanceId_,
      functionARN = pFunctionARN_
    }

-- | The identifier of the Amazon Connect instance..
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlfInstanceId :: Lens.Lens' DisassociateLambdaFunction Lude.Text
dlfInstanceId = Lens.lens (instanceId :: DisassociateLambdaFunction -> Lude.Text) (\s a -> s {instanceId = a} :: DisassociateLambdaFunction)
{-# DEPRECATED dlfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Lambda function being disassociated.
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlfFunctionARN :: Lens.Lens' DisassociateLambdaFunction Lude.Text
dlfFunctionARN = Lens.lens (functionARN :: DisassociateLambdaFunction -> Lude.Text) (\s a -> s {functionARN = a} :: DisassociateLambdaFunction)
{-# DEPRECATED dlfFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

instance Lude.AWSRequest DisassociateLambdaFunction where
  type
    Rs DisassociateLambdaFunction =
      DisassociateLambdaFunctionResponse
  request = Req.delete connectService
  response = Res.receiveNull DisassociateLambdaFunctionResponse'

instance Lude.ToHeaders DisassociateLambdaFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DisassociateLambdaFunction where
  toPath DisassociateLambdaFunction' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/lambda-function"]

instance Lude.ToQuery DisassociateLambdaFunction where
  toQuery DisassociateLambdaFunction' {..} =
    Lude.mconcat ["functionArn" Lude.=: functionARN]

-- | /See:/ 'mkDisassociateLambdaFunctionResponse' smart constructor.
data DisassociateLambdaFunctionResponse = DisassociateLambdaFunctionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateLambdaFunctionResponse' with the minimum fields required to make a request.
mkDisassociateLambdaFunctionResponse ::
  DisassociateLambdaFunctionResponse
mkDisassociateLambdaFunctionResponse =
  DisassociateLambdaFunctionResponse'
