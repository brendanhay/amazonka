{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateLambdaFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the specified Amazon Connect instance to access the specified Lambda function.
module Network.AWS.Connect.AssociateLambdaFunction
  ( -- * Creating a request
    AssociateLambdaFunction (..),
    mkAssociateLambdaFunction,

    -- ** Request lenses
    alfInstanceId,
    alfFunctionARN,

    -- * Destructuring the response
    AssociateLambdaFunctionResponse (..),
    mkAssociateLambdaFunctionResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateLambdaFunction' smart constructor.
data AssociateLambdaFunction = AssociateLambdaFunction'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) for the Lambda function being associated. Maximum number of characters allowed is 140.
    functionARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateLambdaFunction' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'functionARN' - The Amazon Resource Name (ARN) for the Lambda function being associated. Maximum number of characters allowed is 140.
mkAssociateLambdaFunction ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'functionARN'
  Lude.Text ->
  AssociateLambdaFunction
mkAssociateLambdaFunction pInstanceId_ pFunctionARN_ =
  AssociateLambdaFunction'
    { instanceId = pInstanceId_,
      functionARN = pFunctionARN_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alfInstanceId :: Lens.Lens' AssociateLambdaFunction Lude.Text
alfInstanceId = Lens.lens (instanceId :: AssociateLambdaFunction -> Lude.Text) (\s a -> s {instanceId = a} :: AssociateLambdaFunction)
{-# DEPRECATED alfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The Amazon Resource Name (ARN) for the Lambda function being associated. Maximum number of characters allowed is 140.
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alfFunctionARN :: Lens.Lens' AssociateLambdaFunction Lude.Text
alfFunctionARN = Lens.lens (functionARN :: AssociateLambdaFunction -> Lude.Text) (\s a -> s {functionARN = a} :: AssociateLambdaFunction)
{-# DEPRECATED alfFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

instance Lude.AWSRequest AssociateLambdaFunction where
  type Rs AssociateLambdaFunction = AssociateLambdaFunctionResponse
  request = Req.putJSON connectService
  response = Res.receiveNull AssociateLambdaFunctionResponse'

instance Lude.ToHeaders AssociateLambdaFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateLambdaFunction where
  toJSON AssociateLambdaFunction' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("FunctionArn" Lude..= functionARN)])

instance Lude.ToPath AssociateLambdaFunction where
  toPath AssociateLambdaFunction' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/lambda-function"]

instance Lude.ToQuery AssociateLambdaFunction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateLambdaFunctionResponse' smart constructor.
data AssociateLambdaFunctionResponse = AssociateLambdaFunctionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateLambdaFunctionResponse' with the minimum fields required to make a request.
mkAssociateLambdaFunctionResponse ::
  AssociateLambdaFunctionResponse
mkAssociateLambdaFunctionResponse =
  AssociateLambdaFunctionResponse'
