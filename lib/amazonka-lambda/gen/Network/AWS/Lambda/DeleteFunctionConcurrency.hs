{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteFunctionConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a concurrent execution limit from a function.
module Network.AWS.Lambda.DeleteFunctionConcurrency
  ( -- * Creating a request
    DeleteFunctionConcurrency (..),
    mkDeleteFunctionConcurrency,

    -- ** Request lenses
    dfcFunctionName,

    -- * Destructuring the response
    DeleteFunctionConcurrencyResponse (..),
    mkDeleteFunctionConcurrencyResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFunctionConcurrency' smart constructor.
newtype DeleteFunctionConcurrency = DeleteFunctionConcurrency'
  { functionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunctionConcurrency' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
mkDeleteFunctionConcurrency ::
  -- | 'functionName'
  Lude.Text ->
  DeleteFunctionConcurrency
mkDeleteFunctionConcurrency pFunctionName_ =
  DeleteFunctionConcurrency' {functionName = pFunctionName_}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcFunctionName :: Lens.Lens' DeleteFunctionConcurrency Lude.Text
dfcFunctionName = Lens.lens (functionName :: DeleteFunctionConcurrency -> Lude.Text) (\s a -> s {functionName = a} :: DeleteFunctionConcurrency)
{-# DEPRECATED dfcFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest DeleteFunctionConcurrency where
  type
    Rs DeleteFunctionConcurrency =
      DeleteFunctionConcurrencyResponse
  request = Req.delete lambdaService
  response = Res.receiveNull DeleteFunctionConcurrencyResponse'

instance Lude.ToHeaders DeleteFunctionConcurrency where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteFunctionConcurrency where
  toPath DeleteFunctionConcurrency' {..} =
    Lude.mconcat
      ["/2017-10-31/functions/", Lude.toBS functionName, "/concurrency"]

instance Lude.ToQuery DeleteFunctionConcurrency where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFunctionConcurrencyResponse' smart constructor.
data DeleteFunctionConcurrencyResponse = DeleteFunctionConcurrencyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunctionConcurrencyResponse' with the minimum fields required to make a request.
mkDeleteFunctionConcurrencyResponse ::
  DeleteFunctionConcurrencyResponse
mkDeleteFunctionConcurrencyResponse =
  DeleteFunctionConcurrencyResponse'
