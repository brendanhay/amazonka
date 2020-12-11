{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetFunctionConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the reserved concurrency configuration for a function. To set a concurrency limit for a function, use 'PutFunctionConcurrency' .
module Network.AWS.Lambda.GetFunctionConcurrency
  ( -- * Creating a request
    GetFunctionConcurrency (..),
    mkGetFunctionConcurrency,

    -- ** Request lenses
    gFunctionName,

    -- * Destructuring the response
    GetFunctionConcurrencyResponse (..),
    mkGetFunctionConcurrencyResponse,

    -- ** Response lenses
    gfcrsReservedConcurrentExecutions,
    gfcrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFunctionConcurrency' smart constructor.
newtype GetFunctionConcurrency = GetFunctionConcurrency'
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

-- | Creates a value of 'GetFunctionConcurrency' with the minimum fields required to make a request.
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
mkGetFunctionConcurrency ::
  -- | 'functionName'
  Lude.Text ->
  GetFunctionConcurrency
mkGetFunctionConcurrency pFunctionName_ =
  GetFunctionConcurrency' {functionName = pFunctionName_}

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
gFunctionName :: Lens.Lens' GetFunctionConcurrency Lude.Text
gFunctionName = Lens.lens (functionName :: GetFunctionConcurrency -> Lude.Text) (\s a -> s {functionName = a} :: GetFunctionConcurrency)
{-# DEPRECATED gFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest GetFunctionConcurrency where
  type Rs GetFunctionConcurrency = GetFunctionConcurrencyResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFunctionConcurrencyResponse'
            Lude.<$> (x Lude..?> "ReservedConcurrentExecutions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFunctionConcurrency where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetFunctionConcurrency where
  toPath GetFunctionConcurrency' {..} =
    Lude.mconcat
      ["/2019-09-30/functions/", Lude.toBS functionName, "/concurrency"]

instance Lude.ToQuery GetFunctionConcurrency where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFunctionConcurrencyResponse' smart constructor.
data GetFunctionConcurrencyResponse = GetFunctionConcurrencyResponse'
  { reservedConcurrentExecutions ::
      Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'GetFunctionConcurrencyResponse' with the minimum fields required to make a request.
--
-- * 'reservedConcurrentExecutions' - The number of simultaneous executions that are reserved for the function.
-- * 'responseStatus' - The response status code.
mkGetFunctionConcurrencyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFunctionConcurrencyResponse
mkGetFunctionConcurrencyResponse pResponseStatus_ =
  GetFunctionConcurrencyResponse'
    { reservedConcurrentExecutions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of simultaneous executions that are reserved for the function.
--
-- /Note:/ Consider using 'reservedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcrsReservedConcurrentExecutions :: Lens.Lens' GetFunctionConcurrencyResponse (Lude.Maybe Lude.Natural)
gfcrsReservedConcurrentExecutions = Lens.lens (reservedConcurrentExecutions :: GetFunctionConcurrencyResponse -> Lude.Maybe Lude.Natural) (\s a -> s {reservedConcurrentExecutions = a} :: GetFunctionConcurrencyResponse)
{-# DEPRECATED gfcrsReservedConcurrentExecutions "Use generic-lens or generic-optics with 'reservedConcurrentExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcrsResponseStatus :: Lens.Lens' GetFunctionConcurrencyResponse Lude.Int
gfcrsResponseStatus = Lens.lens (responseStatus :: GetFunctionConcurrencyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFunctionConcurrencyResponse)
{-# DEPRECATED gfcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
