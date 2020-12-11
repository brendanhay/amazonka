{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.PutFunctionConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the maximum number of simultaneous executions for a function, and reserves capacity for that concurrency level.
--
-- Concurrency settings apply to the function as a whole, including all published versions and the unpublished version. Reserving concurrency both ensures that your function has capacity to process the specified number of events simultaneously, and prevents it from scaling beyond that level. Use 'GetFunction' to see the current setting for a function.
-- Use 'GetAccountSettings' to see your Regional concurrency limit. You can reserve concurrency for as many functions as you like, as long as you leave at least 100 simultaneous executions unreserved for functions that aren't configured with a per-function limit. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
module Network.AWS.Lambda.PutFunctionConcurrency
  ( -- * Creating a request
    PutFunctionConcurrency (..),
    mkPutFunctionConcurrency,

    -- ** Request lenses
    pfcFunctionName,
    pfcReservedConcurrentExecutions,

    -- * Destructuring the response
    Concurrency (..),
    mkConcurrency,

    -- ** Response lenses
    cReservedConcurrentExecutions,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutFunctionConcurrency' smart constructor.
data PutFunctionConcurrency = PutFunctionConcurrency'
  { functionName ::
      Lude.Text,
    reservedConcurrentExecutions :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutFunctionConcurrency' with the minimum fields required to make a request.
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
-- * 'reservedConcurrentExecutions' - The number of simultaneous executions to reserve for the function.
mkPutFunctionConcurrency ::
  -- | 'functionName'
  Lude.Text ->
  -- | 'reservedConcurrentExecutions'
  Lude.Natural ->
  PutFunctionConcurrency
mkPutFunctionConcurrency
  pFunctionName_
  pReservedConcurrentExecutions_ =
    PutFunctionConcurrency'
      { functionName = pFunctionName_,
        reservedConcurrentExecutions = pReservedConcurrentExecutions_
      }

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
pfcFunctionName :: Lens.Lens' PutFunctionConcurrency Lude.Text
pfcFunctionName = Lens.lens (functionName :: PutFunctionConcurrency -> Lude.Text) (\s a -> s {functionName = a} :: PutFunctionConcurrency)
{-# DEPRECATED pfcFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The number of simultaneous executions to reserve for the function.
--
-- /Note:/ Consider using 'reservedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfcReservedConcurrentExecutions :: Lens.Lens' PutFunctionConcurrency Lude.Natural
pfcReservedConcurrentExecutions = Lens.lens (reservedConcurrentExecutions :: PutFunctionConcurrency -> Lude.Natural) (\s a -> s {reservedConcurrentExecutions = a} :: PutFunctionConcurrency)
{-# DEPRECATED pfcReservedConcurrentExecutions "Use generic-lens or generic-optics with 'reservedConcurrentExecutions' instead." #-}

instance Lude.AWSRequest PutFunctionConcurrency where
  type Rs PutFunctionConcurrency = Concurrency
  request = Req.putJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PutFunctionConcurrency where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutFunctionConcurrency where
  toJSON PutFunctionConcurrency' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "ReservedConcurrentExecutions"
                  Lude..= reservedConcurrentExecutions
              )
          ]
      )

instance Lude.ToPath PutFunctionConcurrency where
  toPath PutFunctionConcurrency' {..} =
    Lude.mconcat
      ["/2017-10-31/functions/", Lude.toBS functionName, "/concurrency"]

instance Lude.ToQuery PutFunctionConcurrency where
  toQuery = Lude.const Lude.mempty
