{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutEvaluations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by an AWS Lambda function to deliver evaluation results to AWS Config. This action is required in every AWS Lambda function that is invoked by an AWS Config rule.
module Network.AWS.Config.PutEvaluations
  ( -- * Creating a request
    PutEvaluations (..),
    mkPutEvaluations,

    -- ** Request lenses
    peEvaluations,
    peTestMode,
    peResultToken,

    -- * Destructuring the response
    PutEvaluationsResponse (..),
    mkPutEvaluationsResponse,

    -- ** Response lenses
    persFailedEvaluations,
    persResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkPutEvaluations' smart constructor.
data PutEvaluations = PutEvaluations'
  { evaluations ::
      Lude.Maybe [Evaluation],
    testMode :: Lude.Maybe Lude.Bool,
    resultToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEvaluations' with the minimum fields required to make a request.
--
-- * 'evaluations' - The assessments that the AWS Lambda function performs. Each evaluation identifies an AWS resource and indicates whether it complies with the AWS Config rule that invokes the AWS Lambda function.
-- * 'resultToken' - An encrypted token that associates an evaluation with an AWS Config rule. Identifies the rule and the event that triggered the evaluation.
-- * 'testMode' - Use this parameter to specify a test run for @PutEvaluations@ . You can verify whether your AWS Lambda function will deliver evaluation results to AWS Config. No updates occur to your existing evaluations, and evaluation results are not sent to AWS Config.
mkPutEvaluations ::
  -- | 'resultToken'
  Lude.Text ->
  PutEvaluations
mkPutEvaluations pResultToken_ =
  PutEvaluations'
    { evaluations = Lude.Nothing,
      testMode = Lude.Nothing,
      resultToken = pResultToken_
    }

-- | The assessments that the AWS Lambda function performs. Each evaluation identifies an AWS resource and indicates whether it complies with the AWS Config rule that invokes the AWS Lambda function.
--
-- /Note:/ Consider using 'evaluations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEvaluations :: Lens.Lens' PutEvaluations (Lude.Maybe [Evaluation])
peEvaluations = Lens.lens (evaluations :: PutEvaluations -> Lude.Maybe [Evaluation]) (\s a -> s {evaluations = a} :: PutEvaluations)
{-# DEPRECATED peEvaluations "Use generic-lens or generic-optics with 'evaluations' instead." #-}

-- | Use this parameter to specify a test run for @PutEvaluations@ . You can verify whether your AWS Lambda function will deliver evaluation results to AWS Config. No updates occur to your existing evaluations, and evaluation results are not sent to AWS Config.
--
-- /Note:/ Consider using 'testMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peTestMode :: Lens.Lens' PutEvaluations (Lude.Maybe Lude.Bool)
peTestMode = Lens.lens (testMode :: PutEvaluations -> Lude.Maybe Lude.Bool) (\s a -> s {testMode = a} :: PutEvaluations)
{-# DEPRECATED peTestMode "Use generic-lens or generic-optics with 'testMode' instead." #-}

-- | An encrypted token that associates an evaluation with an AWS Config rule. Identifies the rule and the event that triggered the evaluation.
--
-- /Note:/ Consider using 'resultToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peResultToken :: Lens.Lens' PutEvaluations Lude.Text
peResultToken = Lens.lens (resultToken :: PutEvaluations -> Lude.Text) (\s a -> s {resultToken = a} :: PutEvaluations)
{-# DEPRECATED peResultToken "Use generic-lens or generic-optics with 'resultToken' instead." #-}

instance Lude.AWSRequest PutEvaluations where
  type Rs PutEvaluations = PutEvaluationsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutEvaluationsResponse'
            Lude.<$> (x Lude..?> "FailedEvaluations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutEvaluations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.PutEvaluations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutEvaluations where
  toJSON PutEvaluations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Evaluations" Lude..=) Lude.<$> evaluations,
            ("TestMode" Lude..=) Lude.<$> testMode,
            Lude.Just ("ResultToken" Lude..= resultToken)
          ]
      )

instance Lude.ToPath PutEvaluations where
  toPath = Lude.const "/"

instance Lude.ToQuery PutEvaluations where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkPutEvaluationsResponse' smart constructor.
data PutEvaluationsResponse = PutEvaluationsResponse'
  { failedEvaluations ::
      Lude.Maybe [Evaluation],
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

-- | Creates a value of 'PutEvaluationsResponse' with the minimum fields required to make a request.
--
-- * 'failedEvaluations' - Requests that failed because of a client or server error.
-- * 'responseStatus' - The response status code.
mkPutEvaluationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutEvaluationsResponse
mkPutEvaluationsResponse pResponseStatus_ =
  PutEvaluationsResponse'
    { failedEvaluations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Requests that failed because of a client or server error.
--
-- /Note:/ Consider using 'failedEvaluations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
persFailedEvaluations :: Lens.Lens' PutEvaluationsResponse (Lude.Maybe [Evaluation])
persFailedEvaluations = Lens.lens (failedEvaluations :: PutEvaluationsResponse -> Lude.Maybe [Evaluation]) (\s a -> s {failedEvaluations = a} :: PutEvaluationsResponse)
{-# DEPRECATED persFailedEvaluations "Use generic-lens or generic-optics with 'failedEvaluations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
persResponseStatus :: Lens.Lens' PutEvaluationsResponse Lude.Int
persResponseStatus = Lens.lens (responseStatus :: PutEvaluationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutEvaluationsResponse)
{-# DEPRECATED persResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
