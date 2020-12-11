{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.StopAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the assessment run that is specified by the ARN of the assessment run.
module Network.AWS.Inspector.StopAssessmentRun
  ( -- * Creating a request
    StopAssessmentRun (..),
    mkStopAssessmentRun,

    -- ** Request lenses
    sarStopAction,
    sarAssessmentRunARN,

    -- * Destructuring the response
    StopAssessmentRunResponse (..),
    mkStopAssessmentRunResponse,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopAssessmentRun' smart constructor.
data StopAssessmentRun = StopAssessmentRun'
  { stopAction ::
      Lude.Maybe StopAction,
    assessmentRunARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAssessmentRun' with the minimum fields required to make a request.
--
-- * 'assessmentRunARN' - The ARN of the assessment run that you want to stop.
-- * 'stopAction' - An input option that can be set to either START_EVALUATION or SKIP_EVALUATION. START_EVALUATION (the default value), stops the AWS agent from collecting data and begins the results evaluation and the findings generation process. SKIP_EVALUATION cancels the assessment run immediately, after which no findings are generated.
mkStopAssessmentRun ::
  -- | 'assessmentRunARN'
  Lude.Text ->
  StopAssessmentRun
mkStopAssessmentRun pAssessmentRunARN_ =
  StopAssessmentRun'
    { stopAction = Lude.Nothing,
      assessmentRunARN = pAssessmentRunARN_
    }

-- | An input option that can be set to either START_EVALUATION or SKIP_EVALUATION. START_EVALUATION (the default value), stops the AWS agent from collecting data and begins the results evaluation and the findings generation process. SKIP_EVALUATION cancels the assessment run immediately, after which no findings are generated.
--
-- /Note:/ Consider using 'stopAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarStopAction :: Lens.Lens' StopAssessmentRun (Lude.Maybe StopAction)
sarStopAction = Lens.lens (stopAction :: StopAssessmentRun -> Lude.Maybe StopAction) (\s a -> s {stopAction = a} :: StopAssessmentRun)
{-# DEPRECATED sarStopAction "Use generic-lens or generic-optics with 'stopAction' instead." #-}

-- | The ARN of the assessment run that you want to stop.
--
-- /Note:/ Consider using 'assessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAssessmentRunARN :: Lens.Lens' StopAssessmentRun Lude.Text
sarAssessmentRunARN = Lens.lens (assessmentRunARN :: StopAssessmentRun -> Lude.Text) (\s a -> s {assessmentRunARN = a} :: StopAssessmentRun)
{-# DEPRECATED sarAssessmentRunARN "Use generic-lens or generic-optics with 'assessmentRunARN' instead." #-}

instance Lude.AWSRequest StopAssessmentRun where
  type Rs StopAssessmentRun = StopAssessmentRunResponse
  request = Req.postJSON inspectorService
  response = Res.receiveNull StopAssessmentRunResponse'

instance Lude.ToHeaders StopAssessmentRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.StopAssessmentRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopAssessmentRun where
  toJSON StopAssessmentRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stopAction" Lude..=) Lude.<$> stopAction,
            Lude.Just ("assessmentRunArn" Lude..= assessmentRunARN)
          ]
      )

instance Lude.ToPath StopAssessmentRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StopAssessmentRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopAssessmentRunResponse' smart constructor.
data StopAssessmentRunResponse = StopAssessmentRunResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAssessmentRunResponse' with the minimum fields required to make a request.
mkStopAssessmentRunResponse ::
  StopAssessmentRunResponse
mkStopAssessmentRunResponse = StopAssessmentRunResponse'
