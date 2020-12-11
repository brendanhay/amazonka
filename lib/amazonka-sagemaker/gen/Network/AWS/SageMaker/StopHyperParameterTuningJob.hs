{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running hyperparameter tuning job and all running training jobs that the tuning job launched.
--
-- All model artifacts output from the training jobs are stored in Amazon Simple Storage Service (Amazon S3). All data that the training jobs write to Amazon CloudWatch Logs are still available in CloudWatch. After the tuning job moves to the @Stopped@ state, it releases all reserved resources for the tuning job.
module Network.AWS.SageMaker.StopHyperParameterTuningJob
  ( -- * Creating a request
    StopHyperParameterTuningJob (..),
    mkStopHyperParameterTuningJob,

    -- ** Request lenses
    shptjHyperParameterTuningJobName,

    -- * Destructuring the response
    StopHyperParameterTuningJobResponse (..),
    mkStopHyperParameterTuningJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStopHyperParameterTuningJob' smart constructor.
newtype StopHyperParameterTuningJob = StopHyperParameterTuningJob'
  { hyperParameterTuningJobName ::
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

-- | Creates a value of 'StopHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- * 'hyperParameterTuningJobName' - The name of the tuning job to stop.
mkStopHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Lude.Text ->
  StopHyperParameterTuningJob
mkStopHyperParameterTuningJob pHyperParameterTuningJobName_ =
  StopHyperParameterTuningJob'
    { hyperParameterTuningJobName =
        pHyperParameterTuningJobName_
    }

-- | The name of the tuning job to stop.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shptjHyperParameterTuningJobName :: Lens.Lens' StopHyperParameterTuningJob Lude.Text
shptjHyperParameterTuningJobName = Lens.lens (hyperParameterTuningJobName :: StopHyperParameterTuningJob -> Lude.Text) (\s a -> s {hyperParameterTuningJobName = a} :: StopHyperParameterTuningJob)
{-# DEPRECATED shptjHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

instance Lude.AWSRequest StopHyperParameterTuningJob where
  type
    Rs StopHyperParameterTuningJob =
      StopHyperParameterTuningJobResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StopHyperParameterTuningJobResponse'

instance Lude.ToHeaders StopHyperParameterTuningJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StopHyperParameterTuningJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopHyperParameterTuningJob where
  toJSON StopHyperParameterTuningJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "HyperParameterTuningJobName"
                  Lude..= hyperParameterTuningJobName
              )
          ]
      )

instance Lude.ToPath StopHyperParameterTuningJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopHyperParameterTuningJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopHyperParameterTuningJobResponse' smart constructor.
data StopHyperParameterTuningJobResponse = StopHyperParameterTuningJobResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopHyperParameterTuningJobResponse' with the minimum fields required to make a request.
mkStopHyperParameterTuningJobResponse ::
  StopHyperParameterTuningJobResponse
mkStopHyperParameterTuningJobResponse =
  StopHyperParameterTuningJobResponse'
