{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopTrainingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a training job. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts, so the results of the training is not lost.
--
-- When it receives a @StopTrainingJob@ request, Amazon SageMaker changes the status of the job to @Stopping@ . After Amazon SageMaker stops the job, it sets the status to @Stopped@ .
module Network.AWS.SageMaker.StopTrainingJob
  ( -- * Creating a request
    StopTrainingJob (..),
    mkStopTrainingJob,

    -- ** Request lenses
    stjTrainingJobName,

    -- * Destructuring the response
    StopTrainingJobResponse (..),
    mkStopTrainingJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStopTrainingJob' smart constructor.
newtype StopTrainingJob = StopTrainingJob'
  { -- | The name of the training job to stop.
    trainingJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTrainingJob' with the minimum fields required to make a request.
--
-- * 'trainingJobName' - The name of the training job to stop.
mkStopTrainingJob ::
  -- | 'trainingJobName'
  Lude.Text ->
  StopTrainingJob
mkStopTrainingJob pTrainingJobName_ =
  StopTrainingJob' {trainingJobName = pTrainingJobName_}

-- | The name of the training job to stop.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjTrainingJobName :: Lens.Lens' StopTrainingJob Lude.Text
stjTrainingJobName = Lens.lens (trainingJobName :: StopTrainingJob -> Lude.Text) (\s a -> s {trainingJobName = a} :: StopTrainingJob)
{-# DEPRECATED stjTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

instance Lude.AWSRequest StopTrainingJob where
  type Rs StopTrainingJob = StopTrainingJobResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StopTrainingJobResponse'

instance Lude.ToHeaders StopTrainingJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StopTrainingJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopTrainingJob where
  toJSON StopTrainingJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("TrainingJobName" Lude..= trainingJobName)]
      )

instance Lude.ToPath StopTrainingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopTrainingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopTrainingJobResponse' smart constructor.
data StopTrainingJobResponse = StopTrainingJobResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTrainingJobResponse' with the minimum fields required to make a request.
mkStopTrainingJobResponse ::
  StopTrainingJobResponse
mkStopTrainingJobResponse = StopTrainingJobResponse'
