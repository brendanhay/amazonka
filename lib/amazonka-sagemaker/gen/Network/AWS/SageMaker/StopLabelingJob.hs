{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopLabelingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running labeling job. A job that is stopped cannot be restarted. Any results obtained before the job is stopped are placed in the Amazon S3 output bucket.
module Network.AWS.SageMaker.StopLabelingJob
  ( -- * Creating a request
    StopLabelingJob (..),
    mkStopLabelingJob,

    -- ** Request lenses
    sljLabelingJobName,

    -- * Destructuring the response
    StopLabelingJobResponse (..),
    mkStopLabelingJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStopLabelingJob' smart constructor.
newtype StopLabelingJob = StopLabelingJob'
  { labelingJobName ::
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

-- | Creates a value of 'StopLabelingJob' with the minimum fields required to make a request.
--
-- * 'labelingJobName' - The name of the labeling job to stop.
mkStopLabelingJob ::
  -- | 'labelingJobName'
  Lude.Text ->
  StopLabelingJob
mkStopLabelingJob pLabelingJobName_ =
  StopLabelingJob' {labelingJobName = pLabelingJobName_}

-- | The name of the labeling job to stop.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sljLabelingJobName :: Lens.Lens' StopLabelingJob Lude.Text
sljLabelingJobName = Lens.lens (labelingJobName :: StopLabelingJob -> Lude.Text) (\s a -> s {labelingJobName = a} :: StopLabelingJob)
{-# DEPRECATED sljLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

instance Lude.AWSRequest StopLabelingJob where
  type Rs StopLabelingJob = StopLabelingJobResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StopLabelingJobResponse'

instance Lude.ToHeaders StopLabelingJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StopLabelingJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopLabelingJob where
  toJSON StopLabelingJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("LabelingJobName" Lude..= labelingJobName)]
      )

instance Lude.ToPath StopLabelingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopLabelingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopLabelingJobResponse' smart constructor.
data StopLabelingJobResponse = StopLabelingJobResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopLabelingJobResponse' with the minimum fields required to make a request.
mkStopLabelingJobResponse ::
  StopLabelingJobResponse
mkStopLabelingJobResponse = StopLabelingJobResponse'
