{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopTransformJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a transform job.
--
-- When Amazon SageMaker receives a @StopTransformJob@ request, the status of the job changes to @Stopping@ . After Amazon SageMaker stops the job, the status is set to @Stopped@ . When you stop a transform job before it is completed, Amazon SageMaker doesn't store the job's output in Amazon S3.
module Network.AWS.SageMaker.StopTransformJob
  ( -- * Creating a request
    StopTransformJob (..),
    mkStopTransformJob,

    -- ** Request lenses
    stjTransformJobName,

    -- * Destructuring the response
    StopTransformJobResponse (..),
    mkStopTransformJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStopTransformJob' smart constructor.
newtype StopTransformJob = StopTransformJob'
  { -- | The name of the transform job to stop.
    transformJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTransformJob' with the minimum fields required to make a request.
--
-- * 'transformJobName' - The name of the transform job to stop.
mkStopTransformJob ::
  -- | 'transformJobName'
  Lude.Text ->
  StopTransformJob
mkStopTransformJob pTransformJobName_ =
  StopTransformJob' {transformJobName = pTransformJobName_}

-- | The name of the transform job to stop.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjTransformJobName :: Lens.Lens' StopTransformJob Lude.Text
stjTransformJobName = Lens.lens (transformJobName :: StopTransformJob -> Lude.Text) (\s a -> s {transformJobName = a} :: StopTransformJob)
{-# DEPRECATED stjTransformJobName "Use generic-lens or generic-optics with 'transformJobName' instead." #-}

instance Lude.AWSRequest StopTransformJob where
  type Rs StopTransformJob = StopTransformJobResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StopTransformJobResponse'

instance Lude.ToHeaders StopTransformJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StopTransformJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopTransformJob where
  toJSON StopTransformJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("TransformJobName" Lude..= transformJobName)]
      )

instance Lude.ToPath StopTransformJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopTransformJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopTransformJobResponse' smart constructor.
data StopTransformJobResponse = StopTransformJobResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTransformJobResponse' with the minimum fields required to make a request.
mkStopTransformJobResponse ::
  StopTransformJobResponse
mkStopTransformJobResponse = StopTransformJobResponse'
