{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a processing job.
module Network.AWS.SageMaker.StopProcessingJob
  ( -- * Creating a request
    StopProcessingJob (..),
    mkStopProcessingJob,

    -- ** Request lenses
    spjProcessingJobName,

    -- * Destructuring the response
    StopProcessingJobResponse (..),
    mkStopProcessingJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStopProcessingJob' smart constructor.
newtype StopProcessingJob = StopProcessingJob'
  { -- | The name of the processing job to stop.
    processingJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopProcessingJob' with the minimum fields required to make a request.
--
-- * 'processingJobName' - The name of the processing job to stop.
mkStopProcessingJob ::
  -- | 'processingJobName'
  Lude.Text ->
  StopProcessingJob
mkStopProcessingJob pProcessingJobName_ =
  StopProcessingJob' {processingJobName = pProcessingJobName_}

-- | The name of the processing job to stop.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spjProcessingJobName :: Lens.Lens' StopProcessingJob Lude.Text
spjProcessingJobName = Lens.lens (processingJobName :: StopProcessingJob -> Lude.Text) (\s a -> s {processingJobName = a} :: StopProcessingJob)
{-# DEPRECATED spjProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

instance Lude.AWSRequest StopProcessingJob where
  type Rs StopProcessingJob = StopProcessingJobResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StopProcessingJobResponse'

instance Lude.ToHeaders StopProcessingJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StopProcessingJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopProcessingJob where
  toJSON StopProcessingJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ProcessingJobName" Lude..= processingJobName)]
      )

instance Lude.ToPath StopProcessingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopProcessingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopProcessingJobResponse' smart constructor.
data StopProcessingJobResponse = StopProcessingJobResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopProcessingJobResponse' with the minimum fields required to make a request.
mkStopProcessingJobResponse ::
  StopProcessingJobResponse
mkStopProcessingJobResponse = StopProcessingJobResponse'
