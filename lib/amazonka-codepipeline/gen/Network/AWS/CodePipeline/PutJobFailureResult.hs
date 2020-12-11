{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutJobFailureResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a job as returned to the pipeline by a job worker. Used for custom actions only.
module Network.AWS.CodePipeline.PutJobFailureResult
  ( -- * Creating a request
    PutJobFailureResult (..),
    mkPutJobFailureResult,

    -- ** Request lenses
    pjfrJobId,
    pjfrFailureDetails,

    -- * Destructuring the response
    PutJobFailureResultResponse (..),
    mkPutJobFailureResultResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @PutJobFailureResult@ action.
--
-- /See:/ 'mkPutJobFailureResult' smart constructor.
data PutJobFailureResult = PutJobFailureResult'
  { jobId :: Lude.Text,
    failureDetails :: FailureDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutJobFailureResult' with the minimum fields required to make a request.
--
-- * 'failureDetails' - The details about the failure of a job.
-- * 'jobId' - The unique system-generated ID of the job that failed. This is the same ID returned from @PollForJobs@ .
mkPutJobFailureResult ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'failureDetails'
  FailureDetails ->
  PutJobFailureResult
mkPutJobFailureResult pJobId_ pFailureDetails_ =
  PutJobFailureResult'
    { jobId = pJobId_,
      failureDetails = pFailureDetails_
    }

-- | The unique system-generated ID of the job that failed. This is the same ID returned from @PollForJobs@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjfrJobId :: Lens.Lens' PutJobFailureResult Lude.Text
pjfrJobId = Lens.lens (jobId :: PutJobFailureResult -> Lude.Text) (\s a -> s {jobId = a} :: PutJobFailureResult)
{-# DEPRECATED pjfrJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The details about the failure of a job.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjfrFailureDetails :: Lens.Lens' PutJobFailureResult FailureDetails
pjfrFailureDetails = Lens.lens (failureDetails :: PutJobFailureResult -> FailureDetails) (\s a -> s {failureDetails = a} :: PutJobFailureResult)
{-# DEPRECATED pjfrFailureDetails "Use generic-lens or generic-optics with 'failureDetails' instead." #-}

instance Lude.AWSRequest PutJobFailureResult where
  type Rs PutJobFailureResult = PutJobFailureResultResponse
  request = Req.postJSON codePipelineService
  response = Res.receiveNull PutJobFailureResultResponse'

instance Lude.ToHeaders PutJobFailureResult where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.PutJobFailureResult" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutJobFailureResult where
  toJSON PutJobFailureResult' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("jobId" Lude..= jobId),
            Lude.Just ("failureDetails" Lude..= failureDetails)
          ]
      )

instance Lude.ToPath PutJobFailureResult where
  toPath = Lude.const "/"

instance Lude.ToQuery PutJobFailureResult where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutJobFailureResultResponse' smart constructor.
data PutJobFailureResultResponse = PutJobFailureResultResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutJobFailureResultResponse' with the minimum fields required to make a request.
mkPutJobFailureResultResponse ::
  PutJobFailureResultResponse
mkPutJobFailureResultResponse = PutJobFailureResultResponse'
