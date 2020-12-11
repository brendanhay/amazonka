{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a third party job as returned to the pipeline by a job worker. Used for partner actions only.
module Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
  ( -- * Creating a request
    PutThirdPartyJobFailureResult (..),
    mkPutThirdPartyJobFailureResult,

    -- ** Request lenses
    ptpjfrJobId,
    ptpjfrClientToken,
    ptpjfrFailureDetails,

    -- * Destructuring the response
    PutThirdPartyJobFailureResultResponse (..),
    mkPutThirdPartyJobFailureResultResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @PutThirdPartyJobFailureResult@ action.
--
-- /See:/ 'mkPutThirdPartyJobFailureResult' smart constructor.
data PutThirdPartyJobFailureResult = PutThirdPartyJobFailureResult'
  { jobId ::
      Lude.Text,
    clientToken :: Lude.Text,
    failureDetails ::
      FailureDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutThirdPartyJobFailureResult' with the minimum fields required to make a request.
--
-- * 'clientToken' - The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
-- * 'failureDetails' - Represents information about failure details.
-- * 'jobId' - The ID of the job that failed. This is the same ID returned from @PollForThirdPartyJobs@ .
mkPutThirdPartyJobFailureResult ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'clientToken'
  Lude.Text ->
  -- | 'failureDetails'
  FailureDetails ->
  PutThirdPartyJobFailureResult
mkPutThirdPartyJobFailureResult
  pJobId_
  pClientToken_
  pFailureDetails_ =
    PutThirdPartyJobFailureResult'
      { jobId = pJobId_,
        clientToken = pClientToken_,
        failureDetails = pFailureDetails_
      }

-- | The ID of the job that failed. This is the same ID returned from @PollForThirdPartyJobs@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpjfrJobId :: Lens.Lens' PutThirdPartyJobFailureResult Lude.Text
ptpjfrJobId = Lens.lens (jobId :: PutThirdPartyJobFailureResult -> Lude.Text) (\s a -> s {jobId = a} :: PutThirdPartyJobFailureResult)
{-# DEPRECATED ptpjfrJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpjfrClientToken :: Lens.Lens' PutThirdPartyJobFailureResult Lude.Text
ptpjfrClientToken = Lens.lens (clientToken :: PutThirdPartyJobFailureResult -> Lude.Text) (\s a -> s {clientToken = a} :: PutThirdPartyJobFailureResult)
{-# DEPRECATED ptpjfrClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Represents information about failure details.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpjfrFailureDetails :: Lens.Lens' PutThirdPartyJobFailureResult FailureDetails
ptpjfrFailureDetails = Lens.lens (failureDetails :: PutThirdPartyJobFailureResult -> FailureDetails) (\s a -> s {failureDetails = a} :: PutThirdPartyJobFailureResult)
{-# DEPRECATED ptpjfrFailureDetails "Use generic-lens or generic-optics with 'failureDetails' instead." #-}

instance Lude.AWSRequest PutThirdPartyJobFailureResult where
  type
    Rs PutThirdPartyJobFailureResult =
      PutThirdPartyJobFailureResultResponse
  request = Req.postJSON codePipelineService
  response = Res.receiveNull PutThirdPartyJobFailureResultResponse'

instance Lude.ToHeaders PutThirdPartyJobFailureResult where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.PutThirdPartyJobFailureResult" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutThirdPartyJobFailureResult where
  toJSON PutThirdPartyJobFailureResult' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("jobId" Lude..= jobId),
            Lude.Just ("clientToken" Lude..= clientToken),
            Lude.Just ("failureDetails" Lude..= failureDetails)
          ]
      )

instance Lude.ToPath PutThirdPartyJobFailureResult where
  toPath = Lude.const "/"

instance Lude.ToQuery PutThirdPartyJobFailureResult where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutThirdPartyJobFailureResultResponse' smart constructor.
data PutThirdPartyJobFailureResultResponse = PutThirdPartyJobFailureResultResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutThirdPartyJobFailureResultResponse' with the minimum fields required to make a request.
mkPutThirdPartyJobFailureResultResponse ::
  PutThirdPartyJobFailureResultResponse
mkPutThirdPartyJobFailureResultResponse =
  PutThirdPartyJobFailureResultResponse'
