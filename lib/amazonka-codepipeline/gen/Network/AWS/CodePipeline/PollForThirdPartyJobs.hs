{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PollForThirdPartyJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines whether there are any third party jobs for a job worker to act on. Used for partner actions only.
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the S3 bucket used to store artifacts for the pipeline, if the action requires access to that S3 bucket for input or output artifacts.
module Network.AWS.CodePipeline.PollForThirdPartyJobs
  ( -- * Creating a request
    PollForThirdPartyJobs (..),
    mkPollForThirdPartyJobs,

    -- ** Request lenses
    pftpjMaxBatchSize,
    pftpjActionTypeId,

    -- * Destructuring the response
    PollForThirdPartyJobsResponse (..),
    mkPollForThirdPartyJobsResponse,

    -- ** Response lenses
    pftpjrsJobs,
    pftpjrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @PollForThirdPartyJobs@ action.
--
-- /See:/ 'mkPollForThirdPartyJobs' smart constructor.
data PollForThirdPartyJobs = PollForThirdPartyJobs'
  { -- | The maximum number of jobs to return in a poll for jobs call.
    maxBatchSize :: Lude.Maybe Lude.Natural,
    -- | Represents information about an action type.
    actionTypeId :: ActionTypeId
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PollForThirdPartyJobs' with the minimum fields required to make a request.
--
-- * 'maxBatchSize' - The maximum number of jobs to return in a poll for jobs call.
-- * 'actionTypeId' - Represents information about an action type.
mkPollForThirdPartyJobs ::
  -- | 'actionTypeId'
  ActionTypeId ->
  PollForThirdPartyJobs
mkPollForThirdPartyJobs pActionTypeId_ =
  PollForThirdPartyJobs'
    { maxBatchSize = Lude.Nothing,
      actionTypeId = pActionTypeId_
    }

-- | The maximum number of jobs to return in a poll for jobs call.
--
-- /Note:/ Consider using 'maxBatchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjMaxBatchSize :: Lens.Lens' PollForThirdPartyJobs (Lude.Maybe Lude.Natural)
pftpjMaxBatchSize = Lens.lens (maxBatchSize :: PollForThirdPartyJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxBatchSize = a} :: PollForThirdPartyJobs)
{-# DEPRECATED pftpjMaxBatchSize "Use generic-lens or generic-optics with 'maxBatchSize' instead." #-}

-- | Represents information about an action type.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjActionTypeId :: Lens.Lens' PollForThirdPartyJobs ActionTypeId
pftpjActionTypeId = Lens.lens (actionTypeId :: PollForThirdPartyJobs -> ActionTypeId) (\s a -> s {actionTypeId = a} :: PollForThirdPartyJobs)
{-# DEPRECATED pftpjActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

instance Lude.AWSRequest PollForThirdPartyJobs where
  type Rs PollForThirdPartyJobs = PollForThirdPartyJobsResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          PollForThirdPartyJobsResponse'
            Lude.<$> (x Lude..?> "jobs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PollForThirdPartyJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.PollForThirdPartyJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PollForThirdPartyJobs where
  toJSON PollForThirdPartyJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maxBatchSize" Lude..=) Lude.<$> maxBatchSize,
            Lude.Just ("actionTypeId" Lude..= actionTypeId)
          ]
      )

instance Lude.ToPath PollForThirdPartyJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery PollForThirdPartyJobs where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @PollForThirdPartyJobs@ action.
--
-- /See:/ 'mkPollForThirdPartyJobsResponse' smart constructor.
data PollForThirdPartyJobsResponse = PollForThirdPartyJobsResponse'
  { -- | Information about the jobs to take action on.
    jobs :: Lude.Maybe [ThirdPartyJob],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PollForThirdPartyJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobs' - Information about the jobs to take action on.
-- * 'responseStatus' - The response status code.
mkPollForThirdPartyJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PollForThirdPartyJobsResponse
mkPollForThirdPartyJobsResponse pResponseStatus_ =
  PollForThirdPartyJobsResponse'
    { jobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the jobs to take action on.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjrsJobs :: Lens.Lens' PollForThirdPartyJobsResponse (Lude.Maybe [ThirdPartyJob])
pftpjrsJobs = Lens.lens (jobs :: PollForThirdPartyJobsResponse -> Lude.Maybe [ThirdPartyJob]) (\s a -> s {jobs = a} :: PollForThirdPartyJobsResponse)
{-# DEPRECATED pftpjrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjrsResponseStatus :: Lens.Lens' PollForThirdPartyJobsResponse Lude.Int
pftpjrsResponseStatus = Lens.lens (responseStatus :: PollForThirdPartyJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PollForThirdPartyJobsResponse)
{-# DEPRECATED pftpjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
