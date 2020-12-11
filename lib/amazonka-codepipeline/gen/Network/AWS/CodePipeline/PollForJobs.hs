{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PollForJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about any jobs for AWS CodePipeline to act on. @PollForJobs@ is valid only for action types with "Custom" in the owner field. If the action type contains "AWS" or "ThirdParty" in the owner field, the @PollForJobs@ action returns an error.
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the S3 bucket used to store artifacts for the pipeline, if the action requires access to that S3 bucket for input or output artifacts. This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.PollForJobs
  ( -- * Creating a request
    PollForJobs (..),
    mkPollForJobs,

    -- ** Request lenses
    pfjMaxBatchSize,
    pfjQueryParam,
    pfjActionTypeId,

    -- * Destructuring the response
    PollForJobsResponse (..),
    mkPollForJobsResponse,

    -- ** Response lenses
    pfjrsJobs,
    pfjrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @PollForJobs@ action.
--
-- /See:/ 'mkPollForJobs' smart constructor.
data PollForJobs = PollForJobs'
  { maxBatchSize ::
      Lude.Maybe Lude.Natural,
    queryParam :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    actionTypeId :: ActionTypeId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PollForJobs' with the minimum fields required to make a request.
--
-- * 'actionTypeId' - Represents information about an action type.
-- * 'maxBatchSize' - The maximum number of jobs to return in a poll for jobs call.
-- * 'queryParam' - A map of property names and values. For an action type with no queryable properties, this value must be null or an empty map. For an action type with a queryable property, you must supply that property as a key in the map. Only jobs whose action configuration matches the mapped value are returned.
mkPollForJobs ::
  -- | 'actionTypeId'
  ActionTypeId ->
  PollForJobs
mkPollForJobs pActionTypeId_ =
  PollForJobs'
    { maxBatchSize = Lude.Nothing,
      queryParam = Lude.Nothing,
      actionTypeId = pActionTypeId_
    }

-- | The maximum number of jobs to return in a poll for jobs call.
--
-- /Note:/ Consider using 'maxBatchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjMaxBatchSize :: Lens.Lens' PollForJobs (Lude.Maybe Lude.Natural)
pfjMaxBatchSize = Lens.lens (maxBatchSize :: PollForJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxBatchSize = a} :: PollForJobs)
{-# DEPRECATED pfjMaxBatchSize "Use generic-lens or generic-optics with 'maxBatchSize' instead." #-}

-- | A map of property names and values. For an action type with no queryable properties, this value must be null or an empty map. For an action type with a queryable property, you must supply that property as a key in the map. Only jobs whose action configuration matches the mapped value are returned.
--
-- /Note:/ Consider using 'queryParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjQueryParam :: Lens.Lens' PollForJobs (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pfjQueryParam = Lens.lens (queryParam :: PollForJobs -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {queryParam = a} :: PollForJobs)
{-# DEPRECATED pfjQueryParam "Use generic-lens or generic-optics with 'queryParam' instead." #-}

-- | Represents information about an action type.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjActionTypeId :: Lens.Lens' PollForJobs ActionTypeId
pfjActionTypeId = Lens.lens (actionTypeId :: PollForJobs -> ActionTypeId) (\s a -> s {actionTypeId = a} :: PollForJobs)
{-# DEPRECATED pfjActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

instance Lude.AWSRequest PollForJobs where
  type Rs PollForJobs = PollForJobsResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          PollForJobsResponse'
            Lude.<$> (x Lude..?> "jobs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PollForJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.PollForJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PollForJobs where
  toJSON PollForJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maxBatchSize" Lude..=) Lude.<$> maxBatchSize,
            ("queryParam" Lude..=) Lude.<$> queryParam,
            Lude.Just ("actionTypeId" Lude..= actionTypeId)
          ]
      )

instance Lude.ToPath PollForJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery PollForJobs where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @PollForJobs@ action.
--
-- /See:/ 'mkPollForJobsResponse' smart constructor.
data PollForJobsResponse = PollForJobsResponse'
  { jobs ::
      Lude.Maybe [Job],
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

-- | Creates a value of 'PollForJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobs' - Information about the jobs to take action on.
-- * 'responseStatus' - The response status code.
mkPollForJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PollForJobsResponse
mkPollForJobsResponse pResponseStatus_ =
  PollForJobsResponse'
    { jobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the jobs to take action on.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjrsJobs :: Lens.Lens' PollForJobsResponse (Lude.Maybe [Job])
pfjrsJobs = Lens.lens (jobs :: PollForJobsResponse -> Lude.Maybe [Job]) (\s a -> s {jobs = a} :: PollForJobsResponse)
{-# DEPRECATED pfjrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjrsResponseStatus :: Lens.Lens' PollForJobsResponse Lude.Int
pfjrsResponseStatus = Lens.lens (responseStatus :: PollForJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PollForJobsResponse)
{-# DEPRECATED pfjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
