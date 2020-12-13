{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.GetPendingJobExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the list of all jobs for a thing that are not in a terminal status.
module Network.AWS.IoTJobsData.GetPendingJobExecutions
  ( -- * Creating a request
    GetPendingJobExecutions (..),
    mkGetPendingJobExecutions,

    -- ** Request lenses
    gpjeThingName,

    -- * Destructuring the response
    GetPendingJobExecutionsResponse (..),
    mkGetPendingJobExecutionsResponse,

    -- ** Response lenses
    gpjersInProgressJobs,
    gpjersQueuedJobs,
    gpjersResponseStatus,
  )
where

import Network.AWS.IoTJobsData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPendingJobExecutions' smart constructor.
newtype GetPendingJobExecutions = GetPendingJobExecutions'
  { -- | The name of the thing that is executing the job.
    thingName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPendingJobExecutions' with the minimum fields required to make a request.
--
-- * 'thingName' - The name of the thing that is executing the job.
mkGetPendingJobExecutions ::
  -- | 'thingName'
  Lude.Text ->
  GetPendingJobExecutions
mkGetPendingJobExecutions pThingName_ =
  GetPendingJobExecutions' {thingName = pThingName_}

-- | The name of the thing that is executing the job.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpjeThingName :: Lens.Lens' GetPendingJobExecutions Lude.Text
gpjeThingName = Lens.lens (thingName :: GetPendingJobExecutions -> Lude.Text) (\s a -> s {thingName = a} :: GetPendingJobExecutions)
{-# DEPRECATED gpjeThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest GetPendingJobExecutions where
  type Rs GetPendingJobExecutions = GetPendingJobExecutionsResponse
  request = Req.get ioTJobsDataService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPendingJobExecutionsResponse'
            Lude.<$> (x Lude..?> "inProgressJobs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "queuedJobs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPendingJobExecutions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPendingJobExecutions where
  toPath GetPendingJobExecutions' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/jobs"]

instance Lude.ToQuery GetPendingJobExecutions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPendingJobExecutionsResponse' smart constructor.
data GetPendingJobExecutionsResponse = GetPendingJobExecutionsResponse'
  { -- | A list of JobExecutionSummary objects with status IN_PROGRESS.
    inProgressJobs :: Lude.Maybe [JobExecutionSummary],
    -- | A list of JobExecutionSummary objects with status QUEUED.
    queuedJobs :: Lude.Maybe [JobExecutionSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPendingJobExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'inProgressJobs' - A list of JobExecutionSummary objects with status IN_PROGRESS.
-- * 'queuedJobs' - A list of JobExecutionSummary objects with status QUEUED.
-- * 'responseStatus' - The response status code.
mkGetPendingJobExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPendingJobExecutionsResponse
mkGetPendingJobExecutionsResponse pResponseStatus_ =
  GetPendingJobExecutionsResponse'
    { inProgressJobs = Lude.Nothing,
      queuedJobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of JobExecutionSummary objects with status IN_PROGRESS.
--
-- /Note:/ Consider using 'inProgressJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpjersInProgressJobs :: Lens.Lens' GetPendingJobExecutionsResponse (Lude.Maybe [JobExecutionSummary])
gpjersInProgressJobs = Lens.lens (inProgressJobs :: GetPendingJobExecutionsResponse -> Lude.Maybe [JobExecutionSummary]) (\s a -> s {inProgressJobs = a} :: GetPendingJobExecutionsResponse)
{-# DEPRECATED gpjersInProgressJobs "Use generic-lens or generic-optics with 'inProgressJobs' instead." #-}

-- | A list of JobExecutionSummary objects with status QUEUED.
--
-- /Note:/ Consider using 'queuedJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpjersQueuedJobs :: Lens.Lens' GetPendingJobExecutionsResponse (Lude.Maybe [JobExecutionSummary])
gpjersQueuedJobs = Lens.lens (queuedJobs :: GetPendingJobExecutionsResponse -> Lude.Maybe [JobExecutionSummary]) (\s a -> s {queuedJobs = a} :: GetPendingJobExecutionsResponse)
{-# DEPRECATED gpjersQueuedJobs "Use generic-lens or generic-optics with 'queuedJobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpjersResponseStatus :: Lens.Lens' GetPendingJobExecutionsResponse Lude.Int
gpjersResponseStatus = Lens.lens (responseStatus :: GetPendingJobExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPendingJobExecutionsResponse)
{-# DEPRECATED gpjersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
