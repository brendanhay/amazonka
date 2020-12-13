{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a job execution.
module Network.AWS.IoT.DescribeJobExecution
  ( -- * Creating a request
    DescribeJobExecution (..),
    mkDescribeJobExecution,

    -- ** Request lenses
    djefJobId,
    djefExecutionNumber,
    djefThingName,

    -- * Destructuring the response
    DescribeJobExecutionResponse (..),
    mkDescribeJobExecutionResponse,

    -- ** Response lenses
    djersExecution,
    djersResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeJobExecution' smart constructor.
data DescribeJobExecution = DescribeJobExecution'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Lude.Text,
    -- | A string (consisting of the digits "0" through "9" which is used to specify a particular job execution on a particular device.
    executionNumber :: Lude.Maybe Lude.Integer,
    -- | The name of the thing on which the job execution is running.
    thingName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJobExecution' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'executionNumber' - A string (consisting of the digits "0" through "9" which is used to specify a particular job execution on a particular device.
-- * 'thingName' - The name of the thing on which the job execution is running.
mkDescribeJobExecution ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'thingName'
  Lude.Text ->
  DescribeJobExecution
mkDescribeJobExecution pJobId_ pThingName_ =
  DescribeJobExecution'
    { jobId = pJobId_,
      executionNumber = Lude.Nothing,
      thingName = pThingName_
    }

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djefJobId :: Lens.Lens' DescribeJobExecution Lude.Text
djefJobId = Lens.lens (jobId :: DescribeJobExecution -> Lude.Text) (\s a -> s {jobId = a} :: DescribeJobExecution)
{-# DEPRECATED djefJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | A string (consisting of the digits "0" through "9" which is used to specify a particular job execution on a particular device.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djefExecutionNumber :: Lens.Lens' DescribeJobExecution (Lude.Maybe Lude.Integer)
djefExecutionNumber = Lens.lens (executionNumber :: DescribeJobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {executionNumber = a} :: DescribeJobExecution)
{-# DEPRECATED djefExecutionNumber "Use generic-lens or generic-optics with 'executionNumber' instead." #-}

-- | The name of the thing on which the job execution is running.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djefThingName :: Lens.Lens' DescribeJobExecution Lude.Text
djefThingName = Lens.lens (thingName :: DescribeJobExecution -> Lude.Text) (\s a -> s {thingName = a} :: DescribeJobExecution)
{-# DEPRECATED djefThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest DescribeJobExecution where
  type Rs DescribeJobExecution = DescribeJobExecutionResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeJobExecutionResponse'
            Lude.<$> (x Lude..?> "execution") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeJobExecution where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeJobExecution where
  toPath DescribeJobExecution' {..} =
    Lude.mconcat
      ["/things/", Lude.toBS thingName, "/jobs/", Lude.toBS jobId]

instance Lude.ToQuery DescribeJobExecution where
  toQuery DescribeJobExecution' {..} =
    Lude.mconcat ["executionNumber" Lude.=: executionNumber]

-- | /See:/ 'mkDescribeJobExecutionResponse' smart constructor.
data DescribeJobExecutionResponse = DescribeJobExecutionResponse'
  { -- | Information about the job execution.
    execution :: Lude.Maybe JobExecution,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJobExecutionResponse' with the minimum fields required to make a request.
--
-- * 'execution' - Information about the job execution.
-- * 'responseStatus' - The response status code.
mkDescribeJobExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeJobExecutionResponse
mkDescribeJobExecutionResponse pResponseStatus_ =
  DescribeJobExecutionResponse'
    { execution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the job execution.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djersExecution :: Lens.Lens' DescribeJobExecutionResponse (Lude.Maybe JobExecution)
djersExecution = Lens.lens (execution :: DescribeJobExecutionResponse -> Lude.Maybe JobExecution) (\s a -> s {execution = a} :: DescribeJobExecutionResponse)
{-# DEPRECATED djersExecution "Use generic-lens or generic-optics with 'execution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djersResponseStatus :: Lens.Lens' DescribeJobExecutionResponse Lude.Int
djersResponseStatus = Lens.lens (responseStatus :: DescribeJobExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeJobExecutionResponse)
{-# DEPRECATED djersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
