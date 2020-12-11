{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.DescribeJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of a job execution.
module Network.AWS.IoTJobsData.DescribeJobExecution
  ( -- * Creating a request
    DescribeJobExecution (..),
    mkDescribeJobExecution,

    -- ** Request lenses
    djeIncludeJobDocument,
    djeExecutionNumber,
    djeJobId,
    djeThingName,

    -- * Destructuring the response
    DescribeJobExecutionResponse (..),
    mkDescribeJobExecutionResponse,

    -- ** Response lenses
    djersExecution,
    djersResponseStatus,
  )
where

import Network.AWS.IoTJobsData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeJobExecution' smart constructor.
data DescribeJobExecution = DescribeJobExecution'
  { includeJobDocument ::
      Lude.Maybe Lude.Bool,
    executionNumber :: Lude.Maybe Lude.Integer,
    jobId :: Lude.Text,
    thingName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJobExecution' with the minimum fields required to make a request.
--
-- * 'executionNumber' - Optional. A number that identifies a particular job execution on a particular device. If not specified, the latest job execution is returned.
-- * 'includeJobDocument' - Optional. When set to true, the response contains the job document. The default is false.
-- * 'jobId' - The unique identifier assigned to this job when it was created.
-- * 'thingName' - The thing name associated with the device the job execution is running on.
mkDescribeJobExecution ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'thingName'
  Lude.Text ->
  DescribeJobExecution
mkDescribeJobExecution pJobId_ pThingName_ =
  DescribeJobExecution'
    { includeJobDocument = Lude.Nothing,
      executionNumber = Lude.Nothing,
      jobId = pJobId_,
      thingName = pThingName_
    }

-- | Optional. When set to true, the response contains the job document. The default is false.
--
-- /Note:/ Consider using 'includeJobDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeIncludeJobDocument :: Lens.Lens' DescribeJobExecution (Lude.Maybe Lude.Bool)
djeIncludeJobDocument = Lens.lens (includeJobDocument :: DescribeJobExecution -> Lude.Maybe Lude.Bool) (\s a -> s {includeJobDocument = a} :: DescribeJobExecution)
{-# DEPRECATED djeIncludeJobDocument "Use generic-lens or generic-optics with 'includeJobDocument' instead." #-}

-- | Optional. A number that identifies a particular job execution on a particular device. If not specified, the latest job execution is returned.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeExecutionNumber :: Lens.Lens' DescribeJobExecution (Lude.Maybe Lude.Integer)
djeExecutionNumber = Lens.lens (executionNumber :: DescribeJobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {executionNumber = a} :: DescribeJobExecution)
{-# DEPRECATED djeExecutionNumber "Use generic-lens or generic-optics with 'executionNumber' instead." #-}

-- | The unique identifier assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeJobId :: Lens.Lens' DescribeJobExecution Lude.Text
djeJobId = Lens.lens (jobId :: DescribeJobExecution -> Lude.Text) (\s a -> s {jobId = a} :: DescribeJobExecution)
{-# DEPRECATED djeJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The thing name associated with the device the job execution is running on.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeThingName :: Lens.Lens' DescribeJobExecution Lude.Text
djeThingName = Lens.lens (thingName :: DescribeJobExecution -> Lude.Text) (\s a -> s {thingName = a} :: DescribeJobExecution)
{-# DEPRECATED djeThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest DescribeJobExecution where
  type Rs DescribeJobExecution = DescribeJobExecutionResponse
  request = Req.get ioTJobsDataService
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
    Lude.mconcat
      [ "includeJobDocument" Lude.=: includeJobDocument,
        "executionNumber" Lude.=: executionNumber
      ]

-- | /See:/ 'mkDescribeJobExecutionResponse' smart constructor.
data DescribeJobExecutionResponse = DescribeJobExecutionResponse'
  { execution ::
      Lude.Maybe JobExecution,
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

-- | Creates a value of 'DescribeJobExecutionResponse' with the minimum fields required to make a request.
--
-- * 'execution' - Contains data about a job execution.
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

-- | Contains data about a job execution.
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
