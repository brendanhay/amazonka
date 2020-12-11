{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AssociateTargetsWithJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a group with a continuous job. The following criteria must be met:
--
--
--     * The job must have been created with the @targetSelection@ field set to "CONTINUOUS".
--
--
--     * The job status must currently be "IN_PROGRESS".
--
--
--     * The total number of targets associated with a job must not exceed 100.
module Network.AWS.IoT.AssociateTargetsWithJob
  ( -- * Creating a request
    AssociateTargetsWithJob (..),
    mkAssociateTargetsWithJob,

    -- ** Request lenses
    atwjNamespaceId,
    atwjComment,
    atwjTargets,
    atwjJobId,

    -- * Destructuring the response
    AssociateTargetsWithJobResponse (..),
    mkAssociateTargetsWithJobResponse,

    -- ** Response lenses
    atwjrsJobId,
    atwjrsJobARN,
    atwjrsDescription,
    atwjrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateTargetsWithJob' smart constructor.
data AssociateTargetsWithJob = AssociateTargetsWithJob'
  { namespaceId ::
      Lude.Maybe Lude.Text,
    comment :: Lude.Maybe Lude.Text,
    targets :: Lude.NonEmpty Lude.Text,
    jobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateTargetsWithJob' with the minimum fields required to make a request.
--
-- * 'comment' - An optional comment string describing why the job was associated with the targets.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
-- * 'targets' - A list of thing group ARNs that define the targets of the job.
mkAssociateTargetsWithJob ::
  -- | 'targets'
  Lude.NonEmpty Lude.Text ->
  -- | 'jobId'
  Lude.Text ->
  AssociateTargetsWithJob
mkAssociateTargetsWithJob pTargets_ pJobId_ =
  AssociateTargetsWithJob'
    { namespaceId = Lude.Nothing,
      comment = Lude.Nothing,
      targets = pTargets_,
      jobId = pJobId_
    }

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjNamespaceId :: Lens.Lens' AssociateTargetsWithJob (Lude.Maybe Lude.Text)
atwjNamespaceId = Lens.lens (namespaceId :: AssociateTargetsWithJob -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: AssociateTargetsWithJob)
{-# DEPRECATED atwjNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | An optional comment string describing why the job was associated with the targets.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjComment :: Lens.Lens' AssociateTargetsWithJob (Lude.Maybe Lude.Text)
atwjComment = Lens.lens (comment :: AssociateTargetsWithJob -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: AssociateTargetsWithJob)
{-# DEPRECATED atwjComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A list of thing group ARNs that define the targets of the job.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjTargets :: Lens.Lens' AssociateTargetsWithJob (Lude.NonEmpty Lude.Text)
atwjTargets = Lens.lens (targets :: AssociateTargetsWithJob -> Lude.NonEmpty Lude.Text) (\s a -> s {targets = a} :: AssociateTargetsWithJob)
{-# DEPRECATED atwjTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjJobId :: Lens.Lens' AssociateTargetsWithJob Lude.Text
atwjJobId = Lens.lens (jobId :: AssociateTargetsWithJob -> Lude.Text) (\s a -> s {jobId = a} :: AssociateTargetsWithJob)
{-# DEPRECATED atwjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest AssociateTargetsWithJob where
  type Rs AssociateTargetsWithJob = AssociateTargetsWithJobResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateTargetsWithJobResponse'
            Lude.<$> (x Lude..?> "jobId")
            Lude.<*> (x Lude..?> "jobArn")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateTargetsWithJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AssociateTargetsWithJob where
  toJSON AssociateTargetsWithJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("comment" Lude..=) Lude.<$> comment,
            Lude.Just ("targets" Lude..= targets)
          ]
      )

instance Lude.ToPath AssociateTargetsWithJob where
  toPath AssociateTargetsWithJob' {..} =
    Lude.mconcat ["/jobs/", Lude.toBS jobId, "/targets"]

instance Lude.ToQuery AssociateTargetsWithJob where
  toQuery AssociateTargetsWithJob' {..} =
    Lude.mconcat ["namespaceId" Lude.=: namespaceId]

-- | /See:/ 'mkAssociateTargetsWithJobResponse' smart constructor.
data AssociateTargetsWithJobResponse = AssociateTargetsWithJobResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobARN ::
      Lude.Maybe Lude.Text,
    description ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'AssociateTargetsWithJobResponse' with the minimum fields required to make a request.
--
-- * 'description' - A short text description of the job.
-- * 'jobARN' - An ARN identifying the job.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'responseStatus' - The response status code.
mkAssociateTargetsWithJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateTargetsWithJobResponse
mkAssociateTargetsWithJobResponse pResponseStatus_ =
  AssociateTargetsWithJobResponse'
    { jobId = Lude.Nothing,
      jobARN = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjrsJobId :: Lens.Lens' AssociateTargetsWithJobResponse (Lude.Maybe Lude.Text)
atwjrsJobId = Lens.lens (jobId :: AssociateTargetsWithJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: AssociateTargetsWithJobResponse)
{-# DEPRECATED atwjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | An ARN identifying the job.
--
-- /Note:/ Consider using 'jobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjrsJobARN :: Lens.Lens' AssociateTargetsWithJobResponse (Lude.Maybe Lude.Text)
atwjrsJobARN = Lens.lens (jobARN :: AssociateTargetsWithJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobARN = a} :: AssociateTargetsWithJobResponse)
{-# DEPRECATED atwjrsJobARN "Use generic-lens or generic-optics with 'jobARN' instead." #-}

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjrsDescription :: Lens.Lens' AssociateTargetsWithJobResponse (Lude.Maybe Lude.Text)
atwjrsDescription = Lens.lens (description :: AssociateTargetsWithJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AssociateTargetsWithJobResponse)
{-# DEPRECATED atwjrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjrsResponseStatus :: Lens.Lens' AssociateTargetsWithJobResponse Lude.Int
atwjrsResponseStatus = Lens.lens (responseStatus :: AssociateTargetsWithJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateTargetsWithJobResponse)
{-# DEPRECATED atwjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
