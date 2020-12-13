{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThingRegistrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a bulk thing provisioning task.
module Network.AWS.IoT.DescribeThingRegistrationTask
  ( -- * Creating a request
    DescribeThingRegistrationTask (..),
    mkDescribeThingRegistrationTask,

    -- ** Request lenses
    dtrtTaskId,

    -- * Destructuring the response
    DescribeThingRegistrationTaskResponse (..),
    mkDescribeThingRegistrationTaskResponse,

    -- ** Response lenses
    dtrtrsStatus,
    dtrtrsLastModifiedDate,
    dtrtrsInputFileKey,
    dtrtrsTaskId,
    dtrtrsCreationDate,
    dtrtrsPercentageProgress,
    dtrtrsTemplateBody,
    dtrtrsSuccessCount,
    dtrtrsMessage,
    dtrtrsFailureCount,
    dtrtrsInputFileBucket,
    dtrtrsRoleARN,
    dtrtrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeThingRegistrationTask' smart constructor.
newtype DescribeThingRegistrationTask = DescribeThingRegistrationTask'
  { -- | The task ID.
    taskId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeThingRegistrationTask' with the minimum fields required to make a request.
--
-- * 'taskId' - The task ID.
mkDescribeThingRegistrationTask ::
  -- | 'taskId'
  Lude.Text ->
  DescribeThingRegistrationTask
mkDescribeThingRegistrationTask pTaskId_ =
  DescribeThingRegistrationTask' {taskId = pTaskId_}

-- | The task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtTaskId :: Lens.Lens' DescribeThingRegistrationTask Lude.Text
dtrtTaskId = Lens.lens (taskId :: DescribeThingRegistrationTask -> Lude.Text) (\s a -> s {taskId = a} :: DescribeThingRegistrationTask)
{-# DEPRECATED dtrtTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest DescribeThingRegistrationTask where
  type
    Rs DescribeThingRegistrationTask =
      DescribeThingRegistrationTaskResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeThingRegistrationTaskResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "lastModifiedDate")
            Lude.<*> (x Lude..?> "inputFileKey")
            Lude.<*> (x Lude..?> "taskId")
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (x Lude..?> "percentageProgress")
            Lude.<*> (x Lude..?> "templateBody")
            Lude.<*> (x Lude..?> "successCount")
            Lude.<*> (x Lude..?> "message")
            Lude.<*> (x Lude..?> "failureCount")
            Lude.<*> (x Lude..?> "inputFileBucket")
            Lude.<*> (x Lude..?> "roleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeThingRegistrationTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeThingRegistrationTask where
  toPath DescribeThingRegistrationTask' {..} =
    Lude.mconcat ["/thing-registration-tasks/", Lude.toBS taskId]

instance Lude.ToQuery DescribeThingRegistrationTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeThingRegistrationTaskResponse' smart constructor.
data DescribeThingRegistrationTaskResponse = DescribeThingRegistrationTaskResponse'
  { -- | The status of the bulk thing provisioning task.
    status :: Lude.Maybe TaskStatus,
    -- | The date when the task was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The input file key.
    inputFileKey :: Lude.Maybe Lude.Text,
    -- | The task ID.
    taskId :: Lude.Maybe Lude.Text,
    -- | The task creation date.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The progress of the bulk provisioning task expressed as a percentage.
    percentageProgress :: Lude.Maybe Lude.Natural,
    -- | The task's template.
    templateBody :: Lude.Maybe Lude.Text,
    -- | The number of things successfully provisioned.
    successCount :: Lude.Maybe Lude.Int,
    -- | The message.
    message :: Lude.Maybe Lude.Text,
    -- | The number of things that failed to be provisioned.
    failureCount :: Lude.Maybe Lude.Int,
    -- | The S3 bucket that contains the input file.
    inputFileBucket :: Lude.Maybe Lude.Text,
    -- | The role ARN that grants access to the input file bucket.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeThingRegistrationTaskResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the bulk thing provisioning task.
-- * 'lastModifiedDate' - The date when the task was last modified.
-- * 'inputFileKey' - The input file key.
-- * 'taskId' - The task ID.
-- * 'creationDate' - The task creation date.
-- * 'percentageProgress' - The progress of the bulk provisioning task expressed as a percentage.
-- * 'templateBody' - The task's template.
-- * 'successCount' - The number of things successfully provisioned.
-- * 'message' - The message.
-- * 'failureCount' - The number of things that failed to be provisioned.
-- * 'inputFileBucket' - The S3 bucket that contains the input file.
-- * 'roleARN' - The role ARN that grants access to the input file bucket.
-- * 'responseStatus' - The response status code.
mkDescribeThingRegistrationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeThingRegistrationTaskResponse
mkDescribeThingRegistrationTaskResponse pResponseStatus_ =
  DescribeThingRegistrationTaskResponse'
    { status = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      inputFileKey = Lude.Nothing,
      taskId = Lude.Nothing,
      creationDate = Lude.Nothing,
      percentageProgress = Lude.Nothing,
      templateBody = Lude.Nothing,
      successCount = Lude.Nothing,
      message = Lude.Nothing,
      failureCount = Lude.Nothing,
      inputFileBucket = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the bulk thing provisioning task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe TaskStatus)
dtrtrsStatus = Lens.lens (status :: DescribeThingRegistrationTaskResponse -> Lude.Maybe TaskStatus) (\s a -> s {status = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date when the task was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsLastModifiedDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Timestamp)
dtrtrsLastModifiedDate = Lens.lens (lastModifiedDate :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The input file key.
--
-- /Note:/ Consider using 'inputFileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsInputFileKey :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Text)
dtrtrsInputFileKey = Lens.lens (inputFileKey :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {inputFileKey = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsInputFileKey "Use generic-lens or generic-optics with 'inputFileKey' instead." #-}

-- | The task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsTaskId :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Text)
dtrtrsTaskId = Lens.lens (taskId :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The task creation date.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsCreationDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Timestamp)
dtrtrsCreationDate = Lens.lens (creationDate :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The progress of the bulk provisioning task expressed as a percentage.
--
-- /Note:/ Consider using 'percentageProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsPercentageProgress :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Natural)
dtrtrsPercentageProgress = Lens.lens (percentageProgress :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Natural) (\s a -> s {percentageProgress = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsPercentageProgress "Use generic-lens or generic-optics with 'percentageProgress' instead." #-}

-- | The task's template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsTemplateBody :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Text)
dtrtrsTemplateBody = Lens.lens (templateBody :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The number of things successfully provisioned.
--
-- /Note:/ Consider using 'successCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsSuccessCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Int)
dtrtrsSuccessCount = Lens.lens (successCount :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Int) (\s a -> s {successCount = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsSuccessCount "Use generic-lens or generic-optics with 'successCount' instead." #-}

-- | The message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsMessage :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Text)
dtrtrsMessage = Lens.lens (message :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The number of things that failed to be provisioned.
--
-- /Note:/ Consider using 'failureCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsFailureCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Int)
dtrtrsFailureCount = Lens.lens (failureCount :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Int) (\s a -> s {failureCount = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsFailureCount "Use generic-lens or generic-optics with 'failureCount' instead." #-}

-- | The S3 bucket that contains the input file.
--
-- /Note:/ Consider using 'inputFileBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsInputFileBucket :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Text)
dtrtrsInputFileBucket = Lens.lens (inputFileBucket :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {inputFileBucket = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsInputFileBucket "Use generic-lens or generic-optics with 'inputFileBucket' instead." #-}

-- | The role ARN that grants access to the input file bucket.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsRoleARN :: Lens.Lens' DescribeThingRegistrationTaskResponse (Lude.Maybe Lude.Text)
dtrtrsRoleARN = Lens.lens (roleARN :: DescribeThingRegistrationTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrsResponseStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse Lude.Int
dtrtrsResponseStatus = Lens.lens (responseStatus :: DescribeThingRegistrationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeThingRegistrationTaskResponse)
{-# DEPRECATED dtrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
