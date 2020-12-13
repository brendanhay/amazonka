{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.StartThingRegistrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bulk thing provisioning task.
module Network.AWS.IoT.StartThingRegistrationTask
  ( -- * Creating a request
    StartThingRegistrationTask (..),
    mkStartThingRegistrationTask,

    -- ** Request lenses
    strtInputFileKey,
    strtTemplateBody,
    strtInputFileBucket,
    strtRoleARN,

    -- * Destructuring the response
    StartThingRegistrationTaskResponse (..),
    mkStartThingRegistrationTaskResponse,

    -- ** Response lenses
    strtrsTaskId,
    strtrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartThingRegistrationTask' smart constructor.
data StartThingRegistrationTask = StartThingRegistrationTask'
  { -- | The name of input file within the S3 bucket. This file contains a newline delimited JSON file. Each line contains the parameter values to provision one device (thing).
    inputFileKey :: Lude.Text,
    -- | The provisioning template.
    templateBody :: Lude.Text,
    -- | The S3 bucket that contains the input file.
    inputFileBucket :: Lude.Text,
    -- | The IAM role ARN that grants permission the input file.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartThingRegistrationTask' with the minimum fields required to make a request.
--
-- * 'inputFileKey' - The name of input file within the S3 bucket. This file contains a newline delimited JSON file. Each line contains the parameter values to provision one device (thing).
-- * 'templateBody' - The provisioning template.
-- * 'inputFileBucket' - The S3 bucket that contains the input file.
-- * 'roleARN' - The IAM role ARN that grants permission the input file.
mkStartThingRegistrationTask ::
  -- | 'inputFileKey'
  Lude.Text ->
  -- | 'templateBody'
  Lude.Text ->
  -- | 'inputFileBucket'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  StartThingRegistrationTask
mkStartThingRegistrationTask
  pInputFileKey_
  pTemplateBody_
  pInputFileBucket_
  pRoleARN_ =
    StartThingRegistrationTask'
      { inputFileKey = pInputFileKey_,
        templateBody = pTemplateBody_,
        inputFileBucket = pInputFileBucket_,
        roleARN = pRoleARN_
      }

-- | The name of input file within the S3 bucket. This file contains a newline delimited JSON file. Each line contains the parameter values to provision one device (thing).
--
-- /Note:/ Consider using 'inputFileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtInputFileKey :: Lens.Lens' StartThingRegistrationTask Lude.Text
strtInputFileKey = Lens.lens (inputFileKey :: StartThingRegistrationTask -> Lude.Text) (\s a -> s {inputFileKey = a} :: StartThingRegistrationTask)
{-# DEPRECATED strtInputFileKey "Use generic-lens or generic-optics with 'inputFileKey' instead." #-}

-- | The provisioning template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtTemplateBody :: Lens.Lens' StartThingRegistrationTask Lude.Text
strtTemplateBody = Lens.lens (templateBody :: StartThingRegistrationTask -> Lude.Text) (\s a -> s {templateBody = a} :: StartThingRegistrationTask)
{-# DEPRECATED strtTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The S3 bucket that contains the input file.
--
-- /Note:/ Consider using 'inputFileBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtInputFileBucket :: Lens.Lens' StartThingRegistrationTask Lude.Text
strtInputFileBucket = Lens.lens (inputFileBucket :: StartThingRegistrationTask -> Lude.Text) (\s a -> s {inputFileBucket = a} :: StartThingRegistrationTask)
{-# DEPRECATED strtInputFileBucket "Use generic-lens or generic-optics with 'inputFileBucket' instead." #-}

-- | The IAM role ARN that grants permission the input file.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtRoleARN :: Lens.Lens' StartThingRegistrationTask Lude.Text
strtRoleARN = Lens.lens (roleARN :: StartThingRegistrationTask -> Lude.Text) (\s a -> s {roleARN = a} :: StartThingRegistrationTask)
{-# DEPRECATED strtRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest StartThingRegistrationTask where
  type
    Rs StartThingRegistrationTask =
      StartThingRegistrationTaskResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartThingRegistrationTaskResponse'
            Lude.<$> (x Lude..?> "taskId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartThingRegistrationTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON StartThingRegistrationTask where
  toJSON StartThingRegistrationTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("inputFileKey" Lude..= inputFileKey),
            Lude.Just ("templateBody" Lude..= templateBody),
            Lude.Just ("inputFileBucket" Lude..= inputFileBucket),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath StartThingRegistrationTask where
  toPath = Lude.const "/thing-registration-tasks"

instance Lude.ToQuery StartThingRegistrationTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartThingRegistrationTaskResponse' smart constructor.
data StartThingRegistrationTaskResponse = StartThingRegistrationTaskResponse'
  { -- | The bulk thing provisioning task ID.
    taskId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartThingRegistrationTaskResponse' with the minimum fields required to make a request.
--
-- * 'taskId' - The bulk thing provisioning task ID.
-- * 'responseStatus' - The response status code.
mkStartThingRegistrationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartThingRegistrationTaskResponse
mkStartThingRegistrationTaskResponse pResponseStatus_ =
  StartThingRegistrationTaskResponse'
    { taskId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The bulk thing provisioning task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtrsTaskId :: Lens.Lens' StartThingRegistrationTaskResponse (Lude.Maybe Lude.Text)
strtrsTaskId = Lens.lens (taskId :: StartThingRegistrationTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: StartThingRegistrationTaskResponse)
{-# DEPRECATED strtrsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtrsResponseStatus :: Lens.Lens' StartThingRegistrationTaskResponse Lude.Int
strtrsResponseStatus = Lens.lens (responseStatus :: StartThingRegistrationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartThingRegistrationTaskResponse)
{-# DEPRECATED strtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
