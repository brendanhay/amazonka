{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateOTAUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS IoT OTAUpdate on a target group of things or groups.
module Network.AWS.IoT.CreateOTAUpdate
  ( -- * Creating a request
    CreateOTAUpdate (..),
    mkCreateOTAUpdate,

    -- ** Request lenses
    cotauAwsJobAbortConfig,
    cotauAwsJobExecutionsRolloutConfig,
    cotauProtocols,
    cotauAwsJobPresignedURLConfig,
    cotauTargets,
    cotauFiles,
    cotauAdditionalParameters,
    cotauAwsJobTimeoutConfig,
    cotauOtaUpdateId,
    cotauDescription,
    cotauTargetSelection,
    cotauTags,
    cotauRoleARN,

    -- * Destructuring the response
    CreateOTAUpdateResponse (..),
    mkCreateOTAUpdateResponse,

    -- ** Response lenses
    cotaursAwsIotJobId,
    cotaursOtaUpdateStatus,
    cotaursAwsIotJobARN,
    cotaursOtaUpdateId,
    cotaursOtaUpdateARN,
    cotaursResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateOTAUpdate' smart constructor.
data CreateOTAUpdate = CreateOTAUpdate'
  { -- | The criteria that determine when and how a job abort takes place.
    awsJobAbortConfig :: Lude.Maybe AWSJobAbortConfig,
    -- | Configuration for the rollout of OTA updates.
    awsJobExecutionsRolloutConfig :: Lude.Maybe AWSJobExecutionsRolloutConfig,
    -- | The protocol used to transfer the OTA update image. Valid values are [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the target device can choose the protocol.
    protocols :: Lude.Maybe (Lude.NonEmpty Protocol),
    -- | Configuration information for pre-signed URLs.
    awsJobPresignedURLConfig :: Lude.Maybe AWSJobPresignedURLConfig,
    -- | The devices targeted to receive OTA updates.
    targets :: Lude.NonEmpty Lude.Text,
    -- | The files to be streamed by the OTA update.
    files :: Lude.NonEmpty OTAUpdateFile,
    -- | A list of additional OTA update parameters which are name-value pairs.
    additionalParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
    awsJobTimeoutConfig :: Lude.Maybe AWSJobTimeoutConfig,
    -- | The ID of the OTA update to be created.
    otaUpdateId :: Lude.Text,
    -- | The description of the OTA update.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies whether the update will continue to run (CONTINUOUS), or will be complete after all the things specified as targets have completed the update (SNAPSHOT). If continuous, the update may also be run on a thing when a change is detected in a target. For example, an update will run on a thing when the thing is added to a target group, even after the update was completed by all things originally in the group. Valid values: CONTINUOUS | SNAPSHOT.
    targetSelection :: Lude.Maybe TargetSelection,
    -- | Metadata which can be used to manage updates.
    tags :: Lude.Maybe [Tag],
    -- | The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs and AWS Code Signing resources to create an OTA update job.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOTAUpdate' with the minimum fields required to make a request.
--
-- * 'awsJobAbortConfig' - The criteria that determine when and how a job abort takes place.
-- * 'awsJobExecutionsRolloutConfig' - Configuration for the rollout of OTA updates.
-- * 'protocols' - The protocol used to transfer the OTA update image. Valid values are [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the target device can choose the protocol.
-- * 'awsJobPresignedURLConfig' - Configuration information for pre-signed URLs.
-- * 'targets' - The devices targeted to receive OTA updates.
-- * 'files' - The files to be streamed by the OTA update.
-- * 'additionalParameters' - A list of additional OTA update parameters which are name-value pairs.
-- * 'awsJobTimeoutConfig' - Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
-- * 'otaUpdateId' - The ID of the OTA update to be created.
-- * 'description' - The description of the OTA update.
-- * 'targetSelection' - Specifies whether the update will continue to run (CONTINUOUS), or will be complete after all the things specified as targets have completed the update (SNAPSHOT). If continuous, the update may also be run on a thing when a change is detected in a target. For example, an update will run on a thing when the thing is added to a target group, even after the update was completed by all things originally in the group. Valid values: CONTINUOUS | SNAPSHOT.
-- * 'tags' - Metadata which can be used to manage updates.
-- * 'roleARN' - The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs and AWS Code Signing resources to create an OTA update job.
mkCreateOTAUpdate ::
  -- | 'targets'
  Lude.NonEmpty Lude.Text ->
  -- | 'files'
  Lude.NonEmpty OTAUpdateFile ->
  -- | 'otaUpdateId'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CreateOTAUpdate
mkCreateOTAUpdate pTargets_ pFiles_ pOtaUpdateId_ pRoleARN_ =
  CreateOTAUpdate'
    { awsJobAbortConfig = Lude.Nothing,
      awsJobExecutionsRolloutConfig = Lude.Nothing,
      protocols = Lude.Nothing,
      awsJobPresignedURLConfig = Lude.Nothing,
      targets = pTargets_,
      files = pFiles_,
      additionalParameters = Lude.Nothing,
      awsJobTimeoutConfig = Lude.Nothing,
      otaUpdateId = pOtaUpdateId_,
      description = Lude.Nothing,
      targetSelection = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The criteria that determine when and how a job abort takes place.
--
-- /Note:/ Consider using 'awsJobAbortConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAwsJobAbortConfig :: Lens.Lens' CreateOTAUpdate (Lude.Maybe AWSJobAbortConfig)
cotauAwsJobAbortConfig = Lens.lens (awsJobAbortConfig :: CreateOTAUpdate -> Lude.Maybe AWSJobAbortConfig) (\s a -> s {awsJobAbortConfig = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauAwsJobAbortConfig "Use generic-lens or generic-optics with 'awsJobAbortConfig' instead." #-}

-- | Configuration for the rollout of OTA updates.
--
-- /Note:/ Consider using 'awsJobExecutionsRolloutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAwsJobExecutionsRolloutConfig :: Lens.Lens' CreateOTAUpdate (Lude.Maybe AWSJobExecutionsRolloutConfig)
cotauAwsJobExecutionsRolloutConfig = Lens.lens (awsJobExecutionsRolloutConfig :: CreateOTAUpdate -> Lude.Maybe AWSJobExecutionsRolloutConfig) (\s a -> s {awsJobExecutionsRolloutConfig = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauAwsJobExecutionsRolloutConfig "Use generic-lens or generic-optics with 'awsJobExecutionsRolloutConfig' instead." #-}

-- | The protocol used to transfer the OTA update image. Valid values are [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the target device can choose the protocol.
--
-- /Note:/ Consider using 'protocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauProtocols :: Lens.Lens' CreateOTAUpdate (Lude.Maybe (Lude.NonEmpty Protocol))
cotauProtocols = Lens.lens (protocols :: CreateOTAUpdate -> Lude.Maybe (Lude.NonEmpty Protocol)) (\s a -> s {protocols = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauProtocols "Use generic-lens or generic-optics with 'protocols' instead." #-}

-- | Configuration information for pre-signed URLs.
--
-- /Note:/ Consider using 'awsJobPresignedURLConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAwsJobPresignedURLConfig :: Lens.Lens' CreateOTAUpdate (Lude.Maybe AWSJobPresignedURLConfig)
cotauAwsJobPresignedURLConfig = Lens.lens (awsJobPresignedURLConfig :: CreateOTAUpdate -> Lude.Maybe AWSJobPresignedURLConfig) (\s a -> s {awsJobPresignedURLConfig = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauAwsJobPresignedURLConfig "Use generic-lens or generic-optics with 'awsJobPresignedURLConfig' instead." #-}

-- | The devices targeted to receive OTA updates.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauTargets :: Lens.Lens' CreateOTAUpdate (Lude.NonEmpty Lude.Text)
cotauTargets = Lens.lens (targets :: CreateOTAUpdate -> Lude.NonEmpty Lude.Text) (\s a -> s {targets = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The files to be streamed by the OTA update.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauFiles :: Lens.Lens' CreateOTAUpdate (Lude.NonEmpty OTAUpdateFile)
cotauFiles = Lens.lens (files :: CreateOTAUpdate -> Lude.NonEmpty OTAUpdateFile) (\s a -> s {files = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauFiles "Use generic-lens or generic-optics with 'files' instead." #-}

-- | A list of additional OTA update parameters which are name-value pairs.
--
-- /Note:/ Consider using 'additionalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAdditionalParameters :: Lens.Lens' CreateOTAUpdate (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cotauAdditionalParameters = Lens.lens (additionalParameters :: CreateOTAUpdate -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {additionalParameters = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauAdditionalParameters "Use generic-lens or generic-optics with 'additionalParameters' instead." #-}

-- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
--
-- /Note:/ Consider using 'awsJobTimeoutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauAwsJobTimeoutConfig :: Lens.Lens' CreateOTAUpdate (Lude.Maybe AWSJobTimeoutConfig)
cotauAwsJobTimeoutConfig = Lens.lens (awsJobTimeoutConfig :: CreateOTAUpdate -> Lude.Maybe AWSJobTimeoutConfig) (\s a -> s {awsJobTimeoutConfig = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauAwsJobTimeoutConfig "Use generic-lens or generic-optics with 'awsJobTimeoutConfig' instead." #-}

-- | The ID of the OTA update to be created.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauOtaUpdateId :: Lens.Lens' CreateOTAUpdate Lude.Text
cotauOtaUpdateId = Lens.lens (otaUpdateId :: CreateOTAUpdate -> Lude.Text) (\s a -> s {otaUpdateId = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauOtaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead." #-}

-- | The description of the OTA update.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauDescription :: Lens.Lens' CreateOTAUpdate (Lude.Maybe Lude.Text)
cotauDescription = Lens.lens (description :: CreateOTAUpdate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether the update will continue to run (CONTINUOUS), or will be complete after all the things specified as targets have completed the update (SNAPSHOT). If continuous, the update may also be run on a thing when a change is detected in a target. For example, an update will run on a thing when the thing is added to a target group, even after the update was completed by all things originally in the group. Valid values: CONTINUOUS | SNAPSHOT.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauTargetSelection :: Lens.Lens' CreateOTAUpdate (Lude.Maybe TargetSelection)
cotauTargetSelection = Lens.lens (targetSelection :: CreateOTAUpdate -> Lude.Maybe TargetSelection) (\s a -> s {targetSelection = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauTargetSelection "Use generic-lens or generic-optics with 'targetSelection' instead." #-}

-- | Metadata which can be used to manage updates.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauTags :: Lens.Lens' CreateOTAUpdate (Lude.Maybe [Tag])
cotauTags = Lens.lens (tags :: CreateOTAUpdate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs and AWS Code Signing resources to create an OTA update job.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotauRoleARN :: Lens.Lens' CreateOTAUpdate Lude.Text
cotauRoleARN = Lens.lens (roleARN :: CreateOTAUpdate -> Lude.Text) (\s a -> s {roleARN = a} :: CreateOTAUpdate)
{-# DEPRECATED cotauRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateOTAUpdate where
  type Rs CreateOTAUpdate = CreateOTAUpdateResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateOTAUpdateResponse'
            Lude.<$> (x Lude..?> "awsIotJobId")
            Lude.<*> (x Lude..?> "otaUpdateStatus")
            Lude.<*> (x Lude..?> "awsIotJobArn")
            Lude.<*> (x Lude..?> "otaUpdateId")
            Lude.<*> (x Lude..?> "otaUpdateArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateOTAUpdate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateOTAUpdate where
  toJSON CreateOTAUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("awsJobAbortConfig" Lude..=) Lude.<$> awsJobAbortConfig,
            ("awsJobExecutionsRolloutConfig" Lude..=)
              Lude.<$> awsJobExecutionsRolloutConfig,
            ("protocols" Lude..=) Lude.<$> protocols,
            ("awsJobPresignedUrlConfig" Lude..=)
              Lude.<$> awsJobPresignedURLConfig,
            Lude.Just ("targets" Lude..= targets),
            Lude.Just ("files" Lude..= files),
            ("additionalParameters" Lude..=) Lude.<$> additionalParameters,
            ("awsJobTimeoutConfig" Lude..=) Lude.<$> awsJobTimeoutConfig,
            ("description" Lude..=) Lude.<$> description,
            ("targetSelection" Lude..=) Lude.<$> targetSelection,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateOTAUpdate where
  toPath CreateOTAUpdate' {..} =
    Lude.mconcat ["/otaUpdates/", Lude.toBS otaUpdateId]

instance Lude.ToQuery CreateOTAUpdate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateOTAUpdateResponse' smart constructor.
data CreateOTAUpdateResponse = CreateOTAUpdateResponse'
  { -- | The AWS IoT job ID associated with the OTA update.
    awsIotJobId :: Lude.Maybe Lude.Text,
    -- | The OTA update status.
    otaUpdateStatus :: Lude.Maybe OTAUpdateStatus,
    -- | The AWS IoT job ARN associated with the OTA update.
    awsIotJobARN :: Lude.Maybe Lude.Text,
    -- | The OTA update ID.
    otaUpdateId :: Lude.Maybe Lude.Text,
    -- | The OTA update ARN.
    otaUpdateARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOTAUpdateResponse' with the minimum fields required to make a request.
--
-- * 'awsIotJobId' - The AWS IoT job ID associated with the OTA update.
-- * 'otaUpdateStatus' - The OTA update status.
-- * 'awsIotJobARN' - The AWS IoT job ARN associated with the OTA update.
-- * 'otaUpdateId' - The OTA update ID.
-- * 'otaUpdateARN' - The OTA update ARN.
-- * 'responseStatus' - The response status code.
mkCreateOTAUpdateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOTAUpdateResponse
mkCreateOTAUpdateResponse pResponseStatus_ =
  CreateOTAUpdateResponse'
    { awsIotJobId = Lude.Nothing,
      otaUpdateStatus = Lude.Nothing,
      awsIotJobARN = Lude.Nothing,
      otaUpdateId = Lude.Nothing,
      otaUpdateARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The AWS IoT job ID associated with the OTA update.
--
-- /Note:/ Consider using 'awsIotJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaursAwsIotJobId :: Lens.Lens' CreateOTAUpdateResponse (Lude.Maybe Lude.Text)
cotaursAwsIotJobId = Lens.lens (awsIotJobId :: CreateOTAUpdateResponse -> Lude.Maybe Lude.Text) (\s a -> s {awsIotJobId = a} :: CreateOTAUpdateResponse)
{-# DEPRECATED cotaursAwsIotJobId "Use generic-lens or generic-optics with 'awsIotJobId' instead." #-}

-- | The OTA update status.
--
-- /Note:/ Consider using 'otaUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaursOtaUpdateStatus :: Lens.Lens' CreateOTAUpdateResponse (Lude.Maybe OTAUpdateStatus)
cotaursOtaUpdateStatus = Lens.lens (otaUpdateStatus :: CreateOTAUpdateResponse -> Lude.Maybe OTAUpdateStatus) (\s a -> s {otaUpdateStatus = a} :: CreateOTAUpdateResponse)
{-# DEPRECATED cotaursOtaUpdateStatus "Use generic-lens or generic-optics with 'otaUpdateStatus' instead." #-}

-- | The AWS IoT job ARN associated with the OTA update.
--
-- /Note:/ Consider using 'awsIotJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaursAwsIotJobARN :: Lens.Lens' CreateOTAUpdateResponse (Lude.Maybe Lude.Text)
cotaursAwsIotJobARN = Lens.lens (awsIotJobARN :: CreateOTAUpdateResponse -> Lude.Maybe Lude.Text) (\s a -> s {awsIotJobARN = a} :: CreateOTAUpdateResponse)
{-# DEPRECATED cotaursAwsIotJobARN "Use generic-lens or generic-optics with 'awsIotJobARN' instead." #-}

-- | The OTA update ID.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaursOtaUpdateId :: Lens.Lens' CreateOTAUpdateResponse (Lude.Maybe Lude.Text)
cotaursOtaUpdateId = Lens.lens (otaUpdateId :: CreateOTAUpdateResponse -> Lude.Maybe Lude.Text) (\s a -> s {otaUpdateId = a} :: CreateOTAUpdateResponse)
{-# DEPRECATED cotaursOtaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead." #-}

-- | The OTA update ARN.
--
-- /Note:/ Consider using 'otaUpdateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaursOtaUpdateARN :: Lens.Lens' CreateOTAUpdateResponse (Lude.Maybe Lude.Text)
cotaursOtaUpdateARN = Lens.lens (otaUpdateARN :: CreateOTAUpdateResponse -> Lude.Maybe Lude.Text) (\s a -> s {otaUpdateARN = a} :: CreateOTAUpdateResponse)
{-# DEPRECATED cotaursOtaUpdateARN "Use generic-lens or generic-optics with 'otaUpdateARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cotaursResponseStatus :: Lens.Lens' CreateOTAUpdateResponse Lude.Int
cotaursResponseStatus = Lens.lens (responseStatus :: CreateOTAUpdateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOTAUpdateResponse)
{-# DEPRECATED cotaursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
