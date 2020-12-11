-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateInfo
  ( OTAUpdateInfo (..),

    -- * Smart constructor
    mkOTAUpdateInfo,

    -- * Lenses
    otauiLastModifiedDate,
    otauiAwsJobExecutionsRolloutConfig,
    otauiAwsIotJobId,
    otauiProtocols,
    otauiAwsJobPresignedURLConfig,
    otauiOtaUpdateFiles,
    otauiOtaUpdateStatus,
    otauiTargets,
    otauiAwsIotJobARN,
    otauiCreationDate,
    otauiAdditionalParameters,
    otauiOtaUpdateId,
    otauiErrorInfo,
    otauiOtaUpdateARN,
    otauiDescription,
    otauiTargetSelection,
  )
where

import Network.AWS.IoT.Types.AWSJobExecutionsRolloutConfig
import Network.AWS.IoT.Types.AWSJobPresignedURLConfig
import Network.AWS.IoT.Types.ErrorInfo
import Network.AWS.IoT.Types.OTAUpdateFile
import Network.AWS.IoT.Types.OTAUpdateStatus
import Network.AWS.IoT.Types.Protocol
import Network.AWS.IoT.Types.TargetSelection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an OTA update.
--
-- /See:/ 'mkOTAUpdateInfo' smart constructor.
data OTAUpdateInfo = OTAUpdateInfo'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    awsJobExecutionsRolloutConfig ::
      Lude.Maybe AWSJobExecutionsRolloutConfig,
    awsIotJobId :: Lude.Maybe Lude.Text,
    protocols :: Lude.Maybe (Lude.NonEmpty Protocol),
    awsJobPresignedURLConfig :: Lude.Maybe AWSJobPresignedURLConfig,
    otaUpdateFiles :: Lude.Maybe (Lude.NonEmpty OTAUpdateFile),
    otaUpdateStatus :: Lude.Maybe OTAUpdateStatus,
    targets :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    awsIotJobARN :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    additionalParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    otaUpdateId :: Lude.Maybe Lude.Text,
    errorInfo :: Lude.Maybe ErrorInfo,
    otaUpdateARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    targetSelection :: Lude.Maybe TargetSelection
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OTAUpdateInfo' with the minimum fields required to make a request.
--
-- * 'additionalParameters' - A collection of name/value pairs
-- * 'awsIotJobARN' - The AWS IoT job ARN associated with the OTA update.
-- * 'awsIotJobId' - The AWS IoT job ID associated with the OTA update.
-- * 'awsJobExecutionsRolloutConfig' - Configuration for the rollout of OTA updates.
-- * 'awsJobPresignedURLConfig' - Configuration information for pre-signed URLs. Valid when @protocols@ contains HTTP.
-- * 'creationDate' - The date when the OTA update was created.
-- * 'description' - A description of the OTA update.
-- * 'errorInfo' - Error information associated with the OTA update.
-- * 'lastModifiedDate' - The date when the OTA update was last updated.
-- * 'otaUpdateARN' - The OTA update ARN.
-- * 'otaUpdateFiles' - A list of files associated with the OTA update.
-- * 'otaUpdateId' - The OTA update ID.
-- * 'otaUpdateStatus' - The status of the OTA update.
-- * 'protocols' - The protocol used to transfer the OTA update image. Valid values are [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the target device can choose the protocol.
-- * 'targetSelection' - Specifies whether the OTA update will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the OTA update (SNAPSHOT). If continuous, the OTA update may also be run on a thing when a change is detected in a target. For example, an OTA update will run on a thing when the thing is added to a target group, even after the OTA update was completed by all things originally in the group.
-- * 'targets' - The targets of the OTA update.
mkOTAUpdateInfo ::
  OTAUpdateInfo
mkOTAUpdateInfo =
  OTAUpdateInfo'
    { lastModifiedDate = Lude.Nothing,
      awsJobExecutionsRolloutConfig = Lude.Nothing,
      awsIotJobId = Lude.Nothing,
      protocols = Lude.Nothing,
      awsJobPresignedURLConfig = Lude.Nothing,
      otaUpdateFiles = Lude.Nothing,
      otaUpdateStatus = Lude.Nothing,
      targets = Lude.Nothing,
      awsIotJobARN = Lude.Nothing,
      creationDate = Lude.Nothing,
      additionalParameters = Lude.Nothing,
      otaUpdateId = Lude.Nothing,
      errorInfo = Lude.Nothing,
      otaUpdateARN = Lude.Nothing,
      description = Lude.Nothing,
      targetSelection = Lude.Nothing
    }

-- | The date when the OTA update was last updated.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiLastModifiedDate :: Lens.Lens' OTAUpdateInfo (Lude.Maybe Lude.Timestamp)
otauiLastModifiedDate = Lens.lens (lastModifiedDate :: OTAUpdateInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Configuration for the rollout of OTA updates.
--
-- /Note:/ Consider using 'awsJobExecutionsRolloutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiAwsJobExecutionsRolloutConfig :: Lens.Lens' OTAUpdateInfo (Lude.Maybe AWSJobExecutionsRolloutConfig)
otauiAwsJobExecutionsRolloutConfig = Lens.lens (awsJobExecutionsRolloutConfig :: OTAUpdateInfo -> Lude.Maybe AWSJobExecutionsRolloutConfig) (\s a -> s {awsJobExecutionsRolloutConfig = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiAwsJobExecutionsRolloutConfig "Use generic-lens or generic-optics with 'awsJobExecutionsRolloutConfig' instead." #-}

-- | The AWS IoT job ID associated with the OTA update.
--
-- /Note:/ Consider using 'awsIotJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiAwsIotJobId :: Lens.Lens' OTAUpdateInfo (Lude.Maybe Lude.Text)
otauiAwsIotJobId = Lens.lens (awsIotJobId :: OTAUpdateInfo -> Lude.Maybe Lude.Text) (\s a -> s {awsIotJobId = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiAwsIotJobId "Use generic-lens or generic-optics with 'awsIotJobId' instead." #-}

-- | The protocol used to transfer the OTA update image. Valid values are [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the target device can choose the protocol.
--
-- /Note:/ Consider using 'protocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiProtocols :: Lens.Lens' OTAUpdateInfo (Lude.Maybe (Lude.NonEmpty Protocol))
otauiProtocols = Lens.lens (protocols :: OTAUpdateInfo -> Lude.Maybe (Lude.NonEmpty Protocol)) (\s a -> s {protocols = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiProtocols "Use generic-lens or generic-optics with 'protocols' instead." #-}

-- | Configuration information for pre-signed URLs. Valid when @protocols@ contains HTTP.
--
-- /Note:/ Consider using 'awsJobPresignedURLConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiAwsJobPresignedURLConfig :: Lens.Lens' OTAUpdateInfo (Lude.Maybe AWSJobPresignedURLConfig)
otauiAwsJobPresignedURLConfig = Lens.lens (awsJobPresignedURLConfig :: OTAUpdateInfo -> Lude.Maybe AWSJobPresignedURLConfig) (\s a -> s {awsJobPresignedURLConfig = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiAwsJobPresignedURLConfig "Use generic-lens or generic-optics with 'awsJobPresignedURLConfig' instead." #-}

-- | A list of files associated with the OTA update.
--
-- /Note:/ Consider using 'otaUpdateFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiOtaUpdateFiles :: Lens.Lens' OTAUpdateInfo (Lude.Maybe (Lude.NonEmpty OTAUpdateFile))
otauiOtaUpdateFiles = Lens.lens (otaUpdateFiles :: OTAUpdateInfo -> Lude.Maybe (Lude.NonEmpty OTAUpdateFile)) (\s a -> s {otaUpdateFiles = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiOtaUpdateFiles "Use generic-lens or generic-optics with 'otaUpdateFiles' instead." #-}

-- | The status of the OTA update.
--
-- /Note:/ Consider using 'otaUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiOtaUpdateStatus :: Lens.Lens' OTAUpdateInfo (Lude.Maybe OTAUpdateStatus)
otauiOtaUpdateStatus = Lens.lens (otaUpdateStatus :: OTAUpdateInfo -> Lude.Maybe OTAUpdateStatus) (\s a -> s {otaUpdateStatus = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiOtaUpdateStatus "Use generic-lens or generic-optics with 'otaUpdateStatus' instead." #-}

-- | The targets of the OTA update.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiTargets :: Lens.Lens' OTAUpdateInfo (Lude.Maybe (Lude.NonEmpty Lude.Text))
otauiTargets = Lens.lens (targets :: OTAUpdateInfo -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {targets = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The AWS IoT job ARN associated with the OTA update.
--
-- /Note:/ Consider using 'awsIotJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiAwsIotJobARN :: Lens.Lens' OTAUpdateInfo (Lude.Maybe Lude.Text)
otauiAwsIotJobARN = Lens.lens (awsIotJobARN :: OTAUpdateInfo -> Lude.Maybe Lude.Text) (\s a -> s {awsIotJobARN = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiAwsIotJobARN "Use generic-lens or generic-optics with 'awsIotJobARN' instead." #-}

-- | The date when the OTA update was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiCreationDate :: Lens.Lens' OTAUpdateInfo (Lude.Maybe Lude.Timestamp)
otauiCreationDate = Lens.lens (creationDate :: OTAUpdateInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A collection of name/value pairs
--
-- /Note:/ Consider using 'additionalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiAdditionalParameters :: Lens.Lens' OTAUpdateInfo (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
otauiAdditionalParameters = Lens.lens (additionalParameters :: OTAUpdateInfo -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {additionalParameters = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiAdditionalParameters "Use generic-lens or generic-optics with 'additionalParameters' instead." #-}

-- | The OTA update ID.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiOtaUpdateId :: Lens.Lens' OTAUpdateInfo (Lude.Maybe Lude.Text)
otauiOtaUpdateId = Lens.lens (otaUpdateId :: OTAUpdateInfo -> Lude.Maybe Lude.Text) (\s a -> s {otaUpdateId = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiOtaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead." #-}

-- | Error information associated with the OTA update.
--
-- /Note:/ Consider using 'errorInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiErrorInfo :: Lens.Lens' OTAUpdateInfo (Lude.Maybe ErrorInfo)
otauiErrorInfo = Lens.lens (errorInfo :: OTAUpdateInfo -> Lude.Maybe ErrorInfo) (\s a -> s {errorInfo = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiErrorInfo "Use generic-lens or generic-optics with 'errorInfo' instead." #-}

-- | The OTA update ARN.
--
-- /Note:/ Consider using 'otaUpdateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiOtaUpdateARN :: Lens.Lens' OTAUpdateInfo (Lude.Maybe Lude.Text)
otauiOtaUpdateARN = Lens.lens (otaUpdateARN :: OTAUpdateInfo -> Lude.Maybe Lude.Text) (\s a -> s {otaUpdateARN = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiOtaUpdateARN "Use generic-lens or generic-optics with 'otaUpdateARN' instead." #-}

-- | A description of the OTA update.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiDescription :: Lens.Lens' OTAUpdateInfo (Lude.Maybe Lude.Text)
otauiDescription = Lens.lens (description :: OTAUpdateInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether the OTA update will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the OTA update (SNAPSHOT). If continuous, the OTA update may also be run on a thing when a change is detected in a target. For example, an OTA update will run on a thing when the thing is added to a target group, even after the OTA update was completed by all things originally in the group.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otauiTargetSelection :: Lens.Lens' OTAUpdateInfo (Lude.Maybe TargetSelection)
otauiTargetSelection = Lens.lens (targetSelection :: OTAUpdateInfo -> Lude.Maybe TargetSelection) (\s a -> s {targetSelection = a} :: OTAUpdateInfo)
{-# DEPRECATED otauiTargetSelection "Use generic-lens or generic-optics with 'targetSelection' instead." #-}

instance Lude.FromJSON OTAUpdateInfo where
  parseJSON =
    Lude.withObject
      "OTAUpdateInfo"
      ( \x ->
          OTAUpdateInfo'
            Lude.<$> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "awsJobExecutionsRolloutConfig")
            Lude.<*> (x Lude..:? "awsIotJobId")
            Lude.<*> (x Lude..:? "protocols")
            Lude.<*> (x Lude..:? "awsJobPresignedUrlConfig")
            Lude.<*> (x Lude..:? "otaUpdateFiles")
            Lude.<*> (x Lude..:? "otaUpdateStatus")
            Lude.<*> (x Lude..:? "targets")
            Lude.<*> (x Lude..:? "awsIotJobArn")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "additionalParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "otaUpdateId")
            Lude.<*> (x Lude..:? "errorInfo")
            Lude.<*> (x Lude..:? "otaUpdateArn")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "targetSelection")
      )
