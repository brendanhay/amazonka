{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AwsJobExecutionsRolloutConfig
import Network.AWS.IoT.Types.AwsJobPresignedUrlConfig
import Network.AWS.IoT.Types.ErrorInfo
import Network.AWS.IoT.Types.OTAUpdateFile
import Network.AWS.IoT.Types.OTAUpdateStatus
import Network.AWS.IoT.Types.Protocol
import Network.AWS.IoT.Types.TargetSelection
import qualified Network.AWS.Lens as Lens

-- | Information about an OTA update.
--
-- /See:/ 'newOTAUpdateInfo' smart constructor.
data OTAUpdateInfo = OTAUpdateInfo'
  { -- | The status of the OTA update.
    otaUpdateStatus :: Core.Maybe OTAUpdateStatus,
    -- | The date when the OTA update was last updated.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | Specifies whether the OTA update will continue to run (CONTINUOUS), or
    -- will be complete after all those things specified as targets have
    -- completed the OTA update (SNAPSHOT). If continuous, the OTA update may
    -- also be run on a thing when a change is detected in a target. For
    -- example, an OTA update will run on a thing when the thing is added to a
    -- target group, even after the OTA update was completed by all things
    -- originally in the group.
    targetSelection :: Core.Maybe TargetSelection,
    -- | The OTA update ARN.
    otaUpdateArn :: Core.Maybe Core.Text,
    -- | The AWS IoT job ID associated with the OTA update.
    awsIotJobId :: Core.Maybe Core.Text,
    -- | The date when the OTA update was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The AWS IoT job ARN associated with the OTA update.
    awsIotJobArn :: Core.Maybe Core.Text,
    -- | The protocol used to transfer the OTA update image. Valid values are
    -- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
    -- target device can choose the protocol.
    protocols :: Core.Maybe (Core.NonEmpty Protocol),
    -- | The targets of the OTA update.
    targets :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Configuration information for pre-signed URLs. Valid when @protocols@
    -- contains HTTP.
    awsJobPresignedUrlConfig :: Core.Maybe AwsJobPresignedUrlConfig,
    -- | Error information associated with the OTA update.
    errorInfo :: Core.Maybe ErrorInfo,
    -- | A description of the OTA update.
    description :: Core.Maybe Core.Text,
    -- | A list of files associated with the OTA update.
    otaUpdateFiles :: Core.Maybe (Core.NonEmpty OTAUpdateFile),
    -- | The OTA update ID.
    otaUpdateId :: Core.Maybe Core.Text,
    -- | A collection of name\/value pairs
    additionalParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Configuration for the rollout of OTA updates.
    awsJobExecutionsRolloutConfig :: Core.Maybe AwsJobExecutionsRolloutConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OTAUpdateInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otaUpdateStatus', 'oTAUpdateInfo_otaUpdateStatus' - The status of the OTA update.
--
-- 'lastModifiedDate', 'oTAUpdateInfo_lastModifiedDate' - The date when the OTA update was last updated.
--
-- 'targetSelection', 'oTAUpdateInfo_targetSelection' - Specifies whether the OTA update will continue to run (CONTINUOUS), or
-- will be complete after all those things specified as targets have
-- completed the OTA update (SNAPSHOT). If continuous, the OTA update may
-- also be run on a thing when a change is detected in a target. For
-- example, an OTA update will run on a thing when the thing is added to a
-- target group, even after the OTA update was completed by all things
-- originally in the group.
--
-- 'otaUpdateArn', 'oTAUpdateInfo_otaUpdateArn' - The OTA update ARN.
--
-- 'awsIotJobId', 'oTAUpdateInfo_awsIotJobId' - The AWS IoT job ID associated with the OTA update.
--
-- 'creationDate', 'oTAUpdateInfo_creationDate' - The date when the OTA update was created.
--
-- 'awsIotJobArn', 'oTAUpdateInfo_awsIotJobArn' - The AWS IoT job ARN associated with the OTA update.
--
-- 'protocols', 'oTAUpdateInfo_protocols' - The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
--
-- 'targets', 'oTAUpdateInfo_targets' - The targets of the OTA update.
--
-- 'awsJobPresignedUrlConfig', 'oTAUpdateInfo_awsJobPresignedUrlConfig' - Configuration information for pre-signed URLs. Valid when @protocols@
-- contains HTTP.
--
-- 'errorInfo', 'oTAUpdateInfo_errorInfo' - Error information associated with the OTA update.
--
-- 'description', 'oTAUpdateInfo_description' - A description of the OTA update.
--
-- 'otaUpdateFiles', 'oTAUpdateInfo_otaUpdateFiles' - A list of files associated with the OTA update.
--
-- 'otaUpdateId', 'oTAUpdateInfo_otaUpdateId' - The OTA update ID.
--
-- 'additionalParameters', 'oTAUpdateInfo_additionalParameters' - A collection of name\/value pairs
--
-- 'awsJobExecutionsRolloutConfig', 'oTAUpdateInfo_awsJobExecutionsRolloutConfig' - Configuration for the rollout of OTA updates.
newOTAUpdateInfo ::
  OTAUpdateInfo
newOTAUpdateInfo =
  OTAUpdateInfo'
    { otaUpdateStatus = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      targetSelection = Core.Nothing,
      otaUpdateArn = Core.Nothing,
      awsIotJobId = Core.Nothing,
      creationDate = Core.Nothing,
      awsIotJobArn = Core.Nothing,
      protocols = Core.Nothing,
      targets = Core.Nothing,
      awsJobPresignedUrlConfig = Core.Nothing,
      errorInfo = Core.Nothing,
      description = Core.Nothing,
      otaUpdateFiles = Core.Nothing,
      otaUpdateId = Core.Nothing,
      additionalParameters = Core.Nothing,
      awsJobExecutionsRolloutConfig = Core.Nothing
    }

-- | The status of the OTA update.
oTAUpdateInfo_otaUpdateStatus :: Lens.Lens' OTAUpdateInfo (Core.Maybe OTAUpdateStatus)
oTAUpdateInfo_otaUpdateStatus = Lens.lens (\OTAUpdateInfo' {otaUpdateStatus} -> otaUpdateStatus) (\s@OTAUpdateInfo' {} a -> s {otaUpdateStatus = a} :: OTAUpdateInfo)

-- | The date when the OTA update was last updated.
oTAUpdateInfo_lastModifiedDate :: Lens.Lens' OTAUpdateInfo (Core.Maybe Core.UTCTime)
oTAUpdateInfo_lastModifiedDate = Lens.lens (\OTAUpdateInfo' {lastModifiedDate} -> lastModifiedDate) (\s@OTAUpdateInfo' {} a -> s {lastModifiedDate = a} :: OTAUpdateInfo) Core.. Lens.mapping Core._Time

-- | Specifies whether the OTA update will continue to run (CONTINUOUS), or
-- will be complete after all those things specified as targets have
-- completed the OTA update (SNAPSHOT). If continuous, the OTA update may
-- also be run on a thing when a change is detected in a target. For
-- example, an OTA update will run on a thing when the thing is added to a
-- target group, even after the OTA update was completed by all things
-- originally in the group.
oTAUpdateInfo_targetSelection :: Lens.Lens' OTAUpdateInfo (Core.Maybe TargetSelection)
oTAUpdateInfo_targetSelection = Lens.lens (\OTAUpdateInfo' {targetSelection} -> targetSelection) (\s@OTAUpdateInfo' {} a -> s {targetSelection = a} :: OTAUpdateInfo)

-- | The OTA update ARN.
oTAUpdateInfo_otaUpdateArn :: Lens.Lens' OTAUpdateInfo (Core.Maybe Core.Text)
oTAUpdateInfo_otaUpdateArn = Lens.lens (\OTAUpdateInfo' {otaUpdateArn} -> otaUpdateArn) (\s@OTAUpdateInfo' {} a -> s {otaUpdateArn = a} :: OTAUpdateInfo)

-- | The AWS IoT job ID associated with the OTA update.
oTAUpdateInfo_awsIotJobId :: Lens.Lens' OTAUpdateInfo (Core.Maybe Core.Text)
oTAUpdateInfo_awsIotJobId = Lens.lens (\OTAUpdateInfo' {awsIotJobId} -> awsIotJobId) (\s@OTAUpdateInfo' {} a -> s {awsIotJobId = a} :: OTAUpdateInfo)

-- | The date when the OTA update was created.
oTAUpdateInfo_creationDate :: Lens.Lens' OTAUpdateInfo (Core.Maybe Core.UTCTime)
oTAUpdateInfo_creationDate = Lens.lens (\OTAUpdateInfo' {creationDate} -> creationDate) (\s@OTAUpdateInfo' {} a -> s {creationDate = a} :: OTAUpdateInfo) Core.. Lens.mapping Core._Time

-- | The AWS IoT job ARN associated with the OTA update.
oTAUpdateInfo_awsIotJobArn :: Lens.Lens' OTAUpdateInfo (Core.Maybe Core.Text)
oTAUpdateInfo_awsIotJobArn = Lens.lens (\OTAUpdateInfo' {awsIotJobArn} -> awsIotJobArn) (\s@OTAUpdateInfo' {} a -> s {awsIotJobArn = a} :: OTAUpdateInfo)

-- | The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
oTAUpdateInfo_protocols :: Lens.Lens' OTAUpdateInfo (Core.Maybe (Core.NonEmpty Protocol))
oTAUpdateInfo_protocols = Lens.lens (\OTAUpdateInfo' {protocols} -> protocols) (\s@OTAUpdateInfo' {} a -> s {protocols = a} :: OTAUpdateInfo) Core.. Lens.mapping Lens._Coerce

-- | The targets of the OTA update.
oTAUpdateInfo_targets :: Lens.Lens' OTAUpdateInfo (Core.Maybe (Core.NonEmpty Core.Text))
oTAUpdateInfo_targets = Lens.lens (\OTAUpdateInfo' {targets} -> targets) (\s@OTAUpdateInfo' {} a -> s {targets = a} :: OTAUpdateInfo) Core.. Lens.mapping Lens._Coerce

-- | Configuration information for pre-signed URLs. Valid when @protocols@
-- contains HTTP.
oTAUpdateInfo_awsJobPresignedUrlConfig :: Lens.Lens' OTAUpdateInfo (Core.Maybe AwsJobPresignedUrlConfig)
oTAUpdateInfo_awsJobPresignedUrlConfig = Lens.lens (\OTAUpdateInfo' {awsJobPresignedUrlConfig} -> awsJobPresignedUrlConfig) (\s@OTAUpdateInfo' {} a -> s {awsJobPresignedUrlConfig = a} :: OTAUpdateInfo)

-- | Error information associated with the OTA update.
oTAUpdateInfo_errorInfo :: Lens.Lens' OTAUpdateInfo (Core.Maybe ErrorInfo)
oTAUpdateInfo_errorInfo = Lens.lens (\OTAUpdateInfo' {errorInfo} -> errorInfo) (\s@OTAUpdateInfo' {} a -> s {errorInfo = a} :: OTAUpdateInfo)

-- | A description of the OTA update.
oTAUpdateInfo_description :: Lens.Lens' OTAUpdateInfo (Core.Maybe Core.Text)
oTAUpdateInfo_description = Lens.lens (\OTAUpdateInfo' {description} -> description) (\s@OTAUpdateInfo' {} a -> s {description = a} :: OTAUpdateInfo)

-- | A list of files associated with the OTA update.
oTAUpdateInfo_otaUpdateFiles :: Lens.Lens' OTAUpdateInfo (Core.Maybe (Core.NonEmpty OTAUpdateFile))
oTAUpdateInfo_otaUpdateFiles = Lens.lens (\OTAUpdateInfo' {otaUpdateFiles} -> otaUpdateFiles) (\s@OTAUpdateInfo' {} a -> s {otaUpdateFiles = a} :: OTAUpdateInfo) Core.. Lens.mapping Lens._Coerce

-- | The OTA update ID.
oTAUpdateInfo_otaUpdateId :: Lens.Lens' OTAUpdateInfo (Core.Maybe Core.Text)
oTAUpdateInfo_otaUpdateId = Lens.lens (\OTAUpdateInfo' {otaUpdateId} -> otaUpdateId) (\s@OTAUpdateInfo' {} a -> s {otaUpdateId = a} :: OTAUpdateInfo)

-- | A collection of name\/value pairs
oTAUpdateInfo_additionalParameters :: Lens.Lens' OTAUpdateInfo (Core.Maybe (Core.HashMap Core.Text Core.Text))
oTAUpdateInfo_additionalParameters = Lens.lens (\OTAUpdateInfo' {additionalParameters} -> additionalParameters) (\s@OTAUpdateInfo' {} a -> s {additionalParameters = a} :: OTAUpdateInfo) Core.. Lens.mapping Lens._Coerce

-- | Configuration for the rollout of OTA updates.
oTAUpdateInfo_awsJobExecutionsRolloutConfig :: Lens.Lens' OTAUpdateInfo (Core.Maybe AwsJobExecutionsRolloutConfig)
oTAUpdateInfo_awsJobExecutionsRolloutConfig = Lens.lens (\OTAUpdateInfo' {awsJobExecutionsRolloutConfig} -> awsJobExecutionsRolloutConfig) (\s@OTAUpdateInfo' {} a -> s {awsJobExecutionsRolloutConfig = a} :: OTAUpdateInfo)

instance Core.FromJSON OTAUpdateInfo where
  parseJSON =
    Core.withObject
      "OTAUpdateInfo"
      ( \x ->
          OTAUpdateInfo'
            Core.<$> (x Core..:? "otaUpdateStatus")
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "targetSelection")
            Core.<*> (x Core..:? "otaUpdateArn")
            Core.<*> (x Core..:? "awsIotJobId")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "awsIotJobArn")
            Core.<*> (x Core..:? "protocols")
            Core.<*> (x Core..:? "targets")
            Core.<*> (x Core..:? "awsJobPresignedUrlConfig")
            Core.<*> (x Core..:? "errorInfo")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "otaUpdateFiles")
            Core.<*> (x Core..:? "otaUpdateId")
            Core.<*> ( x Core..:? "additionalParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "awsJobExecutionsRolloutConfig")
      )

instance Core.Hashable OTAUpdateInfo

instance Core.NFData OTAUpdateInfo
