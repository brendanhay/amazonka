{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types.AwsJobExecutionsRolloutConfig
import Network.AWS.IoT.Types.AwsJobPresignedUrlConfig
import Network.AWS.IoT.Types.ErrorInfo
import Network.AWS.IoT.Types.OTAUpdateFile
import Network.AWS.IoT.Types.OTAUpdateStatus
import Network.AWS.IoT.Types.Protocol
import Network.AWS.IoT.Types.TargetSelection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an OTA update.
--
-- /See:/ 'newOTAUpdateInfo' smart constructor.
data OTAUpdateInfo = OTAUpdateInfo'
  { -- | The status of the OTA update.
    otaUpdateStatus :: Prelude.Maybe OTAUpdateStatus,
    -- | The date when the OTA update was last updated.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies whether the OTA update will continue to run (CONTINUOUS), or
    -- will be complete after all those things specified as targets have
    -- completed the OTA update (SNAPSHOT). If continuous, the OTA update may
    -- also be run on a thing when a change is detected in a target. For
    -- example, an OTA update will run on a thing when the thing is added to a
    -- target group, even after the OTA update was completed by all things
    -- originally in the group.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | The OTA update ARN.
    otaUpdateArn :: Prelude.Maybe Prelude.Text,
    -- | The AWS IoT job ID associated with the OTA update.
    awsIotJobId :: Prelude.Maybe Prelude.Text,
    -- | The date when the OTA update was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS IoT job ARN associated with the OTA update.
    awsIotJobArn :: Prelude.Maybe Prelude.Text,
    -- | The protocol used to transfer the OTA update image. Valid values are
    -- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
    -- target device can choose the protocol.
    protocols :: Prelude.Maybe (Prelude.NonEmpty Protocol),
    -- | The targets of the OTA update.
    targets :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Configuration information for pre-signed URLs. Valid when @protocols@
    -- contains HTTP.
    awsJobPresignedUrlConfig :: Prelude.Maybe AwsJobPresignedUrlConfig,
    -- | Error information associated with the OTA update.
    errorInfo :: Prelude.Maybe ErrorInfo,
    -- | A description of the OTA update.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of files associated with the OTA update.
    otaUpdateFiles :: Prelude.Maybe (Prelude.NonEmpty OTAUpdateFile),
    -- | The OTA update ID.
    otaUpdateId :: Prelude.Maybe Prelude.Text,
    -- | A collection of name\/value pairs
    additionalParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration for the rollout of OTA updates.
    awsJobExecutionsRolloutConfig :: Prelude.Maybe AwsJobExecutionsRolloutConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { otaUpdateStatus = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      otaUpdateArn = Prelude.Nothing,
      awsIotJobId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      awsIotJobArn = Prelude.Nothing,
      protocols = Prelude.Nothing,
      targets = Prelude.Nothing,
      awsJobPresignedUrlConfig = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      description = Prelude.Nothing,
      otaUpdateFiles = Prelude.Nothing,
      otaUpdateId = Prelude.Nothing,
      additionalParameters = Prelude.Nothing,
      awsJobExecutionsRolloutConfig = Prelude.Nothing
    }

-- | The status of the OTA update.
oTAUpdateInfo_otaUpdateStatus :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe OTAUpdateStatus)
oTAUpdateInfo_otaUpdateStatus = Lens.lens (\OTAUpdateInfo' {otaUpdateStatus} -> otaUpdateStatus) (\s@OTAUpdateInfo' {} a -> s {otaUpdateStatus = a} :: OTAUpdateInfo)

-- | The date when the OTA update was last updated.
oTAUpdateInfo_lastModifiedDate :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.UTCTime)
oTAUpdateInfo_lastModifiedDate = Lens.lens (\OTAUpdateInfo' {lastModifiedDate} -> lastModifiedDate) (\s@OTAUpdateInfo' {} a -> s {lastModifiedDate = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Prelude._Time

-- | Specifies whether the OTA update will continue to run (CONTINUOUS), or
-- will be complete after all those things specified as targets have
-- completed the OTA update (SNAPSHOT). If continuous, the OTA update may
-- also be run on a thing when a change is detected in a target. For
-- example, an OTA update will run on a thing when the thing is added to a
-- target group, even after the OTA update was completed by all things
-- originally in the group.
oTAUpdateInfo_targetSelection :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe TargetSelection)
oTAUpdateInfo_targetSelection = Lens.lens (\OTAUpdateInfo' {targetSelection} -> targetSelection) (\s@OTAUpdateInfo' {} a -> s {targetSelection = a} :: OTAUpdateInfo)

-- | The OTA update ARN.
oTAUpdateInfo_otaUpdateArn :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_otaUpdateArn = Lens.lens (\OTAUpdateInfo' {otaUpdateArn} -> otaUpdateArn) (\s@OTAUpdateInfo' {} a -> s {otaUpdateArn = a} :: OTAUpdateInfo)

-- | The AWS IoT job ID associated with the OTA update.
oTAUpdateInfo_awsIotJobId :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_awsIotJobId = Lens.lens (\OTAUpdateInfo' {awsIotJobId} -> awsIotJobId) (\s@OTAUpdateInfo' {} a -> s {awsIotJobId = a} :: OTAUpdateInfo)

-- | The date when the OTA update was created.
oTAUpdateInfo_creationDate :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.UTCTime)
oTAUpdateInfo_creationDate = Lens.lens (\OTAUpdateInfo' {creationDate} -> creationDate) (\s@OTAUpdateInfo' {} a -> s {creationDate = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Prelude._Time

-- | The AWS IoT job ARN associated with the OTA update.
oTAUpdateInfo_awsIotJobArn :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_awsIotJobArn = Lens.lens (\OTAUpdateInfo' {awsIotJobArn} -> awsIotJobArn) (\s@OTAUpdateInfo' {} a -> s {awsIotJobArn = a} :: OTAUpdateInfo)

-- | The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
oTAUpdateInfo_protocols :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe (Prelude.NonEmpty Protocol))
oTAUpdateInfo_protocols = Lens.lens (\OTAUpdateInfo' {protocols} -> protocols) (\s@OTAUpdateInfo' {} a -> s {protocols = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The targets of the OTA update.
oTAUpdateInfo_targets :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
oTAUpdateInfo_targets = Lens.lens (\OTAUpdateInfo' {targets} -> targets) (\s@OTAUpdateInfo' {} a -> s {targets = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Configuration information for pre-signed URLs. Valid when @protocols@
-- contains HTTP.
oTAUpdateInfo_awsJobPresignedUrlConfig :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe AwsJobPresignedUrlConfig)
oTAUpdateInfo_awsJobPresignedUrlConfig = Lens.lens (\OTAUpdateInfo' {awsJobPresignedUrlConfig} -> awsJobPresignedUrlConfig) (\s@OTAUpdateInfo' {} a -> s {awsJobPresignedUrlConfig = a} :: OTAUpdateInfo)

-- | Error information associated with the OTA update.
oTAUpdateInfo_errorInfo :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe ErrorInfo)
oTAUpdateInfo_errorInfo = Lens.lens (\OTAUpdateInfo' {errorInfo} -> errorInfo) (\s@OTAUpdateInfo' {} a -> s {errorInfo = a} :: OTAUpdateInfo)

-- | A description of the OTA update.
oTAUpdateInfo_description :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_description = Lens.lens (\OTAUpdateInfo' {description} -> description) (\s@OTAUpdateInfo' {} a -> s {description = a} :: OTAUpdateInfo)

-- | A list of files associated with the OTA update.
oTAUpdateInfo_otaUpdateFiles :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe (Prelude.NonEmpty OTAUpdateFile))
oTAUpdateInfo_otaUpdateFiles = Lens.lens (\OTAUpdateInfo' {otaUpdateFiles} -> otaUpdateFiles) (\s@OTAUpdateInfo' {} a -> s {otaUpdateFiles = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The OTA update ID.
oTAUpdateInfo_otaUpdateId :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_otaUpdateId = Lens.lens (\OTAUpdateInfo' {otaUpdateId} -> otaUpdateId) (\s@OTAUpdateInfo' {} a -> s {otaUpdateId = a} :: OTAUpdateInfo)

-- | A collection of name\/value pairs
oTAUpdateInfo_additionalParameters :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
oTAUpdateInfo_additionalParameters = Lens.lens (\OTAUpdateInfo' {additionalParameters} -> additionalParameters) (\s@OTAUpdateInfo' {} a -> s {additionalParameters = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Configuration for the rollout of OTA updates.
oTAUpdateInfo_awsJobExecutionsRolloutConfig :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe AwsJobExecutionsRolloutConfig)
oTAUpdateInfo_awsJobExecutionsRolloutConfig = Lens.lens (\OTAUpdateInfo' {awsJobExecutionsRolloutConfig} -> awsJobExecutionsRolloutConfig) (\s@OTAUpdateInfo' {} a -> s {awsJobExecutionsRolloutConfig = a} :: OTAUpdateInfo)

instance Prelude.FromJSON OTAUpdateInfo where
  parseJSON =
    Prelude.withObject
      "OTAUpdateInfo"
      ( \x ->
          OTAUpdateInfo'
            Prelude.<$> (x Prelude..:? "otaUpdateStatus")
            Prelude.<*> (x Prelude..:? "lastModifiedDate")
            Prelude.<*> (x Prelude..:? "targetSelection")
            Prelude.<*> (x Prelude..:? "otaUpdateArn")
            Prelude.<*> (x Prelude..:? "awsIotJobId")
            Prelude.<*> (x Prelude..:? "creationDate")
            Prelude.<*> (x Prelude..:? "awsIotJobArn")
            Prelude.<*> (x Prelude..:? "protocols")
            Prelude.<*> (x Prelude..:? "targets")
            Prelude.<*> (x Prelude..:? "awsJobPresignedUrlConfig")
            Prelude.<*> (x Prelude..:? "errorInfo")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "otaUpdateFiles")
            Prelude.<*> (x Prelude..:? "otaUpdateId")
            Prelude.<*> ( x Prelude..:? "additionalParameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "awsJobExecutionsRolloutConfig")
      )

instance Prelude.Hashable OTAUpdateInfo

instance Prelude.NFData OTAUpdateInfo
