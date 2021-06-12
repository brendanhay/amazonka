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
-- Module      : Network.AWS.Config.Types.ConfigExportDeliveryInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigExportDeliveryInfo where

import Network.AWS.Config.Types.DeliveryStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides status of the delivery of the snapshot or the configuration
-- history to the specified Amazon S3 bucket. Also provides the status of
-- notifications about the Amazon S3 delivery to the specified Amazon SNS
-- topic.
--
-- /See:/ 'newConfigExportDeliveryInfo' smart constructor.
data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo'
  { -- | The error message from the last attempted delivery.
    lastErrorMessage :: Core.Maybe Core.Text,
    -- | The time that the next delivery occurs.
    nextDeliveryTime :: Core.Maybe Core.POSIX,
    -- | The time of the last successful delivery.
    lastSuccessfulTime :: Core.Maybe Core.POSIX,
    -- | The error code from the last attempted delivery.
    lastErrorCode :: Core.Maybe Core.Text,
    -- | Status of the last attempted delivery.
    lastStatus :: Core.Maybe DeliveryStatus,
    -- | The time of the last attempted delivery.
    lastAttemptTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigExportDeliveryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastErrorMessage', 'configExportDeliveryInfo_lastErrorMessage' - The error message from the last attempted delivery.
--
-- 'nextDeliveryTime', 'configExportDeliveryInfo_nextDeliveryTime' - The time that the next delivery occurs.
--
-- 'lastSuccessfulTime', 'configExportDeliveryInfo_lastSuccessfulTime' - The time of the last successful delivery.
--
-- 'lastErrorCode', 'configExportDeliveryInfo_lastErrorCode' - The error code from the last attempted delivery.
--
-- 'lastStatus', 'configExportDeliveryInfo_lastStatus' - Status of the last attempted delivery.
--
-- 'lastAttemptTime', 'configExportDeliveryInfo_lastAttemptTime' - The time of the last attempted delivery.
newConfigExportDeliveryInfo ::
  ConfigExportDeliveryInfo
newConfigExportDeliveryInfo =
  ConfigExportDeliveryInfo'
    { lastErrorMessage =
        Core.Nothing,
      nextDeliveryTime = Core.Nothing,
      lastSuccessfulTime = Core.Nothing,
      lastErrorCode = Core.Nothing,
      lastStatus = Core.Nothing,
      lastAttemptTime = Core.Nothing
    }

-- | The error message from the last attempted delivery.
configExportDeliveryInfo_lastErrorMessage :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Core.Text)
configExportDeliveryInfo_lastErrorMessage = Lens.lens (\ConfigExportDeliveryInfo' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigExportDeliveryInfo' {} a -> s {lastErrorMessage = a} :: ConfigExportDeliveryInfo)

-- | The time that the next delivery occurs.
configExportDeliveryInfo_nextDeliveryTime :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Core.UTCTime)
configExportDeliveryInfo_nextDeliveryTime = Lens.lens (\ConfigExportDeliveryInfo' {nextDeliveryTime} -> nextDeliveryTime) (\s@ConfigExportDeliveryInfo' {} a -> s {nextDeliveryTime = a} :: ConfigExportDeliveryInfo) Core.. Lens.mapping Core._Time

-- | The time of the last successful delivery.
configExportDeliveryInfo_lastSuccessfulTime :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Core.UTCTime)
configExportDeliveryInfo_lastSuccessfulTime = Lens.lens (\ConfigExportDeliveryInfo' {lastSuccessfulTime} -> lastSuccessfulTime) (\s@ConfigExportDeliveryInfo' {} a -> s {lastSuccessfulTime = a} :: ConfigExportDeliveryInfo) Core.. Lens.mapping Core._Time

-- | The error code from the last attempted delivery.
configExportDeliveryInfo_lastErrorCode :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Core.Text)
configExportDeliveryInfo_lastErrorCode = Lens.lens (\ConfigExportDeliveryInfo' {lastErrorCode} -> lastErrorCode) (\s@ConfigExportDeliveryInfo' {} a -> s {lastErrorCode = a} :: ConfigExportDeliveryInfo)

-- | Status of the last attempted delivery.
configExportDeliveryInfo_lastStatus :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe DeliveryStatus)
configExportDeliveryInfo_lastStatus = Lens.lens (\ConfigExportDeliveryInfo' {lastStatus} -> lastStatus) (\s@ConfigExportDeliveryInfo' {} a -> s {lastStatus = a} :: ConfigExportDeliveryInfo)

-- | The time of the last attempted delivery.
configExportDeliveryInfo_lastAttemptTime :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Core.UTCTime)
configExportDeliveryInfo_lastAttemptTime = Lens.lens (\ConfigExportDeliveryInfo' {lastAttemptTime} -> lastAttemptTime) (\s@ConfigExportDeliveryInfo' {} a -> s {lastAttemptTime = a} :: ConfigExportDeliveryInfo) Core.. Lens.mapping Core._Time

instance Core.FromJSON ConfigExportDeliveryInfo where
  parseJSON =
    Core.withObject
      "ConfigExportDeliveryInfo"
      ( \x ->
          ConfigExportDeliveryInfo'
            Core.<$> (x Core..:? "lastErrorMessage")
            Core.<*> (x Core..:? "nextDeliveryTime")
            Core.<*> (x Core..:? "lastSuccessfulTime")
            Core.<*> (x Core..:? "lastErrorCode")
            Core.<*> (x Core..:? "lastStatus")
            Core.<*> (x Core..:? "lastAttemptTime")
      )

instance Core.Hashable ConfigExportDeliveryInfo

instance Core.NFData ConfigExportDeliveryInfo
