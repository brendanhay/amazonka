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
-- Module      : Network.AWS.Config.Types.ConfigExportDeliveryInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigExportDeliveryInfo where

import Network.AWS.Config.Types.DeliveryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides status of the delivery of the snapshot or the configuration
-- history to the specified Amazon S3 bucket. Also provides the status of
-- notifications about the Amazon S3 delivery to the specified Amazon SNS
-- topic.
--
-- /See:/ 'newConfigExportDeliveryInfo' smart constructor.
data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo'
  { -- | The error message from the last attempted delivery.
    lastErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | The time that the next delivery occurs.
    nextDeliveryTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time of the last successful delivery.
    lastSuccessfulTime :: Prelude.Maybe Prelude.POSIX,
    -- | The error code from the last attempted delivery.
    lastErrorCode :: Prelude.Maybe Prelude.Text,
    -- | Status of the last attempted delivery.
    lastStatus :: Prelude.Maybe DeliveryStatus,
    -- | The time of the last attempted delivery.
    lastAttemptTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      nextDeliveryTime = Prelude.Nothing,
      lastSuccessfulTime = Prelude.Nothing,
      lastErrorCode = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      lastAttemptTime = Prelude.Nothing
    }

-- | The error message from the last attempted delivery.
configExportDeliveryInfo_lastErrorMessage :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.Text)
configExportDeliveryInfo_lastErrorMessage = Lens.lens (\ConfigExportDeliveryInfo' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigExportDeliveryInfo' {} a -> s {lastErrorMessage = a} :: ConfigExportDeliveryInfo)

-- | The time that the next delivery occurs.
configExportDeliveryInfo_nextDeliveryTime :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.UTCTime)
configExportDeliveryInfo_nextDeliveryTime = Lens.lens (\ConfigExportDeliveryInfo' {nextDeliveryTime} -> nextDeliveryTime) (\s@ConfigExportDeliveryInfo' {} a -> s {nextDeliveryTime = a} :: ConfigExportDeliveryInfo) Prelude.. Lens.mapping Prelude._Time

-- | The time of the last successful delivery.
configExportDeliveryInfo_lastSuccessfulTime :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.UTCTime)
configExportDeliveryInfo_lastSuccessfulTime = Lens.lens (\ConfigExportDeliveryInfo' {lastSuccessfulTime} -> lastSuccessfulTime) (\s@ConfigExportDeliveryInfo' {} a -> s {lastSuccessfulTime = a} :: ConfigExportDeliveryInfo) Prelude.. Lens.mapping Prelude._Time

-- | The error code from the last attempted delivery.
configExportDeliveryInfo_lastErrorCode :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.Text)
configExportDeliveryInfo_lastErrorCode = Lens.lens (\ConfigExportDeliveryInfo' {lastErrorCode} -> lastErrorCode) (\s@ConfigExportDeliveryInfo' {} a -> s {lastErrorCode = a} :: ConfigExportDeliveryInfo)

-- | Status of the last attempted delivery.
configExportDeliveryInfo_lastStatus :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe DeliveryStatus)
configExportDeliveryInfo_lastStatus = Lens.lens (\ConfigExportDeliveryInfo' {lastStatus} -> lastStatus) (\s@ConfigExportDeliveryInfo' {} a -> s {lastStatus = a} :: ConfigExportDeliveryInfo)

-- | The time of the last attempted delivery.
configExportDeliveryInfo_lastAttemptTime :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.UTCTime)
configExportDeliveryInfo_lastAttemptTime = Lens.lens (\ConfigExportDeliveryInfo' {lastAttemptTime} -> lastAttemptTime) (\s@ConfigExportDeliveryInfo' {} a -> s {lastAttemptTime = a} :: ConfigExportDeliveryInfo) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ConfigExportDeliveryInfo where
  parseJSON =
    Prelude.withObject
      "ConfigExportDeliveryInfo"
      ( \x ->
          ConfigExportDeliveryInfo'
            Prelude.<$> (x Prelude..:? "lastErrorMessage")
            Prelude.<*> (x Prelude..:? "nextDeliveryTime")
            Prelude.<*> (x Prelude..:? "lastSuccessfulTime")
            Prelude.<*> (x Prelude..:? "lastErrorCode")
            Prelude.<*> (x Prelude..:? "lastStatus")
            Prelude.<*> (x Prelude..:? "lastAttemptTime")
      )

instance Prelude.Hashable ConfigExportDeliveryInfo

instance Prelude.NFData ConfigExportDeliveryInfo
