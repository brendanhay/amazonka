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
-- Module      : Amazonka.Config.Types.ConfigExportDeliveryInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigExportDeliveryInfo where

import Amazonka.Config.Types.DeliveryStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides status of the delivery of the snapshot or the configuration
-- history to the specified Amazon S3 bucket. Also provides the status of
-- notifications about the Amazon S3 delivery to the specified Amazon SNS
-- topic.
--
-- /See:/ 'newConfigExportDeliveryInfo' smart constructor.
data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo'
  { -- | The time of the last attempted delivery.
    lastAttemptTime :: Prelude.Maybe Data.POSIX,
    -- | The error code from the last attempted delivery.
    lastErrorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message from the last attempted delivery.
    lastErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | Status of the last attempted delivery.
    lastStatus :: Prelude.Maybe DeliveryStatus,
    -- | The time of the last successful delivery.
    lastSuccessfulTime :: Prelude.Maybe Data.POSIX,
    -- | The time that the next delivery occurs.
    nextDeliveryTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigExportDeliveryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastAttemptTime', 'configExportDeliveryInfo_lastAttemptTime' - The time of the last attempted delivery.
--
-- 'lastErrorCode', 'configExportDeliveryInfo_lastErrorCode' - The error code from the last attempted delivery.
--
-- 'lastErrorMessage', 'configExportDeliveryInfo_lastErrorMessage' - The error message from the last attempted delivery.
--
-- 'lastStatus', 'configExportDeliveryInfo_lastStatus' - Status of the last attempted delivery.
--
-- 'lastSuccessfulTime', 'configExportDeliveryInfo_lastSuccessfulTime' - The time of the last successful delivery.
--
-- 'nextDeliveryTime', 'configExportDeliveryInfo_nextDeliveryTime' - The time that the next delivery occurs.
newConfigExportDeliveryInfo ::
  ConfigExportDeliveryInfo
newConfigExportDeliveryInfo =
  ConfigExportDeliveryInfo'
    { lastAttemptTime =
        Prelude.Nothing,
      lastErrorCode = Prelude.Nothing,
      lastErrorMessage = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      lastSuccessfulTime = Prelude.Nothing,
      nextDeliveryTime = Prelude.Nothing
    }

-- | The time of the last attempted delivery.
configExportDeliveryInfo_lastAttemptTime :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.UTCTime)
configExportDeliveryInfo_lastAttemptTime = Lens.lens (\ConfigExportDeliveryInfo' {lastAttemptTime} -> lastAttemptTime) (\s@ConfigExportDeliveryInfo' {} a -> s {lastAttemptTime = a} :: ConfigExportDeliveryInfo) Prelude.. Lens.mapping Data._Time

-- | The error code from the last attempted delivery.
configExportDeliveryInfo_lastErrorCode :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.Text)
configExportDeliveryInfo_lastErrorCode = Lens.lens (\ConfigExportDeliveryInfo' {lastErrorCode} -> lastErrorCode) (\s@ConfigExportDeliveryInfo' {} a -> s {lastErrorCode = a} :: ConfigExportDeliveryInfo)

-- | The error message from the last attempted delivery.
configExportDeliveryInfo_lastErrorMessage :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.Text)
configExportDeliveryInfo_lastErrorMessage = Lens.lens (\ConfigExportDeliveryInfo' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigExportDeliveryInfo' {} a -> s {lastErrorMessage = a} :: ConfigExportDeliveryInfo)

-- | Status of the last attempted delivery.
configExportDeliveryInfo_lastStatus :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe DeliveryStatus)
configExportDeliveryInfo_lastStatus = Lens.lens (\ConfigExportDeliveryInfo' {lastStatus} -> lastStatus) (\s@ConfigExportDeliveryInfo' {} a -> s {lastStatus = a} :: ConfigExportDeliveryInfo)

-- | The time of the last successful delivery.
configExportDeliveryInfo_lastSuccessfulTime :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.UTCTime)
configExportDeliveryInfo_lastSuccessfulTime = Lens.lens (\ConfigExportDeliveryInfo' {lastSuccessfulTime} -> lastSuccessfulTime) (\s@ConfigExportDeliveryInfo' {} a -> s {lastSuccessfulTime = a} :: ConfigExportDeliveryInfo) Prelude.. Lens.mapping Data._Time

-- | The time that the next delivery occurs.
configExportDeliveryInfo_nextDeliveryTime :: Lens.Lens' ConfigExportDeliveryInfo (Prelude.Maybe Prelude.UTCTime)
configExportDeliveryInfo_nextDeliveryTime = Lens.lens (\ConfigExportDeliveryInfo' {nextDeliveryTime} -> nextDeliveryTime) (\s@ConfigExportDeliveryInfo' {} a -> s {nextDeliveryTime = a} :: ConfigExportDeliveryInfo) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ConfigExportDeliveryInfo where
  parseJSON =
    Data.withObject
      "ConfigExportDeliveryInfo"
      ( \x ->
          ConfigExportDeliveryInfo'
            Prelude.<$> (x Data..:? "lastAttemptTime")
            Prelude.<*> (x Data..:? "lastErrorCode")
            Prelude.<*> (x Data..:? "lastErrorMessage")
            Prelude.<*> (x Data..:? "lastStatus")
            Prelude.<*> (x Data..:? "lastSuccessfulTime")
            Prelude.<*> (x Data..:? "nextDeliveryTime")
      )

instance Prelude.Hashable ConfigExportDeliveryInfo where
  hashWithSalt _salt ConfigExportDeliveryInfo' {..} =
    _salt
      `Prelude.hashWithSalt` lastAttemptTime
      `Prelude.hashWithSalt` lastErrorCode
      `Prelude.hashWithSalt` lastErrorMessage
      `Prelude.hashWithSalt` lastStatus
      `Prelude.hashWithSalt` lastSuccessfulTime
      `Prelude.hashWithSalt` nextDeliveryTime

instance Prelude.NFData ConfigExportDeliveryInfo where
  rnf ConfigExportDeliveryInfo' {..} =
    Prelude.rnf lastAttemptTime `Prelude.seq`
      Prelude.rnf lastErrorCode `Prelude.seq`
        Prelude.rnf lastErrorMessage `Prelude.seq`
          Prelude.rnf lastStatus `Prelude.seq`
            Prelude.rnf lastSuccessfulTime `Prelude.seq`
              Prelude.rnf nextDeliveryTime
