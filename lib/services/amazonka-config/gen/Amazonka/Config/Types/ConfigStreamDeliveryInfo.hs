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
-- Module      : Amazonka.Config.Types.ConfigStreamDeliveryInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigStreamDeliveryInfo where

import Amazonka.Config.Types.DeliveryStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list that contains the status of the delivery of the configuration
-- stream notification to the Amazon SNS topic.
--
-- /See:/ 'newConfigStreamDeliveryInfo' smart constructor.
data ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo'
  { -- | The error code from the last attempted delivery.
    lastErrorCode :: Prelude.Maybe Prelude.Text,
    -- | Status of the last attempted delivery.
    --
    -- __Note__ Providing an SNS topic on a
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel>
    -- for Config is optional. If the SNS delivery is turned off, the last
    -- status will be __Not_Applicable__.
    lastStatus :: Prelude.Maybe DeliveryStatus,
    -- | The time from the last status change.
    lastStatusChangeTime :: Prelude.Maybe Core.POSIX,
    -- | The error message from the last attempted delivery.
    lastErrorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigStreamDeliveryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastErrorCode', 'configStreamDeliveryInfo_lastErrorCode' - The error code from the last attempted delivery.
--
-- 'lastStatus', 'configStreamDeliveryInfo_lastStatus' - Status of the last attempted delivery.
--
-- __Note__ Providing an SNS topic on a
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel>
-- for Config is optional. If the SNS delivery is turned off, the last
-- status will be __Not_Applicable__.
--
-- 'lastStatusChangeTime', 'configStreamDeliveryInfo_lastStatusChangeTime' - The time from the last status change.
--
-- 'lastErrorMessage', 'configStreamDeliveryInfo_lastErrorMessage' - The error message from the last attempted delivery.
newConfigStreamDeliveryInfo ::
  ConfigStreamDeliveryInfo
newConfigStreamDeliveryInfo =
  ConfigStreamDeliveryInfo'
    { lastErrorCode =
        Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      lastStatusChangeTime = Prelude.Nothing,
      lastErrorMessage = Prelude.Nothing
    }

-- | The error code from the last attempted delivery.
configStreamDeliveryInfo_lastErrorCode :: Lens.Lens' ConfigStreamDeliveryInfo (Prelude.Maybe Prelude.Text)
configStreamDeliveryInfo_lastErrorCode = Lens.lens (\ConfigStreamDeliveryInfo' {lastErrorCode} -> lastErrorCode) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastErrorCode = a} :: ConfigStreamDeliveryInfo)

-- | Status of the last attempted delivery.
--
-- __Note__ Providing an SNS topic on a
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel>
-- for Config is optional. If the SNS delivery is turned off, the last
-- status will be __Not_Applicable__.
configStreamDeliveryInfo_lastStatus :: Lens.Lens' ConfigStreamDeliveryInfo (Prelude.Maybe DeliveryStatus)
configStreamDeliveryInfo_lastStatus = Lens.lens (\ConfigStreamDeliveryInfo' {lastStatus} -> lastStatus) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastStatus = a} :: ConfigStreamDeliveryInfo)

-- | The time from the last status change.
configStreamDeliveryInfo_lastStatusChangeTime :: Lens.Lens' ConfigStreamDeliveryInfo (Prelude.Maybe Prelude.UTCTime)
configStreamDeliveryInfo_lastStatusChangeTime = Lens.lens (\ConfigStreamDeliveryInfo' {lastStatusChangeTime} -> lastStatusChangeTime) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastStatusChangeTime = a} :: ConfigStreamDeliveryInfo) Prelude.. Lens.mapping Core._Time

-- | The error message from the last attempted delivery.
configStreamDeliveryInfo_lastErrorMessage :: Lens.Lens' ConfigStreamDeliveryInfo (Prelude.Maybe Prelude.Text)
configStreamDeliveryInfo_lastErrorMessage = Lens.lens (\ConfigStreamDeliveryInfo' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastErrorMessage = a} :: ConfigStreamDeliveryInfo)

instance Core.FromJSON ConfigStreamDeliveryInfo where
  parseJSON =
    Core.withObject
      "ConfigStreamDeliveryInfo"
      ( \x ->
          ConfigStreamDeliveryInfo'
            Prelude.<$> (x Core..:? "lastErrorCode")
            Prelude.<*> (x Core..:? "lastStatus")
            Prelude.<*> (x Core..:? "lastStatusChangeTime")
            Prelude.<*> (x Core..:? "lastErrorMessage")
      )

instance Prelude.Hashable ConfigStreamDeliveryInfo where
  hashWithSalt _salt ConfigStreamDeliveryInfo' {..} =
    _salt `Prelude.hashWithSalt` lastErrorCode
      `Prelude.hashWithSalt` lastStatus
      `Prelude.hashWithSalt` lastStatusChangeTime
      `Prelude.hashWithSalt` lastErrorMessage

instance Prelude.NFData ConfigStreamDeliveryInfo where
  rnf ConfigStreamDeliveryInfo' {..} =
    Prelude.rnf lastErrorCode
      `Prelude.seq` Prelude.rnf lastStatus
      `Prelude.seq` Prelude.rnf lastStatusChangeTime
      `Prelude.seq` Prelude.rnf lastErrorMessage
