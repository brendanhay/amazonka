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
-- Module      : Network.AWS.Config.Types.ConfigStreamDeliveryInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigStreamDeliveryInfo where

import Network.AWS.Config.Types.DeliveryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list that contains the status of the delivery of the configuration
-- stream notification to the Amazon SNS topic.
--
-- /See:/ 'newConfigStreamDeliveryInfo' smart constructor.
data ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo'
  { -- | The error message from the last attempted delivery.
    lastErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code from the last attempted delivery.
    lastErrorCode :: Prelude.Maybe Prelude.Text,
    -- | Status of the last attempted delivery.
    --
    -- __Note__ Providing an SNS topic on a
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel>
    -- for AWS Config is optional. If the SNS delivery is turned off, the last
    -- status will be __Not_Applicable__.
    lastStatus :: Prelude.Maybe DeliveryStatus,
    -- | The time from the last status change.
    lastStatusChangeTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfigStreamDeliveryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastErrorMessage', 'configStreamDeliveryInfo_lastErrorMessage' - The error message from the last attempted delivery.
--
-- 'lastErrorCode', 'configStreamDeliveryInfo_lastErrorCode' - The error code from the last attempted delivery.
--
-- 'lastStatus', 'configStreamDeliveryInfo_lastStatus' - Status of the last attempted delivery.
--
-- __Note__ Providing an SNS topic on a
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel>
-- for AWS Config is optional. If the SNS delivery is turned off, the last
-- status will be __Not_Applicable__.
--
-- 'lastStatusChangeTime', 'configStreamDeliveryInfo_lastStatusChangeTime' - The time from the last status change.
newConfigStreamDeliveryInfo ::
  ConfigStreamDeliveryInfo
newConfigStreamDeliveryInfo =
  ConfigStreamDeliveryInfo'
    { lastErrorMessage =
        Prelude.Nothing,
      lastErrorCode = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      lastStatusChangeTime = Prelude.Nothing
    }

-- | The error message from the last attempted delivery.
configStreamDeliveryInfo_lastErrorMessage :: Lens.Lens' ConfigStreamDeliveryInfo (Prelude.Maybe Prelude.Text)
configStreamDeliveryInfo_lastErrorMessage = Lens.lens (\ConfigStreamDeliveryInfo' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastErrorMessage = a} :: ConfigStreamDeliveryInfo)

-- | The error code from the last attempted delivery.
configStreamDeliveryInfo_lastErrorCode :: Lens.Lens' ConfigStreamDeliveryInfo (Prelude.Maybe Prelude.Text)
configStreamDeliveryInfo_lastErrorCode = Lens.lens (\ConfigStreamDeliveryInfo' {lastErrorCode} -> lastErrorCode) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastErrorCode = a} :: ConfigStreamDeliveryInfo)

-- | Status of the last attempted delivery.
--
-- __Note__ Providing an SNS topic on a
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel>
-- for AWS Config is optional. If the SNS delivery is turned off, the last
-- status will be __Not_Applicable__.
configStreamDeliveryInfo_lastStatus :: Lens.Lens' ConfigStreamDeliveryInfo (Prelude.Maybe DeliveryStatus)
configStreamDeliveryInfo_lastStatus = Lens.lens (\ConfigStreamDeliveryInfo' {lastStatus} -> lastStatus) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastStatus = a} :: ConfigStreamDeliveryInfo)

-- | The time from the last status change.
configStreamDeliveryInfo_lastStatusChangeTime :: Lens.Lens' ConfigStreamDeliveryInfo (Prelude.Maybe Prelude.UTCTime)
configStreamDeliveryInfo_lastStatusChangeTime = Lens.lens (\ConfigStreamDeliveryInfo' {lastStatusChangeTime} -> lastStatusChangeTime) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastStatusChangeTime = a} :: ConfigStreamDeliveryInfo) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ConfigStreamDeliveryInfo where
  parseJSON =
    Prelude.withObject
      "ConfigStreamDeliveryInfo"
      ( \x ->
          ConfigStreamDeliveryInfo'
            Prelude.<$> (x Prelude..:? "lastErrorMessage")
            Prelude.<*> (x Prelude..:? "lastErrorCode")
            Prelude.<*> (x Prelude..:? "lastStatus")
            Prelude.<*> (x Prelude..:? "lastStatusChangeTime")
      )

instance Prelude.Hashable ConfigStreamDeliveryInfo

instance Prelude.NFData ConfigStreamDeliveryInfo
