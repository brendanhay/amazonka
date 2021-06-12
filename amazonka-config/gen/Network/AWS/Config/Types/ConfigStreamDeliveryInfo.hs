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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list that contains the status of the delivery of the configuration
-- stream notification to the Amazon SNS topic.
--
-- /See:/ 'newConfigStreamDeliveryInfo' smart constructor.
data ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo'
  { -- | The error message from the last attempted delivery.
    lastErrorMessage :: Core.Maybe Core.Text,
    -- | The error code from the last attempted delivery.
    lastErrorCode :: Core.Maybe Core.Text,
    -- | Status of the last attempted delivery.
    --
    -- __Note__ Providing an SNS topic on a
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel>
    -- for AWS Config is optional. If the SNS delivery is turned off, the last
    -- status will be __Not_Applicable__.
    lastStatus :: Core.Maybe DeliveryStatus,
    -- | The time from the last status change.
    lastStatusChangeTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      lastErrorCode = Core.Nothing,
      lastStatus = Core.Nothing,
      lastStatusChangeTime = Core.Nothing
    }

-- | The error message from the last attempted delivery.
configStreamDeliveryInfo_lastErrorMessage :: Lens.Lens' ConfigStreamDeliveryInfo (Core.Maybe Core.Text)
configStreamDeliveryInfo_lastErrorMessage = Lens.lens (\ConfigStreamDeliveryInfo' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastErrorMessage = a} :: ConfigStreamDeliveryInfo)

-- | The error code from the last attempted delivery.
configStreamDeliveryInfo_lastErrorCode :: Lens.Lens' ConfigStreamDeliveryInfo (Core.Maybe Core.Text)
configStreamDeliveryInfo_lastErrorCode = Lens.lens (\ConfigStreamDeliveryInfo' {lastErrorCode} -> lastErrorCode) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastErrorCode = a} :: ConfigStreamDeliveryInfo)

-- | Status of the last attempted delivery.
--
-- __Note__ Providing an SNS topic on a
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel>
-- for AWS Config is optional. If the SNS delivery is turned off, the last
-- status will be __Not_Applicable__.
configStreamDeliveryInfo_lastStatus :: Lens.Lens' ConfigStreamDeliveryInfo (Core.Maybe DeliveryStatus)
configStreamDeliveryInfo_lastStatus = Lens.lens (\ConfigStreamDeliveryInfo' {lastStatus} -> lastStatus) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastStatus = a} :: ConfigStreamDeliveryInfo)

-- | The time from the last status change.
configStreamDeliveryInfo_lastStatusChangeTime :: Lens.Lens' ConfigStreamDeliveryInfo (Core.Maybe Core.UTCTime)
configStreamDeliveryInfo_lastStatusChangeTime = Lens.lens (\ConfigStreamDeliveryInfo' {lastStatusChangeTime} -> lastStatusChangeTime) (\s@ConfigStreamDeliveryInfo' {} a -> s {lastStatusChangeTime = a} :: ConfigStreamDeliveryInfo) Core.. Lens.mapping Core._Time

instance Core.FromJSON ConfigStreamDeliveryInfo where
  parseJSON =
    Core.withObject
      "ConfigStreamDeliveryInfo"
      ( \x ->
          ConfigStreamDeliveryInfo'
            Core.<$> (x Core..:? "lastErrorMessage")
            Core.<*> (x Core..:? "lastErrorCode")
            Core.<*> (x Core..:? "lastStatus")
            Core.<*> (x Core..:? "lastStatusChangeTime")
      )

instance Core.Hashable ConfigStreamDeliveryInfo

instance Core.NFData ConfigStreamDeliveryInfo
