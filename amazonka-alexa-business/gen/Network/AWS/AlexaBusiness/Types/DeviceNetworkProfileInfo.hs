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
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Detailed information about a device\'s network profile.
--
-- /See:/ 'newDeviceNetworkProfileInfo' smart constructor.
data DeviceNetworkProfileInfo = DeviceNetworkProfileInfo'
  { -- | The time (in epoch) when the certificate expires.
    certificateExpirationTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the certificate associated with a device.
    certificateArn :: Core.Maybe Core.Text,
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceNetworkProfileInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateExpirationTime', 'deviceNetworkProfileInfo_certificateExpirationTime' - The time (in epoch) when the certificate expires.
--
-- 'certificateArn', 'deviceNetworkProfileInfo_certificateArn' - The ARN of the certificate associated with a device.
--
-- 'networkProfileArn', 'deviceNetworkProfileInfo_networkProfileArn' - The ARN of the network profile associated with a device.
newDeviceNetworkProfileInfo ::
  DeviceNetworkProfileInfo
newDeviceNetworkProfileInfo =
  DeviceNetworkProfileInfo'
    { certificateExpirationTime =
        Core.Nothing,
      certificateArn = Core.Nothing,
      networkProfileArn = Core.Nothing
    }

-- | The time (in epoch) when the certificate expires.
deviceNetworkProfileInfo_certificateExpirationTime :: Lens.Lens' DeviceNetworkProfileInfo (Core.Maybe Core.UTCTime)
deviceNetworkProfileInfo_certificateExpirationTime = Lens.lens (\DeviceNetworkProfileInfo' {certificateExpirationTime} -> certificateExpirationTime) (\s@DeviceNetworkProfileInfo' {} a -> s {certificateExpirationTime = a} :: DeviceNetworkProfileInfo) Core.. Lens.mapping Core._Time

-- | The ARN of the certificate associated with a device.
deviceNetworkProfileInfo_certificateArn :: Lens.Lens' DeviceNetworkProfileInfo (Core.Maybe Core.Text)
deviceNetworkProfileInfo_certificateArn = Lens.lens (\DeviceNetworkProfileInfo' {certificateArn} -> certificateArn) (\s@DeviceNetworkProfileInfo' {} a -> s {certificateArn = a} :: DeviceNetworkProfileInfo)

-- | The ARN of the network profile associated with a device.
deviceNetworkProfileInfo_networkProfileArn :: Lens.Lens' DeviceNetworkProfileInfo (Core.Maybe Core.Text)
deviceNetworkProfileInfo_networkProfileArn = Lens.lens (\DeviceNetworkProfileInfo' {networkProfileArn} -> networkProfileArn) (\s@DeviceNetworkProfileInfo' {} a -> s {networkProfileArn = a} :: DeviceNetworkProfileInfo)

instance Core.FromJSON DeviceNetworkProfileInfo where
  parseJSON =
    Core.withObject
      "DeviceNetworkProfileInfo"
      ( \x ->
          DeviceNetworkProfileInfo'
            Core.<$> (x Core..:? "CertificateExpirationTime")
            Core.<*> (x Core..:? "CertificateArn")
            Core.<*> (x Core..:? "NetworkProfileArn")
      )

instance Core.Hashable DeviceNetworkProfileInfo

instance Core.NFData DeviceNetworkProfileInfo
