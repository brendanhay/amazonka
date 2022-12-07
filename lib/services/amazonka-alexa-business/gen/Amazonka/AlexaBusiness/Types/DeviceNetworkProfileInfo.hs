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
-- Module      : Amazonka.AlexaBusiness.Types.DeviceNetworkProfileInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.DeviceNetworkProfileInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about a device\'s network profile.
--
-- /See:/ 'newDeviceNetworkProfileInfo' smart constructor.
data DeviceNetworkProfileInfo = DeviceNetworkProfileInfo'
  { -- | The ARN of the certificate associated with a device.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The time (in epoch) when the certificate expires.
    certificateExpirationTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceNetworkProfileInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'deviceNetworkProfileInfo_certificateArn' - The ARN of the certificate associated with a device.
--
-- 'certificateExpirationTime', 'deviceNetworkProfileInfo_certificateExpirationTime' - The time (in epoch) when the certificate expires.
--
-- 'networkProfileArn', 'deviceNetworkProfileInfo_networkProfileArn' - The ARN of the network profile associated with a device.
newDeviceNetworkProfileInfo ::
  DeviceNetworkProfileInfo
newDeviceNetworkProfileInfo =
  DeviceNetworkProfileInfo'
    { certificateArn =
        Prelude.Nothing,
      certificateExpirationTime = Prelude.Nothing,
      networkProfileArn = Prelude.Nothing
    }

-- | The ARN of the certificate associated with a device.
deviceNetworkProfileInfo_certificateArn :: Lens.Lens' DeviceNetworkProfileInfo (Prelude.Maybe Prelude.Text)
deviceNetworkProfileInfo_certificateArn = Lens.lens (\DeviceNetworkProfileInfo' {certificateArn} -> certificateArn) (\s@DeviceNetworkProfileInfo' {} a -> s {certificateArn = a} :: DeviceNetworkProfileInfo)

-- | The time (in epoch) when the certificate expires.
deviceNetworkProfileInfo_certificateExpirationTime :: Lens.Lens' DeviceNetworkProfileInfo (Prelude.Maybe Prelude.UTCTime)
deviceNetworkProfileInfo_certificateExpirationTime = Lens.lens (\DeviceNetworkProfileInfo' {certificateExpirationTime} -> certificateExpirationTime) (\s@DeviceNetworkProfileInfo' {} a -> s {certificateExpirationTime = a} :: DeviceNetworkProfileInfo) Prelude.. Lens.mapping Data._Time

-- | The ARN of the network profile associated with a device.
deviceNetworkProfileInfo_networkProfileArn :: Lens.Lens' DeviceNetworkProfileInfo (Prelude.Maybe Prelude.Text)
deviceNetworkProfileInfo_networkProfileArn = Lens.lens (\DeviceNetworkProfileInfo' {networkProfileArn} -> networkProfileArn) (\s@DeviceNetworkProfileInfo' {} a -> s {networkProfileArn = a} :: DeviceNetworkProfileInfo)

instance Data.FromJSON DeviceNetworkProfileInfo where
  parseJSON =
    Data.withObject
      "DeviceNetworkProfileInfo"
      ( \x ->
          DeviceNetworkProfileInfo'
            Prelude.<$> (x Data..:? "CertificateArn")
            Prelude.<*> (x Data..:? "CertificateExpirationTime")
            Prelude.<*> (x Data..:? "NetworkProfileArn")
      )

instance Prelude.Hashable DeviceNetworkProfileInfo where
  hashWithSalt _salt DeviceNetworkProfileInfo' {..} =
    _salt `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateExpirationTime
      `Prelude.hashWithSalt` networkProfileArn

instance Prelude.NFData DeviceNetworkProfileInfo where
  rnf DeviceNetworkProfileInfo' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateExpirationTime
      `Prelude.seq` Prelude.rnf networkProfileArn
