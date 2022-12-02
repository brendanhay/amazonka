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
-- Module      : Amazonka.PrivateNetworks.Types.DeviceIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.DeviceIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.DeviceIdentifierStatus

-- | Information about a subscriber of a device that can use a network.
--
-- /See:/ 'newDeviceIdentifier' smart constructor.
data DeviceIdentifier = DeviceIdentifier'
  { -- | The International Mobile Subscriber Identity of the device identifier.
    imsi :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the traffic group to which the device
    -- identifier belongs.
    trafficGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network on which the device
    -- identifier appears.
    networkArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the device identifier.
    status :: Prelude.Maybe DeviceIdentifierStatus,
    -- | The Amazon Resource Name (ARN) of the order used to purchase the device
    -- identifier.
    orderArn :: Prelude.Maybe Prelude.Text,
    -- | The vendor of the device identifier.
    vendor :: Prelude.Maybe Prelude.Text,
    -- | The creation time of this device identifier.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Integrated Circuit Card Identifier of the device identifier.
    iccid :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the device identifier.
    deviceIdentifierArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imsi', 'deviceIdentifier_imsi' - The International Mobile Subscriber Identity of the device identifier.
--
-- 'trafficGroupArn', 'deviceIdentifier_trafficGroupArn' - The Amazon Resource Name (ARN) of the traffic group to which the device
-- identifier belongs.
--
-- 'networkArn', 'deviceIdentifier_networkArn' - The Amazon Resource Name (ARN) of the network on which the device
-- identifier appears.
--
-- 'status', 'deviceIdentifier_status' - The status of the device identifier.
--
-- 'orderArn', 'deviceIdentifier_orderArn' - The Amazon Resource Name (ARN) of the order used to purchase the device
-- identifier.
--
-- 'vendor', 'deviceIdentifier_vendor' - The vendor of the device identifier.
--
-- 'createdAt', 'deviceIdentifier_createdAt' - The creation time of this device identifier.
--
-- 'iccid', 'deviceIdentifier_iccid' - The Integrated Circuit Card Identifier of the device identifier.
--
-- 'deviceIdentifierArn', 'deviceIdentifier_deviceIdentifierArn' - The Amazon Resource Name (ARN) of the device identifier.
newDeviceIdentifier ::
  DeviceIdentifier
newDeviceIdentifier =
  DeviceIdentifier'
    { imsi = Prelude.Nothing,
      trafficGroupArn = Prelude.Nothing,
      networkArn = Prelude.Nothing,
      status = Prelude.Nothing,
      orderArn = Prelude.Nothing,
      vendor = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      iccid = Prelude.Nothing,
      deviceIdentifierArn = Prelude.Nothing
    }

-- | The International Mobile Subscriber Identity of the device identifier.
deviceIdentifier_imsi :: Lens.Lens' DeviceIdentifier (Prelude.Maybe Prelude.Text)
deviceIdentifier_imsi = Lens.lens (\DeviceIdentifier' {imsi} -> imsi) (\s@DeviceIdentifier' {} a -> s {imsi = a} :: DeviceIdentifier) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the traffic group to which the device
-- identifier belongs.
deviceIdentifier_trafficGroupArn :: Lens.Lens' DeviceIdentifier (Prelude.Maybe Prelude.Text)
deviceIdentifier_trafficGroupArn = Lens.lens (\DeviceIdentifier' {trafficGroupArn} -> trafficGroupArn) (\s@DeviceIdentifier' {} a -> s {trafficGroupArn = a} :: DeviceIdentifier)

-- | The Amazon Resource Name (ARN) of the network on which the device
-- identifier appears.
deviceIdentifier_networkArn :: Lens.Lens' DeviceIdentifier (Prelude.Maybe Prelude.Text)
deviceIdentifier_networkArn = Lens.lens (\DeviceIdentifier' {networkArn} -> networkArn) (\s@DeviceIdentifier' {} a -> s {networkArn = a} :: DeviceIdentifier)

-- | The status of the device identifier.
deviceIdentifier_status :: Lens.Lens' DeviceIdentifier (Prelude.Maybe DeviceIdentifierStatus)
deviceIdentifier_status = Lens.lens (\DeviceIdentifier' {status} -> status) (\s@DeviceIdentifier' {} a -> s {status = a} :: DeviceIdentifier)

-- | The Amazon Resource Name (ARN) of the order used to purchase the device
-- identifier.
deviceIdentifier_orderArn :: Lens.Lens' DeviceIdentifier (Prelude.Maybe Prelude.Text)
deviceIdentifier_orderArn = Lens.lens (\DeviceIdentifier' {orderArn} -> orderArn) (\s@DeviceIdentifier' {} a -> s {orderArn = a} :: DeviceIdentifier)

-- | The vendor of the device identifier.
deviceIdentifier_vendor :: Lens.Lens' DeviceIdentifier (Prelude.Maybe Prelude.Text)
deviceIdentifier_vendor = Lens.lens (\DeviceIdentifier' {vendor} -> vendor) (\s@DeviceIdentifier' {} a -> s {vendor = a} :: DeviceIdentifier)

-- | The creation time of this device identifier.
deviceIdentifier_createdAt :: Lens.Lens' DeviceIdentifier (Prelude.Maybe Prelude.UTCTime)
deviceIdentifier_createdAt = Lens.lens (\DeviceIdentifier' {createdAt} -> createdAt) (\s@DeviceIdentifier' {} a -> s {createdAt = a} :: DeviceIdentifier) Prelude.. Lens.mapping Data._Time

-- | The Integrated Circuit Card Identifier of the device identifier.
deviceIdentifier_iccid :: Lens.Lens' DeviceIdentifier (Prelude.Maybe Prelude.Text)
deviceIdentifier_iccid = Lens.lens (\DeviceIdentifier' {iccid} -> iccid) (\s@DeviceIdentifier' {} a -> s {iccid = a} :: DeviceIdentifier)

-- | The Amazon Resource Name (ARN) of the device identifier.
deviceIdentifier_deviceIdentifierArn :: Lens.Lens' DeviceIdentifier (Prelude.Maybe Prelude.Text)
deviceIdentifier_deviceIdentifierArn = Lens.lens (\DeviceIdentifier' {deviceIdentifierArn} -> deviceIdentifierArn) (\s@DeviceIdentifier' {} a -> s {deviceIdentifierArn = a} :: DeviceIdentifier)

instance Data.FromJSON DeviceIdentifier where
  parseJSON =
    Data.withObject
      "DeviceIdentifier"
      ( \x ->
          DeviceIdentifier'
            Prelude.<$> (x Data..:? "imsi")
            Prelude.<*> (x Data..:? "trafficGroupArn")
            Prelude.<*> (x Data..:? "networkArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "orderArn")
            Prelude.<*> (x Data..:? "vendor")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "iccid")
            Prelude.<*> (x Data..:? "deviceIdentifierArn")
      )

instance Prelude.Hashable DeviceIdentifier where
  hashWithSalt _salt DeviceIdentifier' {..} =
    _salt `Prelude.hashWithSalt` imsi
      `Prelude.hashWithSalt` trafficGroupArn
      `Prelude.hashWithSalt` networkArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` orderArn
      `Prelude.hashWithSalt` vendor
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` iccid
      `Prelude.hashWithSalt` deviceIdentifierArn

instance Prelude.NFData DeviceIdentifier where
  rnf DeviceIdentifier' {..} =
    Prelude.rnf imsi
      `Prelude.seq` Prelude.rnf trafficGroupArn
      `Prelude.seq` Prelude.rnf networkArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf orderArn
      `Prelude.seq` Prelude.rnf vendor
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf iccid
      `Prelude.seq` Prelude.rnf deviceIdentifierArn
