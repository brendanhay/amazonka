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
-- Module      : Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Lists iSCSI information about a VTL device.
--
-- /See:/ 'newDeviceiSCSIAttributes' smart constructor.
data DeviceiSCSIAttributes = DeviceiSCSIAttributes'
  { -- | Indicates whether mutual CHAP is enabled for the iSCSI target.
    chapEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the unique Amazon Resource Name (ARN) that encodes the iSCSI
    -- qualified name(iqn) of a tape drive or media changer target.
    targetARN :: Prelude.Maybe Prelude.Text,
    -- | The network interface identifier of the VTL device.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The port used to communicate with iSCSI VTL device targets.
    networkInterfacePort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeviceiSCSIAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chapEnabled', 'deviceiSCSIAttributes_chapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- 'targetARN', 'deviceiSCSIAttributes_targetARN' - Specifies the unique Amazon Resource Name (ARN) that encodes the iSCSI
-- qualified name(iqn) of a tape drive or media changer target.
--
-- 'networkInterfaceId', 'deviceiSCSIAttributes_networkInterfaceId' - The network interface identifier of the VTL device.
--
-- 'networkInterfacePort', 'deviceiSCSIAttributes_networkInterfacePort' - The port used to communicate with iSCSI VTL device targets.
newDeviceiSCSIAttributes ::
  DeviceiSCSIAttributes
newDeviceiSCSIAttributes =
  DeviceiSCSIAttributes'
    { chapEnabled =
        Prelude.Nothing,
      targetARN = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      networkInterfacePort = Prelude.Nothing
    }

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
deviceiSCSIAttributes_chapEnabled :: Lens.Lens' DeviceiSCSIAttributes (Prelude.Maybe Prelude.Bool)
deviceiSCSIAttributes_chapEnabled = Lens.lens (\DeviceiSCSIAttributes' {chapEnabled} -> chapEnabled) (\s@DeviceiSCSIAttributes' {} a -> s {chapEnabled = a} :: DeviceiSCSIAttributes)

-- | Specifies the unique Amazon Resource Name (ARN) that encodes the iSCSI
-- qualified name(iqn) of a tape drive or media changer target.
deviceiSCSIAttributes_targetARN :: Lens.Lens' DeviceiSCSIAttributes (Prelude.Maybe Prelude.Text)
deviceiSCSIAttributes_targetARN = Lens.lens (\DeviceiSCSIAttributes' {targetARN} -> targetARN) (\s@DeviceiSCSIAttributes' {} a -> s {targetARN = a} :: DeviceiSCSIAttributes)

-- | The network interface identifier of the VTL device.
deviceiSCSIAttributes_networkInterfaceId :: Lens.Lens' DeviceiSCSIAttributes (Prelude.Maybe Prelude.Text)
deviceiSCSIAttributes_networkInterfaceId = Lens.lens (\DeviceiSCSIAttributes' {networkInterfaceId} -> networkInterfaceId) (\s@DeviceiSCSIAttributes' {} a -> s {networkInterfaceId = a} :: DeviceiSCSIAttributes)

-- | The port used to communicate with iSCSI VTL device targets.
deviceiSCSIAttributes_networkInterfacePort :: Lens.Lens' DeviceiSCSIAttributes (Prelude.Maybe Prelude.Int)
deviceiSCSIAttributes_networkInterfacePort = Lens.lens (\DeviceiSCSIAttributes' {networkInterfacePort} -> networkInterfacePort) (\s@DeviceiSCSIAttributes' {} a -> s {networkInterfacePort = a} :: DeviceiSCSIAttributes)

instance Prelude.FromJSON DeviceiSCSIAttributes where
  parseJSON =
    Prelude.withObject
      "DeviceiSCSIAttributes"
      ( \x ->
          DeviceiSCSIAttributes'
            Prelude.<$> (x Prelude..:? "ChapEnabled")
            Prelude.<*> (x Prelude..:? "TargetARN")
            Prelude.<*> (x Prelude..:? "NetworkInterfaceId")
            Prelude.<*> (x Prelude..:? "NetworkInterfacePort")
      )

instance Prelude.Hashable DeviceiSCSIAttributes

instance Prelude.NFData DeviceiSCSIAttributes
