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
-- Module      : Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Lists iSCSI information about a volume.
--
-- /See:/ 'newVolumeiSCSIAttributes' smart constructor.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'
  { -- | Indicates whether mutual CHAP is enabled for the iSCSI target.
    chapEnabled :: Core.Maybe Core.Bool,
    -- | The logical disk number.
    lunNumber :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the volume target.
    targetARN :: Core.Maybe Core.Text,
    -- | The network interface identifier.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The port used to communicate with iSCSI targets.
    networkInterfacePort :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeiSCSIAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chapEnabled', 'volumeiSCSIAttributes_chapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- 'lunNumber', 'volumeiSCSIAttributes_lunNumber' - The logical disk number.
--
-- 'targetARN', 'volumeiSCSIAttributes_targetARN' - The Amazon Resource Name (ARN) of the volume target.
--
-- 'networkInterfaceId', 'volumeiSCSIAttributes_networkInterfaceId' - The network interface identifier.
--
-- 'networkInterfacePort', 'volumeiSCSIAttributes_networkInterfacePort' - The port used to communicate with iSCSI targets.
newVolumeiSCSIAttributes ::
  VolumeiSCSIAttributes
newVolumeiSCSIAttributes =
  VolumeiSCSIAttributes'
    { chapEnabled = Core.Nothing,
      lunNumber = Core.Nothing,
      targetARN = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      networkInterfacePort = Core.Nothing
    }

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
volumeiSCSIAttributes_chapEnabled :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Core.Bool)
volumeiSCSIAttributes_chapEnabled = Lens.lens (\VolumeiSCSIAttributes' {chapEnabled} -> chapEnabled) (\s@VolumeiSCSIAttributes' {} a -> s {chapEnabled = a} :: VolumeiSCSIAttributes)

-- | The logical disk number.
volumeiSCSIAttributes_lunNumber :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Core.Natural)
volumeiSCSIAttributes_lunNumber = Lens.lens (\VolumeiSCSIAttributes' {lunNumber} -> lunNumber) (\s@VolumeiSCSIAttributes' {} a -> s {lunNumber = a} :: VolumeiSCSIAttributes)

-- | The Amazon Resource Name (ARN) of the volume target.
volumeiSCSIAttributes_targetARN :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Core.Text)
volumeiSCSIAttributes_targetARN = Lens.lens (\VolumeiSCSIAttributes' {targetARN} -> targetARN) (\s@VolumeiSCSIAttributes' {} a -> s {targetARN = a} :: VolumeiSCSIAttributes)

-- | The network interface identifier.
volumeiSCSIAttributes_networkInterfaceId :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Core.Text)
volumeiSCSIAttributes_networkInterfaceId = Lens.lens (\VolumeiSCSIAttributes' {networkInterfaceId} -> networkInterfaceId) (\s@VolumeiSCSIAttributes' {} a -> s {networkInterfaceId = a} :: VolumeiSCSIAttributes)

-- | The port used to communicate with iSCSI targets.
volumeiSCSIAttributes_networkInterfacePort :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Core.Int)
volumeiSCSIAttributes_networkInterfacePort = Lens.lens (\VolumeiSCSIAttributes' {networkInterfacePort} -> networkInterfacePort) (\s@VolumeiSCSIAttributes' {} a -> s {networkInterfacePort = a} :: VolumeiSCSIAttributes)

instance Core.FromJSON VolumeiSCSIAttributes where
  parseJSON =
    Core.withObject
      "VolumeiSCSIAttributes"
      ( \x ->
          VolumeiSCSIAttributes'
            Core.<$> (x Core..:? "ChapEnabled")
            Core.<*> (x Core..:? "LunNumber")
            Core.<*> (x Core..:? "TargetARN")
            Core.<*> (x Core..:? "NetworkInterfaceId")
            Core.<*> (x Core..:? "NetworkInterfacePort")
      )

instance Core.Hashable VolumeiSCSIAttributes

instance Core.NFData VolumeiSCSIAttributes
