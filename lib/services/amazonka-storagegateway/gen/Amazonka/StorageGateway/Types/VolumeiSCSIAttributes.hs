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
-- Module      : Amazonka.StorageGateway.Types.VolumeiSCSIAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.VolumeiSCSIAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Lists iSCSI information about a volume.
--
-- /See:/ 'newVolumeiSCSIAttributes' smart constructor.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'
  { -- | The logical disk number.
    lunNumber :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the volume target.
    targetARN :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether mutual CHAP is enabled for the iSCSI target.
    chapEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The network interface identifier.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The port used to communicate with iSCSI targets.
    networkInterfacePort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeiSCSIAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lunNumber', 'volumeiSCSIAttributes_lunNumber' - The logical disk number.
--
-- 'targetARN', 'volumeiSCSIAttributes_targetARN' - The Amazon Resource Name (ARN) of the volume target.
--
-- 'chapEnabled', 'volumeiSCSIAttributes_chapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- 'networkInterfaceId', 'volumeiSCSIAttributes_networkInterfaceId' - The network interface identifier.
--
-- 'networkInterfacePort', 'volumeiSCSIAttributes_networkInterfacePort' - The port used to communicate with iSCSI targets.
newVolumeiSCSIAttributes ::
  VolumeiSCSIAttributes
newVolumeiSCSIAttributes =
  VolumeiSCSIAttributes'
    { lunNumber = Prelude.Nothing,
      targetARN = Prelude.Nothing,
      chapEnabled = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      networkInterfacePort = Prelude.Nothing
    }

-- | The logical disk number.
volumeiSCSIAttributes_lunNumber :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Natural)
volumeiSCSIAttributes_lunNumber = Lens.lens (\VolumeiSCSIAttributes' {lunNumber} -> lunNumber) (\s@VolumeiSCSIAttributes' {} a -> s {lunNumber = a} :: VolumeiSCSIAttributes)

-- | The Amazon Resource Name (ARN) of the volume target.
volumeiSCSIAttributes_targetARN :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Text)
volumeiSCSIAttributes_targetARN = Lens.lens (\VolumeiSCSIAttributes' {targetARN} -> targetARN) (\s@VolumeiSCSIAttributes' {} a -> s {targetARN = a} :: VolumeiSCSIAttributes)

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
volumeiSCSIAttributes_chapEnabled :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Bool)
volumeiSCSIAttributes_chapEnabled = Lens.lens (\VolumeiSCSIAttributes' {chapEnabled} -> chapEnabled) (\s@VolumeiSCSIAttributes' {} a -> s {chapEnabled = a} :: VolumeiSCSIAttributes)

-- | The network interface identifier.
volumeiSCSIAttributes_networkInterfaceId :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Text)
volumeiSCSIAttributes_networkInterfaceId = Lens.lens (\VolumeiSCSIAttributes' {networkInterfaceId} -> networkInterfaceId) (\s@VolumeiSCSIAttributes' {} a -> s {networkInterfaceId = a} :: VolumeiSCSIAttributes)

-- | The port used to communicate with iSCSI targets.
volumeiSCSIAttributes_networkInterfacePort :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Int)
volumeiSCSIAttributes_networkInterfacePort = Lens.lens (\VolumeiSCSIAttributes' {networkInterfacePort} -> networkInterfacePort) (\s@VolumeiSCSIAttributes' {} a -> s {networkInterfacePort = a} :: VolumeiSCSIAttributes)

instance Core.FromJSON VolumeiSCSIAttributes where
  parseJSON =
    Core.withObject
      "VolumeiSCSIAttributes"
      ( \x ->
          VolumeiSCSIAttributes'
            Prelude.<$> (x Core..:? "LunNumber")
            Prelude.<*> (x Core..:? "TargetARN")
            Prelude.<*> (x Core..:? "ChapEnabled")
            Prelude.<*> (x Core..:? "NetworkInterfaceId")
            Prelude.<*> (x Core..:? "NetworkInterfacePort")
      )

instance Prelude.Hashable VolumeiSCSIAttributes where
  hashWithSalt salt' VolumeiSCSIAttributes' {..} =
    salt' `Prelude.hashWithSalt` networkInterfacePort
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` chapEnabled
      `Prelude.hashWithSalt` targetARN
      `Prelude.hashWithSalt` lunNumber

instance Prelude.NFData VolumeiSCSIAttributes where
  rnf VolumeiSCSIAttributes' {..} =
    Prelude.rnf lunNumber
      `Prelude.seq` Prelude.rnf networkInterfacePort
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf chapEnabled
      `Prelude.seq` Prelude.rnf targetARN
