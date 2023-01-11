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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.VolumeiSCSIAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Lists iSCSI information about a volume.
--
-- /See:/ 'newVolumeiSCSIAttributes' smart constructor.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'
  { -- | Indicates whether mutual CHAP is enabled for the iSCSI target.
    chapEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The logical disk number.
    lunNumber :: Prelude.Maybe Prelude.Natural,
    -- | The network interface identifier.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The port used to communicate with iSCSI targets.
    networkInterfacePort :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the volume target.
    targetARN :: Prelude.Maybe Prelude.Text
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
-- 'chapEnabled', 'volumeiSCSIAttributes_chapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- 'lunNumber', 'volumeiSCSIAttributes_lunNumber' - The logical disk number.
--
-- 'networkInterfaceId', 'volumeiSCSIAttributes_networkInterfaceId' - The network interface identifier.
--
-- 'networkInterfacePort', 'volumeiSCSIAttributes_networkInterfacePort' - The port used to communicate with iSCSI targets.
--
-- 'targetARN', 'volumeiSCSIAttributes_targetARN' - The Amazon Resource Name (ARN) of the volume target.
newVolumeiSCSIAttributes ::
  VolumeiSCSIAttributes
newVolumeiSCSIAttributes =
  VolumeiSCSIAttributes'
    { chapEnabled =
        Prelude.Nothing,
      lunNumber = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      networkInterfacePort = Prelude.Nothing,
      targetARN = Prelude.Nothing
    }

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
volumeiSCSIAttributes_chapEnabled :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Bool)
volumeiSCSIAttributes_chapEnabled = Lens.lens (\VolumeiSCSIAttributes' {chapEnabled} -> chapEnabled) (\s@VolumeiSCSIAttributes' {} a -> s {chapEnabled = a} :: VolumeiSCSIAttributes)

-- | The logical disk number.
volumeiSCSIAttributes_lunNumber :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Natural)
volumeiSCSIAttributes_lunNumber = Lens.lens (\VolumeiSCSIAttributes' {lunNumber} -> lunNumber) (\s@VolumeiSCSIAttributes' {} a -> s {lunNumber = a} :: VolumeiSCSIAttributes)

-- | The network interface identifier.
volumeiSCSIAttributes_networkInterfaceId :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Text)
volumeiSCSIAttributes_networkInterfaceId = Lens.lens (\VolumeiSCSIAttributes' {networkInterfaceId} -> networkInterfaceId) (\s@VolumeiSCSIAttributes' {} a -> s {networkInterfaceId = a} :: VolumeiSCSIAttributes)

-- | The port used to communicate with iSCSI targets.
volumeiSCSIAttributes_networkInterfacePort :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Int)
volumeiSCSIAttributes_networkInterfacePort = Lens.lens (\VolumeiSCSIAttributes' {networkInterfacePort} -> networkInterfacePort) (\s@VolumeiSCSIAttributes' {} a -> s {networkInterfacePort = a} :: VolumeiSCSIAttributes)

-- | The Amazon Resource Name (ARN) of the volume target.
volumeiSCSIAttributes_targetARN :: Lens.Lens' VolumeiSCSIAttributes (Prelude.Maybe Prelude.Text)
volumeiSCSIAttributes_targetARN = Lens.lens (\VolumeiSCSIAttributes' {targetARN} -> targetARN) (\s@VolumeiSCSIAttributes' {} a -> s {targetARN = a} :: VolumeiSCSIAttributes)

instance Data.FromJSON VolumeiSCSIAttributes where
  parseJSON =
    Data.withObject
      "VolumeiSCSIAttributes"
      ( \x ->
          VolumeiSCSIAttributes'
            Prelude.<$> (x Data..:? "ChapEnabled")
            Prelude.<*> (x Data..:? "LunNumber")
            Prelude.<*> (x Data..:? "NetworkInterfaceId")
            Prelude.<*> (x Data..:? "NetworkInterfacePort")
            Prelude.<*> (x Data..:? "TargetARN")
      )

instance Prelude.Hashable VolumeiSCSIAttributes where
  hashWithSalt _salt VolumeiSCSIAttributes' {..} =
    _salt `Prelude.hashWithSalt` chapEnabled
      `Prelude.hashWithSalt` lunNumber
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` networkInterfacePort
      `Prelude.hashWithSalt` targetARN

instance Prelude.NFData VolumeiSCSIAttributes where
  rnf VolumeiSCSIAttributes' {..} =
    Prelude.rnf chapEnabled
      `Prelude.seq` Prelude.rnf lunNumber
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf networkInterfacePort
      `Prelude.seq` Prelude.rnf targetARN
