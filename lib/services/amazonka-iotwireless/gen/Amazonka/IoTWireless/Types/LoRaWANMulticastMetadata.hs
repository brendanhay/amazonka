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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANMulticastMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANMulticastMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata information of the LoRaWAN multicast group.
--
-- /See:/ 'newLoRaWANMulticastMetadata' smart constructor.
data LoRaWANMulticastMetadata = LoRaWANMulticastMetadata'
  { fPort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANMulticastMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fPort', 'loRaWANMulticastMetadata_fPort' - Undocumented member.
newLoRaWANMulticastMetadata ::
  LoRaWANMulticastMetadata
newLoRaWANMulticastMetadata =
  LoRaWANMulticastMetadata' {fPort = Prelude.Nothing}

-- | Undocumented member.
loRaWANMulticastMetadata_fPort :: Lens.Lens' LoRaWANMulticastMetadata (Prelude.Maybe Prelude.Natural)
loRaWANMulticastMetadata_fPort = Lens.lens (\LoRaWANMulticastMetadata' {fPort} -> fPort) (\s@LoRaWANMulticastMetadata' {} a -> s {fPort = a} :: LoRaWANMulticastMetadata)

instance Prelude.Hashable LoRaWANMulticastMetadata where
  hashWithSalt _salt LoRaWANMulticastMetadata' {..} =
    _salt `Prelude.hashWithSalt` fPort

instance Prelude.NFData LoRaWANMulticastMetadata where
  rnf LoRaWANMulticastMetadata' {..} = Prelude.rnf fPort

instance Data.ToJSON LoRaWANMulticastMetadata where
  toJSON LoRaWANMulticastMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [("FPort" Data..=) Prelude.<$> fPort]
      )
