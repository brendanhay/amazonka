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
-- Module      : Amazonka.IoTWireless.Types.MulticastWirelessMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.MulticastWirelessMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.LoRaWANMulticastMetadata
import qualified Amazonka.Prelude as Prelude

-- | Wireless metadata that is to be sent to multicast group.
--
-- /See:/ 'newMulticastWirelessMetadata' smart constructor.
data MulticastWirelessMetadata = MulticastWirelessMetadata'
  { loRaWAN :: Prelude.Maybe LoRaWANMulticastMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MulticastWirelessMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loRaWAN', 'multicastWirelessMetadata_loRaWAN' - Undocumented member.
newMulticastWirelessMetadata ::
  MulticastWirelessMetadata
newMulticastWirelessMetadata =
  MulticastWirelessMetadata'
    { loRaWAN =
        Prelude.Nothing
    }

-- | Undocumented member.
multicastWirelessMetadata_loRaWAN :: Lens.Lens' MulticastWirelessMetadata (Prelude.Maybe LoRaWANMulticastMetadata)
multicastWirelessMetadata_loRaWAN = Lens.lens (\MulticastWirelessMetadata' {loRaWAN} -> loRaWAN) (\s@MulticastWirelessMetadata' {} a -> s {loRaWAN = a} :: MulticastWirelessMetadata)

instance Prelude.Hashable MulticastWirelessMetadata where
  hashWithSalt _salt MulticastWirelessMetadata' {..} =
    _salt `Prelude.hashWithSalt` loRaWAN

instance Prelude.NFData MulticastWirelessMetadata where
  rnf MulticastWirelessMetadata' {..} =
    Prelude.rnf loRaWAN

instance Core.ToJSON MulticastWirelessMetadata where
  toJSON MulticastWirelessMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [("LoRaWAN" Core..=) Prelude.<$> loRaWAN]
      )
