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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANMulticast
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANMulticast where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.DlClass
import Amazonka.IoTWireless.Types.SupportedRfRegion
import qualified Amazonka.Prelude as Prelude

-- | The LoRaWAN information that is to be used with the multicast group.
--
-- /See:/ 'newLoRaWANMulticast' smart constructor.
data LoRaWANMulticast = LoRaWANMulticast'
  { dlClass :: Prelude.Maybe DlClass,
    rfRegion :: Prelude.Maybe SupportedRfRegion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANMulticast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dlClass', 'loRaWANMulticast_dlClass' - Undocumented member.
--
-- 'rfRegion', 'loRaWANMulticast_rfRegion' - Undocumented member.
newLoRaWANMulticast ::
  LoRaWANMulticast
newLoRaWANMulticast =
  LoRaWANMulticast'
    { dlClass = Prelude.Nothing,
      rfRegion = Prelude.Nothing
    }

-- | Undocumented member.
loRaWANMulticast_dlClass :: Lens.Lens' LoRaWANMulticast (Prelude.Maybe DlClass)
loRaWANMulticast_dlClass = Lens.lens (\LoRaWANMulticast' {dlClass} -> dlClass) (\s@LoRaWANMulticast' {} a -> s {dlClass = a} :: LoRaWANMulticast)

-- | Undocumented member.
loRaWANMulticast_rfRegion :: Lens.Lens' LoRaWANMulticast (Prelude.Maybe SupportedRfRegion)
loRaWANMulticast_rfRegion = Lens.lens (\LoRaWANMulticast' {rfRegion} -> rfRegion) (\s@LoRaWANMulticast' {} a -> s {rfRegion = a} :: LoRaWANMulticast)

instance Prelude.Hashable LoRaWANMulticast where
  hashWithSalt _salt LoRaWANMulticast' {..} =
    _salt
      `Prelude.hashWithSalt` dlClass
      `Prelude.hashWithSalt` rfRegion

instance Prelude.NFData LoRaWANMulticast where
  rnf LoRaWANMulticast' {..} =
    Prelude.rnf dlClass
      `Prelude.seq` Prelude.rnf rfRegion

instance Data.ToJSON LoRaWANMulticast where
  toJSON LoRaWANMulticast' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DlClass" Data..=) Prelude.<$> dlClass,
            ("RfRegion" Data..=) Prelude.<$> rfRegion
          ]
      )
