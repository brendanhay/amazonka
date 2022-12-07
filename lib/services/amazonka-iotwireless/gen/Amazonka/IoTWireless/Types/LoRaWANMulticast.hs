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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { rfRegion :: Prelude.Maybe SupportedRfRegion,
    dlClass :: Prelude.Maybe DlClass
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
-- 'rfRegion', 'loRaWANMulticast_rfRegion' - Undocumented member.
--
-- 'dlClass', 'loRaWANMulticast_dlClass' - Undocumented member.
newLoRaWANMulticast ::
  LoRaWANMulticast
newLoRaWANMulticast =
  LoRaWANMulticast'
    { rfRegion = Prelude.Nothing,
      dlClass = Prelude.Nothing
    }

-- | Undocumented member.
loRaWANMulticast_rfRegion :: Lens.Lens' LoRaWANMulticast (Prelude.Maybe SupportedRfRegion)
loRaWANMulticast_rfRegion = Lens.lens (\LoRaWANMulticast' {rfRegion} -> rfRegion) (\s@LoRaWANMulticast' {} a -> s {rfRegion = a} :: LoRaWANMulticast)

-- | Undocumented member.
loRaWANMulticast_dlClass :: Lens.Lens' LoRaWANMulticast (Prelude.Maybe DlClass)
loRaWANMulticast_dlClass = Lens.lens (\LoRaWANMulticast' {dlClass} -> dlClass) (\s@LoRaWANMulticast' {} a -> s {dlClass = a} :: LoRaWANMulticast)

instance Prelude.Hashable LoRaWANMulticast where
  hashWithSalt _salt LoRaWANMulticast' {..} =
    _salt `Prelude.hashWithSalt` rfRegion
      `Prelude.hashWithSalt` dlClass

instance Prelude.NFData LoRaWANMulticast where
  rnf LoRaWANMulticast' {..} =
    Prelude.rnf rfRegion
      `Prelude.seq` Prelude.rnf dlClass

instance Data.ToJSON LoRaWANMulticast where
  toJSON LoRaWANMulticast' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RfRegion" Data..=) Prelude.<$> rfRegion,
            ("DlClass" Data..=) Prelude.<$> dlClass
          ]
      )
