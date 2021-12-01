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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANServiceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANServiceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | LoRaWANServiceProfile object.
--
-- /See:/ 'newLoRaWANServiceProfile' smart constructor.
data LoRaWANServiceProfile = LoRaWANServiceProfile'
  { -- | The AddGWMetaData value.
    addGwMetadata :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANServiceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addGwMetadata', 'loRaWANServiceProfile_addGwMetadata' - The AddGWMetaData value.
newLoRaWANServiceProfile ::
  LoRaWANServiceProfile
newLoRaWANServiceProfile =
  LoRaWANServiceProfile'
    { addGwMetadata =
        Prelude.Nothing
    }

-- | The AddGWMetaData value.
loRaWANServiceProfile_addGwMetadata :: Lens.Lens' LoRaWANServiceProfile (Prelude.Maybe Prelude.Bool)
loRaWANServiceProfile_addGwMetadata = Lens.lens (\LoRaWANServiceProfile' {addGwMetadata} -> addGwMetadata) (\s@LoRaWANServiceProfile' {} a -> s {addGwMetadata = a} :: LoRaWANServiceProfile)

instance Prelude.Hashable LoRaWANServiceProfile where
  hashWithSalt salt' LoRaWANServiceProfile' {..} =
    salt' `Prelude.hashWithSalt` addGwMetadata

instance Prelude.NFData LoRaWANServiceProfile where
  rnf LoRaWANServiceProfile' {..} =
    Prelude.rnf addGwMetadata

instance Core.ToJSON LoRaWANServiceProfile where
  toJSON LoRaWANServiceProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AddGwMetadata" Core..=)
              Prelude.<$> addGwMetadata
          ]
      )
