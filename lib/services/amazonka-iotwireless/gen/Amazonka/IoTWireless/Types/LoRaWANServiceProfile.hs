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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANServiceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | LoRaWANServiceProfile object.
--
-- /See:/ 'newLoRaWANServiceProfile' smart constructor.
data LoRaWANServiceProfile = LoRaWANServiceProfile'
  { -- | The AddGWMetaData value.
    addGwMetadata :: Prelude.Maybe Prelude.Bool,
    -- | The DrMax value.
    drMax :: Prelude.Maybe Prelude.Natural,
    -- | The DrMin value.
    drMin :: Prelude.Maybe Prelude.Natural
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
--
-- 'drMax', 'loRaWANServiceProfile_drMax' - The DrMax value.
--
-- 'drMin', 'loRaWANServiceProfile_drMin' - The DrMin value.
newLoRaWANServiceProfile ::
  LoRaWANServiceProfile
newLoRaWANServiceProfile =
  LoRaWANServiceProfile'
    { addGwMetadata =
        Prelude.Nothing,
      drMax = Prelude.Nothing,
      drMin = Prelude.Nothing
    }

-- | The AddGWMetaData value.
loRaWANServiceProfile_addGwMetadata :: Lens.Lens' LoRaWANServiceProfile (Prelude.Maybe Prelude.Bool)
loRaWANServiceProfile_addGwMetadata = Lens.lens (\LoRaWANServiceProfile' {addGwMetadata} -> addGwMetadata) (\s@LoRaWANServiceProfile' {} a -> s {addGwMetadata = a} :: LoRaWANServiceProfile)

-- | The DrMax value.
loRaWANServiceProfile_drMax :: Lens.Lens' LoRaWANServiceProfile (Prelude.Maybe Prelude.Natural)
loRaWANServiceProfile_drMax = Lens.lens (\LoRaWANServiceProfile' {drMax} -> drMax) (\s@LoRaWANServiceProfile' {} a -> s {drMax = a} :: LoRaWANServiceProfile)

-- | The DrMin value.
loRaWANServiceProfile_drMin :: Lens.Lens' LoRaWANServiceProfile (Prelude.Maybe Prelude.Natural)
loRaWANServiceProfile_drMin = Lens.lens (\LoRaWANServiceProfile' {drMin} -> drMin) (\s@LoRaWANServiceProfile' {} a -> s {drMin = a} :: LoRaWANServiceProfile)

instance Prelude.Hashable LoRaWANServiceProfile where
  hashWithSalt _salt LoRaWANServiceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` addGwMetadata
      `Prelude.hashWithSalt` drMax
      `Prelude.hashWithSalt` drMin

instance Prelude.NFData LoRaWANServiceProfile where
  rnf LoRaWANServiceProfile' {..} =
    Prelude.rnf addGwMetadata `Prelude.seq`
      Prelude.rnf drMax `Prelude.seq`
        Prelude.rnf drMin

instance Data.ToJSON LoRaWANServiceProfile where
  toJSON LoRaWANServiceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddGwMetadata" Data..=) Prelude.<$> addGwMetadata,
            ("DrMax" Data..=) Prelude.<$> drMax,
            ("DrMin" Data..=) Prelude.<$> drMin
          ]
      )
