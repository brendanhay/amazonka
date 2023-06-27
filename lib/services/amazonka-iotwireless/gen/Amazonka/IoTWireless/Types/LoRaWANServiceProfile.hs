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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    drMin :: Prelude.Maybe Prelude.Natural,
    -- | The PRAllowed value that describes whether passive roaming is allowed.
    prAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The RAAllowed value that describes whether roaming activation is
    -- allowed.
    raAllowed :: Prelude.Maybe Prelude.Bool
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
--
-- 'prAllowed', 'loRaWANServiceProfile_prAllowed' - The PRAllowed value that describes whether passive roaming is allowed.
--
-- 'raAllowed', 'loRaWANServiceProfile_raAllowed' - The RAAllowed value that describes whether roaming activation is
-- allowed.
newLoRaWANServiceProfile ::
  LoRaWANServiceProfile
newLoRaWANServiceProfile =
  LoRaWANServiceProfile'
    { addGwMetadata =
        Prelude.Nothing,
      drMax = Prelude.Nothing,
      drMin = Prelude.Nothing,
      prAllowed = Prelude.Nothing,
      raAllowed = Prelude.Nothing
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

-- | The PRAllowed value that describes whether passive roaming is allowed.
loRaWANServiceProfile_prAllowed :: Lens.Lens' LoRaWANServiceProfile (Prelude.Maybe Prelude.Bool)
loRaWANServiceProfile_prAllowed = Lens.lens (\LoRaWANServiceProfile' {prAllowed} -> prAllowed) (\s@LoRaWANServiceProfile' {} a -> s {prAllowed = a} :: LoRaWANServiceProfile)

-- | The RAAllowed value that describes whether roaming activation is
-- allowed.
loRaWANServiceProfile_raAllowed :: Lens.Lens' LoRaWANServiceProfile (Prelude.Maybe Prelude.Bool)
loRaWANServiceProfile_raAllowed = Lens.lens (\LoRaWANServiceProfile' {raAllowed} -> raAllowed) (\s@LoRaWANServiceProfile' {} a -> s {raAllowed = a} :: LoRaWANServiceProfile)

instance Prelude.Hashable LoRaWANServiceProfile where
  hashWithSalt _salt LoRaWANServiceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` addGwMetadata
      `Prelude.hashWithSalt` drMax
      `Prelude.hashWithSalt` drMin
      `Prelude.hashWithSalt` prAllowed
      `Prelude.hashWithSalt` raAllowed

instance Prelude.NFData LoRaWANServiceProfile where
  rnf LoRaWANServiceProfile' {..} =
    Prelude.rnf addGwMetadata
      `Prelude.seq` Prelude.rnf drMax
      `Prelude.seq` Prelude.rnf drMin
      `Prelude.seq` Prelude.rnf prAllowed
      `Prelude.seq` Prelude.rnf raAllowed

instance Data.ToJSON LoRaWANServiceProfile where
  toJSON LoRaWANServiceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddGwMetadata" Data..=) Prelude.<$> addGwMetadata,
            ("DrMax" Data..=) Prelude.<$> drMax,
            ("DrMin" Data..=) Prelude.<$> drMin,
            ("PrAllowed" Data..=) Prelude.<$> prAllowed,
            ("RaAllowed" Data..=) Prelude.<$> raAllowed
          ]
      )
