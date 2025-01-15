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
-- Module      : Amazonka.Glacier.Types.ProvisionedCapacityDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.ProvisionedCapacityDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The definition for a provisioned capacity unit.
--
-- /See:/ 'newProvisionedCapacityDescription' smart constructor.
data ProvisionedCapacityDescription = ProvisionedCapacityDescription'
  { -- | The ID that identifies the provisioned capacity unit.
    capacityId :: Prelude.Maybe Prelude.Text,
    -- | The date that the provisioned capacity unit expires, in Universal
    -- Coordinated Time (UTC).
    expirationDate :: Prelude.Maybe Prelude.Text,
    -- | The date that the provisioned capacity unit was purchased, in Universal
    -- Coordinated Time (UTC).
    startDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedCapacityDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityId', 'provisionedCapacityDescription_capacityId' - The ID that identifies the provisioned capacity unit.
--
-- 'expirationDate', 'provisionedCapacityDescription_expirationDate' - The date that the provisioned capacity unit expires, in Universal
-- Coordinated Time (UTC).
--
-- 'startDate', 'provisionedCapacityDescription_startDate' - The date that the provisioned capacity unit was purchased, in Universal
-- Coordinated Time (UTC).
newProvisionedCapacityDescription ::
  ProvisionedCapacityDescription
newProvisionedCapacityDescription =
  ProvisionedCapacityDescription'
    { capacityId =
        Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      startDate = Prelude.Nothing
    }

-- | The ID that identifies the provisioned capacity unit.
provisionedCapacityDescription_capacityId :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Text)
provisionedCapacityDescription_capacityId = Lens.lens (\ProvisionedCapacityDescription' {capacityId} -> capacityId) (\s@ProvisionedCapacityDescription' {} a -> s {capacityId = a} :: ProvisionedCapacityDescription)

-- | The date that the provisioned capacity unit expires, in Universal
-- Coordinated Time (UTC).
provisionedCapacityDescription_expirationDate :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Text)
provisionedCapacityDescription_expirationDate = Lens.lens (\ProvisionedCapacityDescription' {expirationDate} -> expirationDate) (\s@ProvisionedCapacityDescription' {} a -> s {expirationDate = a} :: ProvisionedCapacityDescription)

-- | The date that the provisioned capacity unit was purchased, in Universal
-- Coordinated Time (UTC).
provisionedCapacityDescription_startDate :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Text)
provisionedCapacityDescription_startDate = Lens.lens (\ProvisionedCapacityDescription' {startDate} -> startDate) (\s@ProvisionedCapacityDescription' {} a -> s {startDate = a} :: ProvisionedCapacityDescription)

instance Data.FromJSON ProvisionedCapacityDescription where
  parseJSON =
    Data.withObject
      "ProvisionedCapacityDescription"
      ( \x ->
          ProvisionedCapacityDescription'
            Prelude.<$> (x Data..:? "CapacityId")
            Prelude.<*> (x Data..:? "ExpirationDate")
            Prelude.<*> (x Data..:? "StartDate")
      )

instance
  Prelude.Hashable
    ProvisionedCapacityDescription
  where
  hashWithSalt
    _salt
    ProvisionedCapacityDescription' {..} =
      _salt
        `Prelude.hashWithSalt` capacityId
        `Prelude.hashWithSalt` expirationDate
        `Prelude.hashWithSalt` startDate

instance
  Prelude.NFData
    ProvisionedCapacityDescription
  where
  rnf ProvisionedCapacityDescription' {..} =
    Prelude.rnf capacityId `Prelude.seq`
      Prelude.rnf expirationDate `Prelude.seq`
        Prelude.rnf startDate
