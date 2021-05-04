{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glacier.Types.ProvisionedCapacityDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ProvisionedCapacityDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The definition for a provisioned capacity unit.
--
-- /See:/ 'newProvisionedCapacityDescription' smart constructor.
data ProvisionedCapacityDescription = ProvisionedCapacityDescription'
  { -- | The date that the provisioned capacity unit was purchased, in Universal
    -- Coordinated Time (UTC).
    startDate :: Prelude.Maybe Prelude.Text,
    -- | The ID that identifies the provisioned capacity unit.
    capacityId :: Prelude.Maybe Prelude.Text,
    -- | The date that the provisioned capacity unit expires, in Universal
    -- Coordinated Time (UTC).
    expirationDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedCapacityDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDate', 'provisionedCapacityDescription_startDate' - The date that the provisioned capacity unit was purchased, in Universal
-- Coordinated Time (UTC).
--
-- 'capacityId', 'provisionedCapacityDescription_capacityId' - The ID that identifies the provisioned capacity unit.
--
-- 'expirationDate', 'provisionedCapacityDescription_expirationDate' - The date that the provisioned capacity unit expires, in Universal
-- Coordinated Time (UTC).
newProvisionedCapacityDescription ::
  ProvisionedCapacityDescription
newProvisionedCapacityDescription =
  ProvisionedCapacityDescription'
    { startDate =
        Prelude.Nothing,
      capacityId = Prelude.Nothing,
      expirationDate = Prelude.Nothing
    }

-- | The date that the provisioned capacity unit was purchased, in Universal
-- Coordinated Time (UTC).
provisionedCapacityDescription_startDate :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Text)
provisionedCapacityDescription_startDate = Lens.lens (\ProvisionedCapacityDescription' {startDate} -> startDate) (\s@ProvisionedCapacityDescription' {} a -> s {startDate = a} :: ProvisionedCapacityDescription)

-- | The ID that identifies the provisioned capacity unit.
provisionedCapacityDescription_capacityId :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Text)
provisionedCapacityDescription_capacityId = Lens.lens (\ProvisionedCapacityDescription' {capacityId} -> capacityId) (\s@ProvisionedCapacityDescription' {} a -> s {capacityId = a} :: ProvisionedCapacityDescription)

-- | The date that the provisioned capacity unit expires, in Universal
-- Coordinated Time (UTC).
provisionedCapacityDescription_expirationDate :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Text)
provisionedCapacityDescription_expirationDate = Lens.lens (\ProvisionedCapacityDescription' {expirationDate} -> expirationDate) (\s@ProvisionedCapacityDescription' {} a -> s {expirationDate = a} :: ProvisionedCapacityDescription)

instance
  Prelude.FromJSON
    ProvisionedCapacityDescription
  where
  parseJSON =
    Prelude.withObject
      "ProvisionedCapacityDescription"
      ( \x ->
          ProvisionedCapacityDescription'
            Prelude.<$> (x Prelude..:? "StartDate")
            Prelude.<*> (x Prelude..:? "CapacityId")
            Prelude.<*> (x Prelude..:? "ExpirationDate")
      )

instance
  Prelude.Hashable
    ProvisionedCapacityDescription

instance
  Prelude.NFData
    ProvisionedCapacityDescription
