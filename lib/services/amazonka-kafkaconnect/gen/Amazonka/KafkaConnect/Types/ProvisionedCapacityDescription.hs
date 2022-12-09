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
-- Module      : Amazonka.KafkaConnect.Types.ProvisionedCapacityDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.ProvisionedCapacityDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The description of a connector\'s provisioned capacity.
--
-- /See:/ 'newProvisionedCapacityDescription' smart constructor.
data ProvisionedCapacityDescription = ProvisionedCapacityDescription'
  { -- | The number of microcontroller units (MCUs) allocated to each connector
    -- worker. The valid values are 1,2,4,8.
    mcuCount :: Prelude.Maybe Prelude.Int,
    -- | The number of workers that are allocated to the connector.
    workerCount :: Prelude.Maybe Prelude.Int
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
-- 'mcuCount', 'provisionedCapacityDescription_mcuCount' - The number of microcontroller units (MCUs) allocated to each connector
-- worker. The valid values are 1,2,4,8.
--
-- 'workerCount', 'provisionedCapacityDescription_workerCount' - The number of workers that are allocated to the connector.
newProvisionedCapacityDescription ::
  ProvisionedCapacityDescription
newProvisionedCapacityDescription =
  ProvisionedCapacityDescription'
    { mcuCount =
        Prelude.Nothing,
      workerCount = Prelude.Nothing
    }

-- | The number of microcontroller units (MCUs) allocated to each connector
-- worker. The valid values are 1,2,4,8.
provisionedCapacityDescription_mcuCount :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Int)
provisionedCapacityDescription_mcuCount = Lens.lens (\ProvisionedCapacityDescription' {mcuCount} -> mcuCount) (\s@ProvisionedCapacityDescription' {} a -> s {mcuCount = a} :: ProvisionedCapacityDescription)

-- | The number of workers that are allocated to the connector.
provisionedCapacityDescription_workerCount :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Int)
provisionedCapacityDescription_workerCount = Lens.lens (\ProvisionedCapacityDescription' {workerCount} -> workerCount) (\s@ProvisionedCapacityDescription' {} a -> s {workerCount = a} :: ProvisionedCapacityDescription)

instance Data.FromJSON ProvisionedCapacityDescription where
  parseJSON =
    Data.withObject
      "ProvisionedCapacityDescription"
      ( \x ->
          ProvisionedCapacityDescription'
            Prelude.<$> (x Data..:? "mcuCount")
            Prelude.<*> (x Data..:? "workerCount")
      )

instance
  Prelude.Hashable
    ProvisionedCapacityDescription
  where
  hashWithSalt
    _salt
    ProvisionedCapacityDescription' {..} =
      _salt `Prelude.hashWithSalt` mcuCount
        `Prelude.hashWithSalt` workerCount

instance
  Prelude.NFData
    ProvisionedCapacityDescription
  where
  rnf ProvisionedCapacityDescription' {..} =
    Prelude.rnf mcuCount
      `Prelude.seq` Prelude.rnf workerCount
