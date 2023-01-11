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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughputOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughputOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Replica-specific configuration for the provisioned throughput.
--
-- /See:/ 'newAwsDynamoDbTableProvisionedThroughputOverride' smart constructor.
data AwsDynamoDbTableProvisionedThroughputOverride = AwsDynamoDbTableProvisionedThroughputOverride'
  { -- | The read capacity units for the replica.
    readCapacityUnits :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableProvisionedThroughputOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readCapacityUnits', 'awsDynamoDbTableProvisionedThroughputOverride_readCapacityUnits' - The read capacity units for the replica.
newAwsDynamoDbTableProvisionedThroughputOverride ::
  AwsDynamoDbTableProvisionedThroughputOverride
newAwsDynamoDbTableProvisionedThroughputOverride =
  AwsDynamoDbTableProvisionedThroughputOverride'
    { readCapacityUnits =
        Prelude.Nothing
    }

-- | The read capacity units for the replica.
awsDynamoDbTableProvisionedThroughputOverride_readCapacityUnits :: Lens.Lens' AwsDynamoDbTableProvisionedThroughputOverride (Prelude.Maybe Prelude.Int)
awsDynamoDbTableProvisionedThroughputOverride_readCapacityUnits = Lens.lens (\AwsDynamoDbTableProvisionedThroughputOverride' {readCapacityUnits} -> readCapacityUnits) (\s@AwsDynamoDbTableProvisionedThroughputOverride' {} a -> s {readCapacityUnits = a} :: AwsDynamoDbTableProvisionedThroughputOverride)

instance
  Data.FromJSON
    AwsDynamoDbTableProvisionedThroughputOverride
  where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableProvisionedThroughputOverride"
      ( \x ->
          AwsDynamoDbTableProvisionedThroughputOverride'
            Prelude.<$> (x Data..:? "ReadCapacityUnits")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableProvisionedThroughputOverride
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableProvisionedThroughputOverride' {..} =
      _salt `Prelude.hashWithSalt` readCapacityUnits

instance
  Prelude.NFData
    AwsDynamoDbTableProvisionedThroughputOverride
  where
  rnf
    AwsDynamoDbTableProvisionedThroughputOverride' {..} =
      Prelude.rnf readCapacityUnits

instance
  Data.ToJSON
    AwsDynamoDbTableProvisionedThroughputOverride
  where
  toJSON
    AwsDynamoDbTableProvisionedThroughputOverride' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("ReadCapacityUnits" Data..=)
                Prelude.<$> readCapacityUnits
            ]
        )
