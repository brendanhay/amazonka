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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableReplicaGlobalSecondaryIndex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableReplicaGlobalSecondaryIndex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughputOverride

-- | Information about a global secondary index for a DynamoDB table replica.
--
-- /See:/ 'newAwsDynamoDbTableReplicaGlobalSecondaryIndex' smart constructor.
data AwsDynamoDbTableReplicaGlobalSecondaryIndex = AwsDynamoDbTableReplicaGlobalSecondaryIndex'
  { -- | Replica-specific configuration for the provisioned throughput for the
    -- index.
    provisionedThroughputOverride :: Prelude.Maybe AwsDynamoDbTableProvisionedThroughputOverride,
    -- | The name of the index.
    indexName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableReplicaGlobalSecondaryIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughputOverride', 'awsDynamoDbTableReplicaGlobalSecondaryIndex_provisionedThroughputOverride' - Replica-specific configuration for the provisioned throughput for the
-- index.
--
-- 'indexName', 'awsDynamoDbTableReplicaGlobalSecondaryIndex_indexName' - The name of the index.
newAwsDynamoDbTableReplicaGlobalSecondaryIndex ::
  AwsDynamoDbTableReplicaGlobalSecondaryIndex
newAwsDynamoDbTableReplicaGlobalSecondaryIndex =
  AwsDynamoDbTableReplicaGlobalSecondaryIndex'
    { provisionedThroughputOverride =
        Prelude.Nothing,
      indexName = Prelude.Nothing
    }

-- | Replica-specific configuration for the provisioned throughput for the
-- index.
awsDynamoDbTableReplicaGlobalSecondaryIndex_provisionedThroughputOverride :: Lens.Lens' AwsDynamoDbTableReplicaGlobalSecondaryIndex (Prelude.Maybe AwsDynamoDbTableProvisionedThroughputOverride)
awsDynamoDbTableReplicaGlobalSecondaryIndex_provisionedThroughputOverride = Lens.lens (\AwsDynamoDbTableReplicaGlobalSecondaryIndex' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@AwsDynamoDbTableReplicaGlobalSecondaryIndex' {} a -> s {provisionedThroughputOverride = a} :: AwsDynamoDbTableReplicaGlobalSecondaryIndex)

-- | The name of the index.
awsDynamoDbTableReplicaGlobalSecondaryIndex_indexName :: Lens.Lens' AwsDynamoDbTableReplicaGlobalSecondaryIndex (Prelude.Maybe Prelude.Text)
awsDynamoDbTableReplicaGlobalSecondaryIndex_indexName = Lens.lens (\AwsDynamoDbTableReplicaGlobalSecondaryIndex' {indexName} -> indexName) (\s@AwsDynamoDbTableReplicaGlobalSecondaryIndex' {} a -> s {indexName = a} :: AwsDynamoDbTableReplicaGlobalSecondaryIndex)

instance
  Core.FromJSON
    AwsDynamoDbTableReplicaGlobalSecondaryIndex
  where
  parseJSON =
    Core.withObject
      "AwsDynamoDbTableReplicaGlobalSecondaryIndex"
      ( \x ->
          AwsDynamoDbTableReplicaGlobalSecondaryIndex'
            Prelude.<$> (x Core..:? "ProvisionedThroughputOverride")
              Prelude.<*> (x Core..:? "IndexName")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableReplicaGlobalSecondaryIndex
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableReplicaGlobalSecondaryIndex' {..} =
      _salt
        `Prelude.hashWithSalt` provisionedThroughputOverride
        `Prelude.hashWithSalt` indexName

instance
  Prelude.NFData
    AwsDynamoDbTableReplicaGlobalSecondaryIndex
  where
  rnf AwsDynamoDbTableReplicaGlobalSecondaryIndex' {..} =
    Prelude.rnf provisionedThroughputOverride
      `Prelude.seq` Prelude.rnf indexName

instance
  Core.ToJSON
    AwsDynamoDbTableReplicaGlobalSecondaryIndex
  where
  toJSON
    AwsDynamoDbTableReplicaGlobalSecondaryIndex' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ProvisionedThroughputOverride" Core..=)
                Prelude.<$> provisionedThroughputOverride,
              ("IndexName" Core..=) Prelude.<$> indexName
            ]
        )
