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
-- Module      : Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ProvisionedThroughputOverride
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a replica global secondary index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndex' smart constructor.
data ReplicaGlobalSecondaryIndex = ReplicaGlobalSecondaryIndex'
  { -- | Replica table GSI-specific provisioned throughput. If not specified,
    -- uses the source table GSI\'s read capacity settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | The name of the global secondary index.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaGlobalSecondaryIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughputOverride', 'replicaGlobalSecondaryIndex_provisionedThroughputOverride' - Replica table GSI-specific provisioned throughput. If not specified,
-- uses the source table GSI\'s read capacity settings.
--
-- 'indexName', 'replicaGlobalSecondaryIndex_indexName' - The name of the global secondary index.
newReplicaGlobalSecondaryIndex ::
  -- | 'indexName'
  Prelude.Text ->
  ReplicaGlobalSecondaryIndex
newReplicaGlobalSecondaryIndex pIndexName_ =
  ReplicaGlobalSecondaryIndex'
    { provisionedThroughputOverride =
        Prelude.Nothing,
      indexName = pIndexName_
    }

-- | Replica table GSI-specific provisioned throughput. If not specified,
-- uses the source table GSI\'s read capacity settings.
replicaGlobalSecondaryIndex_provisionedThroughputOverride :: Lens.Lens' ReplicaGlobalSecondaryIndex (Prelude.Maybe ProvisionedThroughputOverride)
replicaGlobalSecondaryIndex_provisionedThroughputOverride = Lens.lens (\ReplicaGlobalSecondaryIndex' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@ReplicaGlobalSecondaryIndex' {} a -> s {provisionedThroughputOverride = a} :: ReplicaGlobalSecondaryIndex)

-- | The name of the global secondary index.
replicaGlobalSecondaryIndex_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndex Prelude.Text
replicaGlobalSecondaryIndex_indexName = Lens.lens (\ReplicaGlobalSecondaryIndex' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndex' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndex)

instance Prelude.Hashable ReplicaGlobalSecondaryIndex where
  hashWithSalt _salt ReplicaGlobalSecondaryIndex' {..} =
    _salt
      `Prelude.hashWithSalt` provisionedThroughputOverride
      `Prelude.hashWithSalt` indexName

instance Prelude.NFData ReplicaGlobalSecondaryIndex where
  rnf ReplicaGlobalSecondaryIndex' {..} =
    Prelude.rnf provisionedThroughputOverride `Prelude.seq`
      Prelude.rnf indexName

instance Data.ToJSON ReplicaGlobalSecondaryIndex where
  toJSON ReplicaGlobalSecondaryIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ProvisionedThroughputOverride" Data..=)
              Prelude.<$> provisionedThroughputOverride,
            Prelude.Just ("IndexName" Data..= indexName)
          ]
      )
