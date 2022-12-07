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
-- Module      : Amazonka.DynamoDB.Types.CreateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.CreateReplicationGroupMemberAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ProvisionedThroughputOverride
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import Amazonka.DynamoDB.Types.TableClass
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a replica to be created.
--
-- /See:/ 'newCreateReplicationGroupMemberAction' smart constructor.
data CreateReplicationGroupMemberAction = CreateReplicationGroupMemberAction'
  { -- | The KMS key that should be used for KMS encryption in the new replica.
    -- To specify a key, use its key ID, Amazon Resource Name (ARN), alias
    -- name, or alias ARN. Note that you should only provide this parameter if
    -- the key is different from the default DynamoDB KMS key
    -- @alias\/aws\/dynamodb@.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific table class. If not specified, uses the source table\'s
    -- table class.
    tableClassOverride :: Prelude.Maybe TableClass,
    -- | Replica-specific provisioned throughput. If not specified, uses the
    -- source table\'s provisioned throughput settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex),
    -- | The Region where the new replica will be created.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationGroupMemberAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsMasterKeyId', 'createReplicationGroupMemberAction_kmsMasterKeyId' - The KMS key that should be used for KMS encryption in the new replica.
-- To specify a key, use its key ID, Amazon Resource Name (ARN), alias
-- name, or alias ARN. Note that you should only provide this parameter if
-- the key is different from the default DynamoDB KMS key
-- @alias\/aws\/dynamodb@.
--
-- 'tableClassOverride', 'createReplicationGroupMemberAction_tableClassOverride' - Replica-specific table class. If not specified, uses the source table\'s
-- table class.
--
-- 'provisionedThroughputOverride', 'createReplicationGroupMemberAction_provisionedThroughputOverride' - Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
--
-- 'globalSecondaryIndexes', 'createReplicationGroupMemberAction_globalSecondaryIndexes' - Replica-specific global secondary index settings.
--
-- 'regionName', 'createReplicationGroupMemberAction_regionName' - The Region where the new replica will be created.
newCreateReplicationGroupMemberAction ::
  -- | 'regionName'
  Prelude.Text ->
  CreateReplicationGroupMemberAction
newCreateReplicationGroupMemberAction pRegionName_ =
  CreateReplicationGroupMemberAction'
    { kmsMasterKeyId =
        Prelude.Nothing,
      tableClassOverride = Prelude.Nothing,
      provisionedThroughputOverride =
        Prelude.Nothing,
      globalSecondaryIndexes =
        Prelude.Nothing,
      regionName = pRegionName_
    }

-- | The KMS key that should be used for KMS encryption in the new replica.
-- To specify a key, use its key ID, Amazon Resource Name (ARN), alias
-- name, or alias ARN. Note that you should only provide this parameter if
-- the key is different from the default DynamoDB KMS key
-- @alias\/aws\/dynamodb@.
createReplicationGroupMemberAction_kmsMasterKeyId :: Lens.Lens' CreateReplicationGroupMemberAction (Prelude.Maybe Prelude.Text)
createReplicationGroupMemberAction_kmsMasterKeyId = Lens.lens (\CreateReplicationGroupMemberAction' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@CreateReplicationGroupMemberAction' {} a -> s {kmsMasterKeyId = a} :: CreateReplicationGroupMemberAction)

-- | Replica-specific table class. If not specified, uses the source table\'s
-- table class.
createReplicationGroupMemberAction_tableClassOverride :: Lens.Lens' CreateReplicationGroupMemberAction (Prelude.Maybe TableClass)
createReplicationGroupMemberAction_tableClassOverride = Lens.lens (\CreateReplicationGroupMemberAction' {tableClassOverride} -> tableClassOverride) (\s@CreateReplicationGroupMemberAction' {} a -> s {tableClassOverride = a} :: CreateReplicationGroupMemberAction)

-- | Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
createReplicationGroupMemberAction_provisionedThroughputOverride :: Lens.Lens' CreateReplicationGroupMemberAction (Prelude.Maybe ProvisionedThroughputOverride)
createReplicationGroupMemberAction_provisionedThroughputOverride = Lens.lens (\CreateReplicationGroupMemberAction' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@CreateReplicationGroupMemberAction' {} a -> s {provisionedThroughputOverride = a} :: CreateReplicationGroupMemberAction)

-- | Replica-specific global secondary index settings.
createReplicationGroupMemberAction_globalSecondaryIndexes :: Lens.Lens' CreateReplicationGroupMemberAction (Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex))
createReplicationGroupMemberAction_globalSecondaryIndexes = Lens.lens (\CreateReplicationGroupMemberAction' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@CreateReplicationGroupMemberAction' {} a -> s {globalSecondaryIndexes = a} :: CreateReplicationGroupMemberAction) Prelude.. Lens.mapping Lens.coerced

-- | The Region where the new replica will be created.
createReplicationGroupMemberAction_regionName :: Lens.Lens' CreateReplicationGroupMemberAction Prelude.Text
createReplicationGroupMemberAction_regionName = Lens.lens (\CreateReplicationGroupMemberAction' {regionName} -> regionName) (\s@CreateReplicationGroupMemberAction' {} a -> s {regionName = a} :: CreateReplicationGroupMemberAction)

instance
  Prelude.Hashable
    CreateReplicationGroupMemberAction
  where
  hashWithSalt
    _salt
    CreateReplicationGroupMemberAction' {..} =
      _salt `Prelude.hashWithSalt` kmsMasterKeyId
        `Prelude.hashWithSalt` tableClassOverride
        `Prelude.hashWithSalt` provisionedThroughputOverride
        `Prelude.hashWithSalt` globalSecondaryIndexes
        `Prelude.hashWithSalt` regionName

instance
  Prelude.NFData
    CreateReplicationGroupMemberAction
  where
  rnf CreateReplicationGroupMemberAction' {..} =
    Prelude.rnf kmsMasterKeyId
      `Prelude.seq` Prelude.rnf tableClassOverride
      `Prelude.seq` Prelude.rnf provisionedThroughputOverride
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf regionName

instance
  Data.ToJSON
    CreateReplicationGroupMemberAction
  where
  toJSON CreateReplicationGroupMemberAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KMSMasterKeyId" Data..=)
              Prelude.<$> kmsMasterKeyId,
            ("TableClassOverride" Data..=)
              Prelude.<$> tableClassOverride,
            ("ProvisionedThroughputOverride" Data..=)
              Prelude.<$> provisionedThroughputOverride,
            ("GlobalSecondaryIndexes" Data..=)
              Prelude.<$> globalSecondaryIndexes,
            Prelude.Just ("RegionName" Data..= regionName)
          ]
      )
