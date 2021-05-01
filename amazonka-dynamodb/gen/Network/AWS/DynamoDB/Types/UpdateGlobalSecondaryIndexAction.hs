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
-- Module      : Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction where

import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the new provisioned throughput settings to be applied to a
-- global secondary index.
--
-- /See:/ 'newUpdateGlobalSecondaryIndexAction' smart constructor.
data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction'
  { -- | The name of the global secondary index to be updated.
    indexName :: Prelude.Text,
    -- | Represents the provisioned throughput settings for the specified global
    -- secondary index.
    --
    -- For current minimum and maximum provisioned throughput values, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
    -- in the /Amazon DynamoDB Developer Guide/.
    provisionedThroughput :: ProvisionedThroughput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalSecondaryIndexAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'updateGlobalSecondaryIndexAction_indexName' - The name of the global secondary index to be updated.
--
-- 'provisionedThroughput', 'updateGlobalSecondaryIndexAction_provisionedThroughput' - Represents the provisioned throughput settings for the specified global
-- secondary index.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
newUpdateGlobalSecondaryIndexAction ::
  -- | 'indexName'
  Prelude.Text ->
  -- | 'provisionedThroughput'
  ProvisionedThroughput ->
  UpdateGlobalSecondaryIndexAction
newUpdateGlobalSecondaryIndexAction
  pIndexName_
  pProvisionedThroughput_ =
    UpdateGlobalSecondaryIndexAction'
      { indexName =
          pIndexName_,
        provisionedThroughput =
          pProvisionedThroughput_
      }

-- | The name of the global secondary index to be updated.
updateGlobalSecondaryIndexAction_indexName :: Lens.Lens' UpdateGlobalSecondaryIndexAction Prelude.Text
updateGlobalSecondaryIndexAction_indexName = Lens.lens (\UpdateGlobalSecondaryIndexAction' {indexName} -> indexName) (\s@UpdateGlobalSecondaryIndexAction' {} a -> s {indexName = a} :: UpdateGlobalSecondaryIndexAction)

-- | Represents the provisioned throughput settings for the specified global
-- secondary index.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
updateGlobalSecondaryIndexAction_provisionedThroughput :: Lens.Lens' UpdateGlobalSecondaryIndexAction ProvisionedThroughput
updateGlobalSecondaryIndexAction_provisionedThroughput = Lens.lens (\UpdateGlobalSecondaryIndexAction' {provisionedThroughput} -> provisionedThroughput) (\s@UpdateGlobalSecondaryIndexAction' {} a -> s {provisionedThroughput = a} :: UpdateGlobalSecondaryIndexAction)

instance
  Prelude.Hashable
    UpdateGlobalSecondaryIndexAction

instance
  Prelude.NFData
    UpdateGlobalSecondaryIndexAction

instance
  Prelude.ToJSON
    UpdateGlobalSecondaryIndexAction
  where
  toJSON UpdateGlobalSecondaryIndexAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexName" Prelude..= indexName),
            Prelude.Just
              ( "ProvisionedThroughput"
                  Prelude..= provisionedThroughput
              )
          ]
      )
