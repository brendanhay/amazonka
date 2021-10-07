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
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the settings of a global secondary index for a global table
-- that will be modified.
--
-- /See:/ 'newGlobalTableGlobalSecondaryIndexSettingsUpdate' smart constructor.
data GlobalTableGlobalSecondaryIndexSettingsUpdate = GlobalTableGlobalSecondaryIndexSettingsUpdate'
  { -- | Auto scaling settings for managing a global secondary index\'s write
    -- capacity units.
    provisionedWriteCapacityAutoScalingSettingsUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException.@
    provisionedWriteCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The name of the global secondary index. The name must be unique among
    -- all other indexes on this table.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalTableGlobalSecondaryIndexSettingsUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedWriteCapacityAutoScalingSettingsUpdate', 'globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing a global secondary index\'s write
-- capacity units.
--
-- 'provisionedWriteCapacityUnits', 'globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException.@
--
-- 'indexName', 'globalTableGlobalSecondaryIndexSettingsUpdate_indexName' - The name of the global secondary index. The name must be unique among
-- all other indexes on this table.
newGlobalTableGlobalSecondaryIndexSettingsUpdate ::
  -- | 'indexName'
  Prelude.Text ->
  GlobalTableGlobalSecondaryIndexSettingsUpdate
newGlobalTableGlobalSecondaryIndexSettingsUpdate
  pIndexName_ =
    GlobalTableGlobalSecondaryIndexSettingsUpdate'
      { provisionedWriteCapacityAutoScalingSettingsUpdate =
          Prelude.Nothing,
        provisionedWriteCapacityUnits =
          Prelude.Nothing,
        indexName = pIndexName_
      }

-- | Auto scaling settings for managing a global secondary index\'s write
-- capacity units.
globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Prelude.Maybe AutoScalingSettingsUpdate)
globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate = Lens.lens (\GlobalTableGlobalSecondaryIndexSettingsUpdate' {provisionedWriteCapacityAutoScalingSettingsUpdate} -> provisionedWriteCapacityAutoScalingSettingsUpdate) (\s@GlobalTableGlobalSecondaryIndexSettingsUpdate' {} a -> s {provisionedWriteCapacityAutoScalingSettingsUpdate = a} :: GlobalTableGlobalSecondaryIndexSettingsUpdate)

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException.@
globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Prelude.Maybe Prelude.Natural)
globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits = Lens.lens (\GlobalTableGlobalSecondaryIndexSettingsUpdate' {provisionedWriteCapacityUnits} -> provisionedWriteCapacityUnits) (\s@GlobalTableGlobalSecondaryIndexSettingsUpdate' {} a -> s {provisionedWriteCapacityUnits = a} :: GlobalTableGlobalSecondaryIndexSettingsUpdate)

-- | The name of the global secondary index. The name must be unique among
-- all other indexes on this table.
globalTableGlobalSecondaryIndexSettingsUpdate_indexName :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate Prelude.Text
globalTableGlobalSecondaryIndexSettingsUpdate_indexName = Lens.lens (\GlobalTableGlobalSecondaryIndexSettingsUpdate' {indexName} -> indexName) (\s@GlobalTableGlobalSecondaryIndexSettingsUpdate' {} a -> s {indexName = a} :: GlobalTableGlobalSecondaryIndexSettingsUpdate)

instance
  Prelude.Hashable
    GlobalTableGlobalSecondaryIndexSettingsUpdate

instance
  Prelude.NFData
    GlobalTableGlobalSecondaryIndexSettingsUpdate

instance
  Core.ToJSON
    GlobalTableGlobalSecondaryIndexSettingsUpdate
  where
  toJSON
    GlobalTableGlobalSecondaryIndexSettingsUpdate' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ( "ProvisionedWriteCapacityAutoScalingSettingsUpdate"
                  Core..=
              )
                Prelude.<$> provisionedWriteCapacityAutoScalingSettingsUpdate,
              ("ProvisionedWriteCapacityUnits" Core..=)
                Prelude.<$> provisionedWriteCapacityUnits,
              Prelude.Just ("IndexName" Core..= indexName)
            ]
        )
