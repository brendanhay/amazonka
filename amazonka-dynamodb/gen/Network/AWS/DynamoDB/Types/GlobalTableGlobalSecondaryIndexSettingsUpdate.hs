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

-- | Represents the settings of a global secondary index for a global table
-- that will be modified.
--
-- /See:/ 'newGlobalTableGlobalSecondaryIndexSettingsUpdate' smart constructor.
data GlobalTableGlobalSecondaryIndexSettingsUpdate = GlobalTableGlobalSecondaryIndexSettingsUpdate'
  { -- | Auto scaling settings for managing a global secondary index\'s write
    -- capacity units.
    provisionedWriteCapacityAutoScalingSettingsUpdate :: Core.Maybe AutoScalingSettingsUpdate,
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException.@
    provisionedWriteCapacityUnits :: Core.Maybe Core.Natural,
    -- | The name of the global secondary index. The name must be unique among
    -- all other indexes on this table.
    indexName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GlobalTableGlobalSecondaryIndexSettingsUpdate
newGlobalTableGlobalSecondaryIndexSettingsUpdate
  pIndexName_ =
    GlobalTableGlobalSecondaryIndexSettingsUpdate'
      { provisionedWriteCapacityAutoScalingSettingsUpdate =
          Core.Nothing,
        provisionedWriteCapacityUnits =
          Core.Nothing,
        indexName = pIndexName_
      }

-- | Auto scaling settings for managing a global secondary index\'s write
-- capacity units.
globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Core.Maybe AutoScalingSettingsUpdate)
globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate = Lens.lens (\GlobalTableGlobalSecondaryIndexSettingsUpdate' {provisionedWriteCapacityAutoScalingSettingsUpdate} -> provisionedWriteCapacityAutoScalingSettingsUpdate) (\s@GlobalTableGlobalSecondaryIndexSettingsUpdate' {} a -> s {provisionedWriteCapacityAutoScalingSettingsUpdate = a} :: GlobalTableGlobalSecondaryIndexSettingsUpdate)

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException.@
globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Core.Maybe Core.Natural)
globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits = Lens.lens (\GlobalTableGlobalSecondaryIndexSettingsUpdate' {provisionedWriteCapacityUnits} -> provisionedWriteCapacityUnits) (\s@GlobalTableGlobalSecondaryIndexSettingsUpdate' {} a -> s {provisionedWriteCapacityUnits = a} :: GlobalTableGlobalSecondaryIndexSettingsUpdate)

-- | The name of the global secondary index. The name must be unique among
-- all other indexes on this table.
globalTableGlobalSecondaryIndexSettingsUpdate_indexName :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate Core.Text
globalTableGlobalSecondaryIndexSettingsUpdate_indexName = Lens.lens (\GlobalTableGlobalSecondaryIndexSettingsUpdate' {indexName} -> indexName) (\s@GlobalTableGlobalSecondaryIndexSettingsUpdate' {} a -> s {indexName = a} :: GlobalTableGlobalSecondaryIndexSettingsUpdate)

instance
  Core.Hashable
    GlobalTableGlobalSecondaryIndexSettingsUpdate

instance
  Core.NFData
    GlobalTableGlobalSecondaryIndexSettingsUpdate

instance
  Core.ToJSON
    GlobalTableGlobalSecondaryIndexSettingsUpdate
  where
  toJSON
    GlobalTableGlobalSecondaryIndexSettingsUpdate' {..} =
      Core.object
        ( Core.catMaybes
            [ ( "ProvisionedWriteCapacityAutoScalingSettingsUpdate"
                  Core..=
              )
                Core.<$> provisionedWriteCapacityAutoScalingSettingsUpdate,
              ("ProvisionedWriteCapacityUnits" Core..=)
                Core.<$> provisionedWriteCapacityUnits,
              Core.Just ("IndexName" Core..= indexName)
            ]
        )
