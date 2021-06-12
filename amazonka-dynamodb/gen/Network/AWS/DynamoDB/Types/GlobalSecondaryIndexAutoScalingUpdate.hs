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
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import qualified Network.AWS.Lens as Lens

-- | Represents the auto scaling settings of a global secondary index for a
-- global table that will be modified.
--
-- /See:/ 'newGlobalSecondaryIndexAutoScalingUpdate' smart constructor.
data GlobalSecondaryIndexAutoScalingUpdate = GlobalSecondaryIndexAutoScalingUpdate'
  { -- | The name of the global secondary index.
    indexName :: Core.Maybe Core.Text,
    provisionedWriteCapacityAutoScalingUpdate :: Core.Maybe AutoScalingSettingsUpdate
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GlobalSecondaryIndexAutoScalingUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'globalSecondaryIndexAutoScalingUpdate_indexName' - The name of the global secondary index.
--
-- 'provisionedWriteCapacityAutoScalingUpdate', 'globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate' - Undocumented member.
newGlobalSecondaryIndexAutoScalingUpdate ::
  GlobalSecondaryIndexAutoScalingUpdate
newGlobalSecondaryIndexAutoScalingUpdate =
  GlobalSecondaryIndexAutoScalingUpdate'
    { indexName =
        Core.Nothing,
      provisionedWriteCapacityAutoScalingUpdate =
        Core.Nothing
    }

-- | The name of the global secondary index.
globalSecondaryIndexAutoScalingUpdate_indexName :: Lens.Lens' GlobalSecondaryIndexAutoScalingUpdate (Core.Maybe Core.Text)
globalSecondaryIndexAutoScalingUpdate_indexName = Lens.lens (\GlobalSecondaryIndexAutoScalingUpdate' {indexName} -> indexName) (\s@GlobalSecondaryIndexAutoScalingUpdate' {} a -> s {indexName = a} :: GlobalSecondaryIndexAutoScalingUpdate)

-- | Undocumented member.
globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate :: Lens.Lens' GlobalSecondaryIndexAutoScalingUpdate (Core.Maybe AutoScalingSettingsUpdate)
globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate = Lens.lens (\GlobalSecondaryIndexAutoScalingUpdate' {provisionedWriteCapacityAutoScalingUpdate} -> provisionedWriteCapacityAutoScalingUpdate) (\s@GlobalSecondaryIndexAutoScalingUpdate' {} a -> s {provisionedWriteCapacityAutoScalingUpdate = a} :: GlobalSecondaryIndexAutoScalingUpdate)

instance
  Core.Hashable
    GlobalSecondaryIndexAutoScalingUpdate

instance
  Core.NFData
    GlobalSecondaryIndexAutoScalingUpdate

instance
  Core.ToJSON
    GlobalSecondaryIndexAutoScalingUpdate
  where
  toJSON GlobalSecondaryIndexAutoScalingUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IndexName" Core..=) Core.<$> indexName,
            ("ProvisionedWriteCapacityAutoScalingUpdate" Core..=)
              Core.<$> provisionedWriteCapacityAutoScalingUpdate
          ]
      )
