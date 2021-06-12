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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import qualified Network.AWS.Lens as Lens

-- | Represents the auto scaling settings of a global secondary index for a
-- replica that will be modified.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexAutoScalingUpdate' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingUpdate = ReplicaGlobalSecondaryIndexAutoScalingUpdate'
  { -- | The name of the global secondary index.
    indexName :: Core.Maybe Core.Text,
    provisionedReadCapacityAutoScalingUpdate :: Core.Maybe AutoScalingSettingsUpdate
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicaGlobalSecondaryIndexAutoScalingUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'replicaGlobalSecondaryIndexAutoScalingUpdate_indexName' - The name of the global secondary index.
--
-- 'provisionedReadCapacityAutoScalingUpdate', 'replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate' - Undocumented member.
newReplicaGlobalSecondaryIndexAutoScalingUpdate ::
  ReplicaGlobalSecondaryIndexAutoScalingUpdate
newReplicaGlobalSecondaryIndexAutoScalingUpdate =
  ReplicaGlobalSecondaryIndexAutoScalingUpdate'
    { indexName =
        Core.Nothing,
      provisionedReadCapacityAutoScalingUpdate =
        Core.Nothing
    }

-- | The name of the global secondary index.
replicaGlobalSecondaryIndexAutoScalingUpdate_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Core.Maybe Core.Text)
replicaGlobalSecondaryIndexAutoScalingUpdate_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingUpdate' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexAutoScalingUpdate' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexAutoScalingUpdate)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Core.Maybe AutoScalingSettingsUpdate)
replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingUpdate' {provisionedReadCapacityAutoScalingUpdate} -> provisionedReadCapacityAutoScalingUpdate) (\s@ReplicaGlobalSecondaryIndexAutoScalingUpdate' {} a -> s {provisionedReadCapacityAutoScalingUpdate = a} :: ReplicaGlobalSecondaryIndexAutoScalingUpdate)

instance
  Core.Hashable
    ReplicaGlobalSecondaryIndexAutoScalingUpdate

instance
  Core.NFData
    ReplicaGlobalSecondaryIndexAutoScalingUpdate

instance
  Core.ToJSON
    ReplicaGlobalSecondaryIndexAutoScalingUpdate
  where
  toJSON
    ReplicaGlobalSecondaryIndexAutoScalingUpdate' {..} =
      Core.object
        ( Core.catMaybes
            [ ("IndexName" Core..=) Core.<$> indexName,
              ("ProvisionedReadCapacityAutoScalingUpdate" Core..=)
                Core.<$> provisionedReadCapacityAutoScalingUpdate
            ]
        )
