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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the auto scaling settings of a global secondary index for a
-- replica that will be modified.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexAutoScalingUpdate' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingUpdate = ReplicaGlobalSecondaryIndexAutoScalingUpdate'
  { -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text,
    provisionedReadCapacityAutoScalingUpdate :: Prelude.Maybe AutoScalingSettingsUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      provisionedReadCapacityAutoScalingUpdate =
        Prelude.Nothing
    }

-- | The name of the global secondary index.
replicaGlobalSecondaryIndexAutoScalingUpdate_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Prelude.Maybe Prelude.Text)
replicaGlobalSecondaryIndexAutoScalingUpdate_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingUpdate' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexAutoScalingUpdate' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexAutoScalingUpdate)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Prelude.Maybe AutoScalingSettingsUpdate)
replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingUpdate' {provisionedReadCapacityAutoScalingUpdate} -> provisionedReadCapacityAutoScalingUpdate) (\s@ReplicaGlobalSecondaryIndexAutoScalingUpdate' {} a -> s {provisionedReadCapacityAutoScalingUpdate = a} :: ReplicaGlobalSecondaryIndexAutoScalingUpdate)

instance
  Prelude.Hashable
    ReplicaGlobalSecondaryIndexAutoScalingUpdate

instance
  Prelude.NFData
    ReplicaGlobalSecondaryIndexAutoScalingUpdate

instance
  Prelude.ToJSON
    ReplicaGlobalSecondaryIndexAutoScalingUpdate
  where
  toJSON
    ReplicaGlobalSecondaryIndexAutoScalingUpdate' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("IndexName" Prelude..=) Prelude.<$> indexName,
              ( "ProvisionedReadCapacityAutoScalingUpdate"
                  Prelude..=
              )
                Prelude.<$> provisionedReadCapacityAutoScalingUpdate
            ]
        )
