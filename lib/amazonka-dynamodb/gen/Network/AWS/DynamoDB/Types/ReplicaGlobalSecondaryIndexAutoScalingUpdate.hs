{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
  ( ReplicaGlobalSecondaryIndexAutoScalingUpdate (..),

    -- * Smart constructor
    mkReplicaGlobalSecondaryIndexAutoScalingUpdate,

    -- * Lenses
    rgsiasuProvisionedReadCapacityAutoScalingUpdate,
    rgsiasuIndexName,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the auto scaling settings of a global secondary index for a replica that will be modified.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexAutoScalingUpdate' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingUpdate = ReplicaGlobalSecondaryIndexAutoScalingUpdate'
  { provisionedReadCapacityAutoScalingUpdate :: Lude.Maybe AutoScalingSettingsUpdate,
    -- | The name of the global secondary index.
    indexName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaGlobalSecondaryIndexAutoScalingUpdate' with the minimum fields required to make a request.
--
-- * 'provisionedReadCapacityAutoScalingUpdate' -
-- * 'indexName' - The name of the global secondary index.
mkReplicaGlobalSecondaryIndexAutoScalingUpdate ::
  ReplicaGlobalSecondaryIndexAutoScalingUpdate
mkReplicaGlobalSecondaryIndexAutoScalingUpdate =
  ReplicaGlobalSecondaryIndexAutoScalingUpdate'
    { provisionedReadCapacityAutoScalingUpdate =
        Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedReadCapacityAutoScalingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasuProvisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Lude.Maybe AutoScalingSettingsUpdate)
rgsiasuProvisionedReadCapacityAutoScalingUpdate = Lens.lens (provisionedReadCapacityAutoScalingUpdate :: ReplicaGlobalSecondaryIndexAutoScalingUpdate -> Lude.Maybe AutoScalingSettingsUpdate) (\s a -> s {provisionedReadCapacityAutoScalingUpdate = a} :: ReplicaGlobalSecondaryIndexAutoScalingUpdate)
{-# DEPRECATED rgsiasuProvisionedReadCapacityAutoScalingUpdate "Use generic-lens or generic-optics with 'provisionedReadCapacityAutoScalingUpdate' instead." #-}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasuIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Lude.Maybe Lude.Text)
rgsiasuIndexName = Lens.lens (indexName :: ReplicaGlobalSecondaryIndexAutoScalingUpdate -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexAutoScalingUpdate)
{-# DEPRECATED rgsiasuIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.ToJSON ReplicaGlobalSecondaryIndexAutoScalingUpdate where
  toJSON ReplicaGlobalSecondaryIndexAutoScalingUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedReadCapacityAutoScalingUpdate" Lude..=)
              Lude.<$> provisionedReadCapacityAutoScalingUpdate,
            ("IndexName" Lude..=) Lude.<$> indexName
          ]
      )
