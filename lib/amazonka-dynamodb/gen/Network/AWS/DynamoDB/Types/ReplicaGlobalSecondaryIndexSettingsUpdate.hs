{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
  ( ReplicaGlobalSecondaryIndexSettingsUpdate (..),

    -- * Smart constructor
    mkReplicaGlobalSecondaryIndexSettingsUpdate,

    -- * Lenses
    rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate,
    rgsisuProvisionedReadCapacityUnits,
    rgsisuIndexName,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexSettingsUpdate' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsUpdate = ReplicaGlobalSecondaryIndexSettingsUpdate'
  { provisionedReadCapacityAutoScalingSettingsUpdate ::
      Lude.Maybe
        AutoScalingSettingsUpdate,
    provisionedReadCapacityUnits ::
      Lude.Maybe
        Lude.Natural,
    indexName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaGlobalSecondaryIndexSettingsUpdate' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
-- * 'provisionedReadCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing a global secondary index replica's read capacity units.
-- * 'provisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
mkReplicaGlobalSecondaryIndexSettingsUpdate ::
  -- | 'indexName'
  Lude.Text ->
  ReplicaGlobalSecondaryIndexSettingsUpdate
mkReplicaGlobalSecondaryIndexSettingsUpdate pIndexName_ =
  ReplicaGlobalSecondaryIndexSettingsUpdate'
    { provisionedReadCapacityAutoScalingSettingsUpdate =
        Lude.Nothing,
      provisionedReadCapacityUnits = Lude.Nothing,
      indexName = pIndexName_
    }

-- | Auto scaling settings for managing a global secondary index replica's read capacity units.
--
-- /Note:/ Consider using 'provisionedReadCapacityAutoScalingSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsUpdate (Lude.Maybe AutoScalingSettingsUpdate)
rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate = Lens.lens (provisionedReadCapacityAutoScalingSettingsUpdate :: ReplicaGlobalSecondaryIndexSettingsUpdate -> Lude.Maybe AutoScalingSettingsUpdate) (\s a -> s {provisionedReadCapacityAutoScalingSettingsUpdate = a} :: ReplicaGlobalSecondaryIndexSettingsUpdate)
{-# DEPRECATED rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate "Use generic-lens or generic-optics with 'provisionedReadCapacityAutoScalingSettingsUpdate' instead." #-}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- /Note:/ Consider using 'provisionedReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisuProvisionedReadCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsUpdate (Lude.Maybe Lude.Natural)
rgsisuProvisionedReadCapacityUnits = Lens.lens (provisionedReadCapacityUnits :: ReplicaGlobalSecondaryIndexSettingsUpdate -> Lude.Maybe Lude.Natural) (\s a -> s {provisionedReadCapacityUnits = a} :: ReplicaGlobalSecondaryIndexSettingsUpdate)
{-# DEPRECATED rgsisuProvisionedReadCapacityUnits "Use generic-lens or generic-optics with 'provisionedReadCapacityUnits' instead." #-}

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisuIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsUpdate Lude.Text
rgsisuIndexName = Lens.lens (indexName :: ReplicaGlobalSecondaryIndexSettingsUpdate -> Lude.Text) (\s a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexSettingsUpdate)
{-# DEPRECATED rgsisuIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.ToJSON ReplicaGlobalSecondaryIndexSettingsUpdate where
  toJSON ReplicaGlobalSecondaryIndexSettingsUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedReadCapacityAutoScalingSettingsUpdate" Lude..=)
              Lude.<$> provisionedReadCapacityAutoScalingSettingsUpdate,
            ("ProvisionedReadCapacityUnits" Lude..=)
              Lude.<$> provisionedReadCapacityUnits,
            Lude.Just ("IndexName" Lude..= indexName)
          ]
      )
