-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
  ( ReplicaGlobalSecondaryIndexAutoScalingDescription (..),

    -- * Smart constructor
    mkReplicaGlobalSecondaryIndexAutoScalingDescription,

    -- * Lenses
    rgsiasdIndexStatus,
    rgsiasdProvisionedWriteCapacityAutoScalingSettings,
    rgsiasdProvisionedReadCapacityAutoScalingSettings,
    rgsiasdIndexName,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.IndexStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the auto scaling configuration for a replica global secondary index.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexAutoScalingDescription' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingDescription = ReplicaGlobalSecondaryIndexAutoScalingDescription'
  { indexStatus ::
      Lude.Maybe
        IndexStatus,
    provisionedWriteCapacityAutoScalingSettings ::
      Lude.Maybe
        AutoScalingSettingsDescription,
    provisionedReadCapacityAutoScalingSettings ::
      Lude.Maybe
        AutoScalingSettingsDescription,
    indexName ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ReplicaGlobalSecondaryIndexAutoScalingDescription' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index.
-- * 'indexStatus' - The current state of the replica global secondary index:
--
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
-- * 'provisionedReadCapacityAutoScalingSettings' - Undocumented field.
-- * 'provisionedWriteCapacityAutoScalingSettings' - Undocumented field.
mkReplicaGlobalSecondaryIndexAutoScalingDescription ::
  ReplicaGlobalSecondaryIndexAutoScalingDescription
mkReplicaGlobalSecondaryIndexAutoScalingDescription =
  ReplicaGlobalSecondaryIndexAutoScalingDescription'
    { indexStatus =
        Lude.Nothing,
      provisionedWriteCapacityAutoScalingSettings =
        Lude.Nothing,
      provisionedReadCapacityAutoScalingSettings =
        Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | The current state of the replica global secondary index:
--
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
--
-- /Note:/ Consider using 'indexStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasdIndexStatus :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Lude.Maybe IndexStatus)
rgsiasdIndexStatus = Lens.lens (indexStatus :: ReplicaGlobalSecondaryIndexAutoScalingDescription -> Lude.Maybe IndexStatus) (\s a -> s {indexStatus = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)
{-# DEPRECATED rgsiasdIndexStatus "Use generic-lens or generic-optics with 'indexStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasdProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Lude.Maybe AutoScalingSettingsDescription)
rgsiasdProvisionedWriteCapacityAutoScalingSettings = Lens.lens (provisionedWriteCapacityAutoScalingSettings :: ReplicaGlobalSecondaryIndexAutoScalingDescription -> Lude.Maybe AutoScalingSettingsDescription) (\s a -> s {provisionedWriteCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)
{-# DEPRECATED rgsiasdProvisionedWriteCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedReadCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasdProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Lude.Maybe AutoScalingSettingsDescription)
rgsiasdProvisionedReadCapacityAutoScalingSettings = Lens.lens (provisionedReadCapacityAutoScalingSettings :: ReplicaGlobalSecondaryIndexAutoScalingDescription -> Lude.Maybe AutoScalingSettingsDescription) (\s a -> s {provisionedReadCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)
{-# DEPRECATED rgsiasdProvisionedReadCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'provisionedReadCapacityAutoScalingSettings' instead." #-}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasdIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Lude.Maybe Lude.Text)
rgsiasdIndexName = Lens.lens (indexName :: ReplicaGlobalSecondaryIndexAutoScalingDescription -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)
{-# DEPRECATED rgsiasdIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance
  Lude.FromJSON
    ReplicaGlobalSecondaryIndexAutoScalingDescription
  where
  parseJSON =
    Lude.withObject
      "ReplicaGlobalSecondaryIndexAutoScalingDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexAutoScalingDescription'
            Lude.<$> (x Lude..:? "IndexStatus")
            Lude.<*> (x Lude..:? "ProvisionedWriteCapacityAutoScalingSettings")
            Lude.<*> (x Lude..:? "ProvisionedReadCapacityAutoScalingSettings")
            Lude.<*> (x Lude..:? "IndexName")
      )
