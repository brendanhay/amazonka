{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
  ( ReplicaGlobalSecondaryIndexSettingsDescription (..),

    -- * Smart constructor
    mkReplicaGlobalSecondaryIndexSettingsDescription,

    -- * Lenses
    rgsisdIndexStatus,
    rgsisdProvisionedReadCapacityUnits,
    rgsisdProvisionedWriteCapacityUnits,
    rgsisdProvisionedWriteCapacityAutoScalingSettings,
    rgsisdProvisionedReadCapacityAutoScalingSettings,
    rgsisdIndexName,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.IndexStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexSettingsDescription' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsDescription = ReplicaGlobalSecondaryIndexSettingsDescription'
  { -- | The current status of the global secondary index:
    --
    --
    --     * @CREATING@ - The global secondary index is being created.
    --
    --
    --     * @UPDATING@ - The global secondary index is being updated.
    --
    --
    --     * @DELETING@ - The global secondary index is being deleted.
    --
    --
    --     * @ACTIVE@ - The global secondary index is ready for use.
    indexStatus :: Lude.Maybe IndexStatus,
    -- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
    provisionedReadCapacityUnits :: Lude.Maybe Lude.Natural,
    -- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
    provisionedWriteCapacityUnits :: Lude.Maybe Lude.Natural,
    -- | Auto scaling settings for a global secondary index replica's write capacity units.
    provisionedWriteCapacityAutoScalingSettings :: Lude.Maybe AutoScalingSettingsDescription,
    -- | Auto scaling settings for a global secondary index replica's read capacity units.
    provisionedReadCapacityAutoScalingSettings :: Lude.Maybe AutoScalingSettingsDescription,
    -- | The name of the global secondary index. The name must be unique among all other indexes on this table.
    indexName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaGlobalSecondaryIndexSettingsDescription' with the minimum fields required to make a request.
--
-- * 'indexStatus' - The current status of the global secondary index:
--
--
--     * @CREATING@ - The global secondary index is being created.
--
--
--     * @UPDATING@ - The global secondary index is being updated.
--
--
--     * @DELETING@ - The global secondary index is being deleted.
--
--
--     * @ACTIVE@ - The global secondary index is ready for use.
--
--
-- * 'provisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
-- * 'provisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
-- * 'provisionedWriteCapacityAutoScalingSettings' - Auto scaling settings for a global secondary index replica's write capacity units.
-- * 'provisionedReadCapacityAutoScalingSettings' - Auto scaling settings for a global secondary index replica's read capacity units.
-- * 'indexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
mkReplicaGlobalSecondaryIndexSettingsDescription ::
  -- | 'indexName'
  Lude.Text ->
  ReplicaGlobalSecondaryIndexSettingsDescription
mkReplicaGlobalSecondaryIndexSettingsDescription pIndexName_ =
  ReplicaGlobalSecondaryIndexSettingsDescription'
    { indexStatus =
        Lude.Nothing,
      provisionedReadCapacityUnits = Lude.Nothing,
      provisionedWriteCapacityUnits = Lude.Nothing,
      provisionedWriteCapacityAutoScalingSettings =
        Lude.Nothing,
      provisionedReadCapacityAutoScalingSettings =
        Lude.Nothing,
      indexName = pIndexName_
    }

-- | The current status of the global secondary index:
--
--
--     * @CREATING@ - The global secondary index is being created.
--
--
--     * @UPDATING@ - The global secondary index is being updated.
--
--
--     * @DELETING@ - The global secondary index is being deleted.
--
--
--     * @ACTIVE@ - The global secondary index is ready for use.
--
--
--
-- /Note:/ Consider using 'indexStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdIndexStatus :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Lude.Maybe IndexStatus)
rgsisdIndexStatus = Lens.lens (indexStatus :: ReplicaGlobalSecondaryIndexSettingsDescription -> Lude.Maybe IndexStatus) (\s a -> s {indexStatus = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)
{-# DEPRECATED rgsisdIndexStatus "Use generic-lens or generic-optics with 'indexStatus' instead." #-}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- /Note:/ Consider using 'provisionedReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdProvisionedReadCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Lude.Maybe Lude.Natural)
rgsisdProvisionedReadCapacityUnits = Lens.lens (provisionedReadCapacityUnits :: ReplicaGlobalSecondaryIndexSettingsDescription -> Lude.Maybe Lude.Natural) (\s a -> s {provisionedReadCapacityUnits = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)
{-# DEPRECATED rgsisdProvisionedReadCapacityUnits "Use generic-lens or generic-optics with 'provisionedReadCapacityUnits' instead." #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- /Note:/ Consider using 'provisionedWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdProvisionedWriteCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Lude.Maybe Lude.Natural)
rgsisdProvisionedWriteCapacityUnits = Lens.lens (provisionedWriteCapacityUnits :: ReplicaGlobalSecondaryIndexSettingsDescription -> Lude.Maybe Lude.Natural) (\s a -> s {provisionedWriteCapacityUnits = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)
{-# DEPRECATED rgsisdProvisionedWriteCapacityUnits "Use generic-lens or generic-optics with 'provisionedWriteCapacityUnits' instead." #-}

-- | Auto scaling settings for a global secondary index replica's write capacity units.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Lude.Maybe AutoScalingSettingsDescription)
rgsisdProvisionedWriteCapacityAutoScalingSettings = Lens.lens (provisionedWriteCapacityAutoScalingSettings :: ReplicaGlobalSecondaryIndexSettingsDescription -> Lude.Maybe AutoScalingSettingsDescription) (\s a -> s {provisionedWriteCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)
{-# DEPRECATED rgsisdProvisionedWriteCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingSettings' instead." #-}

-- | Auto scaling settings for a global secondary index replica's read capacity units.
--
-- /Note:/ Consider using 'provisionedReadCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Lude.Maybe AutoScalingSettingsDescription)
rgsisdProvisionedReadCapacityAutoScalingSettings = Lens.lens (provisionedReadCapacityAutoScalingSettings :: ReplicaGlobalSecondaryIndexSettingsDescription -> Lude.Maybe AutoScalingSettingsDescription) (\s a -> s {provisionedReadCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)
{-# DEPRECATED rgsisdProvisionedReadCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'provisionedReadCapacityAutoScalingSettings' instead." #-}

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription Lude.Text
rgsisdIndexName = Lens.lens (indexName :: ReplicaGlobalSecondaryIndexSettingsDescription -> Lude.Text) (\s a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)
{-# DEPRECATED rgsisdIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance
  Lude.FromJSON
    ReplicaGlobalSecondaryIndexSettingsDescription
  where
  parseJSON =
    Lude.withObject
      "ReplicaGlobalSecondaryIndexSettingsDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexSettingsDescription'
            Lude.<$> (x Lude..:? "IndexStatus")
            Lude.<*> (x Lude..:? "ProvisionedReadCapacityUnits")
            Lude.<*> (x Lude..:? "ProvisionedWriteCapacityUnits")
            Lude.<*> (x Lude..:? "ProvisionedWriteCapacityAutoScalingSettings")
            Lude.<*> (x Lude..:? "ProvisionedReadCapacityAutoScalingSettings")
            Lude.<*> (x Lude..: "IndexName")
      )
