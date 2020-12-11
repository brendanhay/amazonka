-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
  ( UpdateReplicationGroupMemberAction (..),

    -- * Smart constructor
    mkUpdateReplicationGroupMemberAction,

    -- * Lenses
    urgmaKMSMasterKeyId,
    urgmaProvisionedThroughputOverride,
    urgmaGlobalSecondaryIndexes,
    urgmaRegionName,
  )
where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a replica to be modified.
--
-- /See:/ 'mkUpdateReplicationGroupMemberAction' smart constructor.
data UpdateReplicationGroupMemberAction = UpdateReplicationGroupMemberAction'
  { kmsMasterKeyId ::
      Lude.Maybe Lude.Text,
    provisionedThroughputOverride ::
      Lude.Maybe
        ProvisionedThroughputOverride,
    globalSecondaryIndexes ::
      Lude.Maybe
        ( Lude.NonEmpty
            ReplicaGlobalSecondaryIndex
        ),
    regionName ::
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

-- | Creates a value of 'UpdateReplicationGroupMemberAction' with the minimum fields required to make a request.
--
-- * 'globalSecondaryIndexes' - Replica-specific global secondary index settings.
-- * 'kmsMasterKeyId' - The AWS KMS customer master key (CMK) of the replica that should be used for AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
-- * 'provisionedThroughputOverride' - Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
-- * 'regionName' - The Region where the replica exists.
mkUpdateReplicationGroupMemberAction ::
  -- | 'regionName'
  Lude.Text ->
  UpdateReplicationGroupMemberAction
mkUpdateReplicationGroupMemberAction pRegionName_ =
  UpdateReplicationGroupMemberAction'
    { kmsMasterKeyId =
        Lude.Nothing,
      provisionedThroughputOverride = Lude.Nothing,
      globalSecondaryIndexes = Lude.Nothing,
      regionName = pRegionName_
    }

-- | The AWS KMS customer master key (CMK) of the replica that should be used for AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
--
-- /Note:/ Consider using 'kmsMasterKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgmaKMSMasterKeyId :: Lens.Lens' UpdateReplicationGroupMemberAction (Lude.Maybe Lude.Text)
urgmaKMSMasterKeyId = Lens.lens (kmsMasterKeyId :: UpdateReplicationGroupMemberAction -> Lude.Maybe Lude.Text) (\s a -> s {kmsMasterKeyId = a} :: UpdateReplicationGroupMemberAction)
{-# DEPRECATED urgmaKMSMasterKeyId "Use generic-lens or generic-optics with 'kmsMasterKeyId' instead." #-}

-- | Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgmaProvisionedThroughputOverride :: Lens.Lens' UpdateReplicationGroupMemberAction (Lude.Maybe ProvisionedThroughputOverride)
urgmaProvisionedThroughputOverride = Lens.lens (provisionedThroughputOverride :: UpdateReplicationGroupMemberAction -> Lude.Maybe ProvisionedThroughputOverride) (\s a -> s {provisionedThroughputOverride = a} :: UpdateReplicationGroupMemberAction)
{-# DEPRECATED urgmaProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

-- | Replica-specific global secondary index settings.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgmaGlobalSecondaryIndexes :: Lens.Lens' UpdateReplicationGroupMemberAction (Lude.Maybe (Lude.NonEmpty ReplicaGlobalSecondaryIndex))
urgmaGlobalSecondaryIndexes = Lens.lens (globalSecondaryIndexes :: UpdateReplicationGroupMemberAction -> Lude.Maybe (Lude.NonEmpty ReplicaGlobalSecondaryIndex)) (\s a -> s {globalSecondaryIndexes = a} :: UpdateReplicationGroupMemberAction)
{-# DEPRECATED urgmaGlobalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead." #-}

-- | The Region where the replica exists.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgmaRegionName :: Lens.Lens' UpdateReplicationGroupMemberAction Lude.Text
urgmaRegionName = Lens.lens (regionName :: UpdateReplicationGroupMemberAction -> Lude.Text) (\s a -> s {regionName = a} :: UpdateReplicationGroupMemberAction)
{-# DEPRECATED urgmaRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Lude.ToJSON UpdateReplicationGroupMemberAction where
  toJSON UpdateReplicationGroupMemberAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KMSMasterKeyId" Lude..=) Lude.<$> kmsMasterKeyId,
            ("ProvisionedThroughputOverride" Lude..=)
              Lude.<$> provisionedThroughputOverride,
            ("GlobalSecondaryIndexes" Lude..=) Lude.<$> globalSecondaryIndexes,
            Lude.Just ("RegionName" Lude..= regionName)
          ]
      )
