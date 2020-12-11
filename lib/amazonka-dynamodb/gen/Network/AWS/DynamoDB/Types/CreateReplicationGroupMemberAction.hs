-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
  ( CreateReplicationGroupMemberAction (..),

    -- * Smart constructor
    mkCreateReplicationGroupMemberAction,

    -- * Lenses
    crgmaKMSMasterKeyId,
    crgmaProvisionedThroughputOverride,
    crgmaGlobalSecondaryIndexes,
    crgmaRegionName,
  )
where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a replica to be created.
--
-- /See:/ 'mkCreateReplicationGroupMemberAction' smart constructor.
data CreateReplicationGroupMemberAction = CreateReplicationGroupMemberAction'
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

-- | Creates a value of 'CreateReplicationGroupMemberAction' with the minimum fields required to make a request.
--
-- * 'globalSecondaryIndexes' - Replica-specific global secondary index settings.
-- * 'kmsMasterKeyId' - The AWS KMS customer master key (CMK) that should be used for AWS KMS encryption in the new replica. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
-- * 'provisionedThroughputOverride' - Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
-- * 'regionName' - The Region where the new replica will be created.
mkCreateReplicationGroupMemberAction ::
  -- | 'regionName'
  Lude.Text ->
  CreateReplicationGroupMemberAction
mkCreateReplicationGroupMemberAction pRegionName_ =
  CreateReplicationGroupMemberAction'
    { kmsMasterKeyId =
        Lude.Nothing,
      provisionedThroughputOverride = Lude.Nothing,
      globalSecondaryIndexes = Lude.Nothing,
      regionName = pRegionName_
    }

-- | The AWS KMS customer master key (CMK) that should be used for AWS KMS encryption in the new replica. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
--
-- /Note:/ Consider using 'kmsMasterKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgmaKMSMasterKeyId :: Lens.Lens' CreateReplicationGroupMemberAction (Lude.Maybe Lude.Text)
crgmaKMSMasterKeyId = Lens.lens (kmsMasterKeyId :: CreateReplicationGroupMemberAction -> Lude.Maybe Lude.Text) (\s a -> s {kmsMasterKeyId = a} :: CreateReplicationGroupMemberAction)
{-# DEPRECATED crgmaKMSMasterKeyId "Use generic-lens or generic-optics with 'kmsMasterKeyId' instead." #-}

-- | Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgmaProvisionedThroughputOverride :: Lens.Lens' CreateReplicationGroupMemberAction (Lude.Maybe ProvisionedThroughputOverride)
crgmaProvisionedThroughputOverride = Lens.lens (provisionedThroughputOverride :: CreateReplicationGroupMemberAction -> Lude.Maybe ProvisionedThroughputOverride) (\s a -> s {provisionedThroughputOverride = a} :: CreateReplicationGroupMemberAction)
{-# DEPRECATED crgmaProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

-- | Replica-specific global secondary index settings.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgmaGlobalSecondaryIndexes :: Lens.Lens' CreateReplicationGroupMemberAction (Lude.Maybe (Lude.NonEmpty ReplicaGlobalSecondaryIndex))
crgmaGlobalSecondaryIndexes = Lens.lens (globalSecondaryIndexes :: CreateReplicationGroupMemberAction -> Lude.Maybe (Lude.NonEmpty ReplicaGlobalSecondaryIndex)) (\s a -> s {globalSecondaryIndexes = a} :: CreateReplicationGroupMemberAction)
{-# DEPRECATED crgmaGlobalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead." #-}

-- | The Region where the new replica will be created.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgmaRegionName :: Lens.Lens' CreateReplicationGroupMemberAction Lude.Text
crgmaRegionName = Lens.lens (regionName :: CreateReplicationGroupMemberAction -> Lude.Text) (\s a -> s {regionName = a} :: CreateReplicationGroupMemberAction)
{-# DEPRECATED crgmaRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Lude.ToJSON CreateReplicationGroupMemberAction where
  toJSON CreateReplicationGroupMemberAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KMSMasterKeyId" Lude..=) Lude.<$> kmsMasterKeyId,
            ("ProvisionedThroughputOverride" Lude..=)
              Lude.<$> provisionedThroughputOverride,
            ("GlobalSecondaryIndexes" Lude..=) Lude.<$> globalSecondaryIndexes,
            Lude.Just ("RegionName" Lude..= regionName)
          ]
      )
