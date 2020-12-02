{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a replica to be modified.
--
--
--
-- /See:/ 'updateReplicationGroupMemberAction' smart constructor.
data UpdateReplicationGroupMemberAction = UpdateReplicationGroupMemberAction'
  { _urgmaKMSMasterKeyId ::
      !(Maybe Text),
    _urgmaProvisionedThroughputOverride ::
      !( Maybe
           ProvisionedThroughputOverride
       ),
    _urgmaGlobalSecondaryIndexes ::
      !( Maybe
           ( List1
               ReplicaGlobalSecondaryIndex
           )
       ),
    _urgmaRegionName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateReplicationGroupMemberAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urgmaKMSMasterKeyId' - The AWS KMS customer master key (CMK) of the replica that should be used for AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
--
-- * 'urgmaProvisionedThroughputOverride' - Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
--
-- * 'urgmaGlobalSecondaryIndexes' - Replica-specific global secondary index settings.
--
-- * 'urgmaRegionName' - The Region where the replica exists.
updateReplicationGroupMemberAction ::
  -- | 'urgmaRegionName'
  Text ->
  UpdateReplicationGroupMemberAction
updateReplicationGroupMemberAction pRegionName_ =
  UpdateReplicationGroupMemberAction'
    { _urgmaKMSMasterKeyId =
        Nothing,
      _urgmaProvisionedThroughputOverride = Nothing,
      _urgmaGlobalSecondaryIndexes = Nothing,
      _urgmaRegionName = pRegionName_
    }

-- | The AWS KMS customer master key (CMK) of the replica that should be used for AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
urgmaKMSMasterKeyId :: Lens' UpdateReplicationGroupMemberAction (Maybe Text)
urgmaKMSMasterKeyId = lens _urgmaKMSMasterKeyId (\s a -> s {_urgmaKMSMasterKeyId = a})

-- | Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
urgmaProvisionedThroughputOverride :: Lens' UpdateReplicationGroupMemberAction (Maybe ProvisionedThroughputOverride)
urgmaProvisionedThroughputOverride = lens _urgmaProvisionedThroughputOverride (\s a -> s {_urgmaProvisionedThroughputOverride = a})

-- | Replica-specific global secondary index settings.
urgmaGlobalSecondaryIndexes :: Lens' UpdateReplicationGroupMemberAction (Maybe (NonEmpty ReplicaGlobalSecondaryIndex))
urgmaGlobalSecondaryIndexes = lens _urgmaGlobalSecondaryIndexes (\s a -> s {_urgmaGlobalSecondaryIndexes = a}) . mapping _List1

-- | The Region where the replica exists.
urgmaRegionName :: Lens' UpdateReplicationGroupMemberAction Text
urgmaRegionName = lens _urgmaRegionName (\s a -> s {_urgmaRegionName = a})

instance Hashable UpdateReplicationGroupMemberAction

instance NFData UpdateReplicationGroupMemberAction

instance ToJSON UpdateReplicationGroupMemberAction where
  toJSON UpdateReplicationGroupMemberAction' {..} =
    object
      ( catMaybes
          [ ("KMSMasterKeyId" .=) <$> _urgmaKMSMasterKeyId,
            ("ProvisionedThroughputOverride" .=)
              <$> _urgmaProvisionedThroughputOverride,
            ("GlobalSecondaryIndexes" .=) <$> _urgmaGlobalSecondaryIndexes,
            Just ("RegionName" .= _urgmaRegionName)
          ]
      )
