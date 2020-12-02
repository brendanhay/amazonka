{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a replica to be created.
--
--
--
-- /See:/ 'createReplicationGroupMemberAction' smart constructor.
data CreateReplicationGroupMemberAction = CreateReplicationGroupMemberAction'
  { _crgmaKMSMasterKeyId ::
      !(Maybe Text),
    _crgmaProvisionedThroughputOverride ::
      !( Maybe
           ProvisionedThroughputOverride
       ),
    _crgmaGlobalSecondaryIndexes ::
      !( Maybe
           ( List1
               ReplicaGlobalSecondaryIndex
           )
       ),
    _crgmaRegionName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReplicationGroupMemberAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgmaKMSMasterKeyId' - The AWS KMS customer master key (CMK) that should be used for AWS KMS encryption in the new replica. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
--
-- * 'crgmaProvisionedThroughputOverride' - Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
--
-- * 'crgmaGlobalSecondaryIndexes' - Replica-specific global secondary index settings.
--
-- * 'crgmaRegionName' - The Region where the new replica will be created.
createReplicationGroupMemberAction ::
  -- | 'crgmaRegionName'
  Text ->
  CreateReplicationGroupMemberAction
createReplicationGroupMemberAction pRegionName_ =
  CreateReplicationGroupMemberAction'
    { _crgmaKMSMasterKeyId =
        Nothing,
      _crgmaProvisionedThroughputOverride = Nothing,
      _crgmaGlobalSecondaryIndexes = Nothing,
      _crgmaRegionName = pRegionName_
    }

-- | The AWS KMS customer master key (CMK) that should be used for AWS KMS encryption in the new replica. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
crgmaKMSMasterKeyId :: Lens' CreateReplicationGroupMemberAction (Maybe Text)
crgmaKMSMasterKeyId = lens _crgmaKMSMasterKeyId (\s a -> s {_crgmaKMSMasterKeyId = a})

-- | Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
crgmaProvisionedThroughputOverride :: Lens' CreateReplicationGroupMemberAction (Maybe ProvisionedThroughputOverride)
crgmaProvisionedThroughputOverride = lens _crgmaProvisionedThroughputOverride (\s a -> s {_crgmaProvisionedThroughputOverride = a})

-- | Replica-specific global secondary index settings.
crgmaGlobalSecondaryIndexes :: Lens' CreateReplicationGroupMemberAction (Maybe (NonEmpty ReplicaGlobalSecondaryIndex))
crgmaGlobalSecondaryIndexes = lens _crgmaGlobalSecondaryIndexes (\s a -> s {_crgmaGlobalSecondaryIndexes = a}) . mapping _List1

-- | The Region where the new replica will be created.
crgmaRegionName :: Lens' CreateReplicationGroupMemberAction Text
crgmaRegionName = lens _crgmaRegionName (\s a -> s {_crgmaRegionName = a})

instance Hashable CreateReplicationGroupMemberAction

instance NFData CreateReplicationGroupMemberAction

instance ToJSON CreateReplicationGroupMemberAction where
  toJSON CreateReplicationGroupMemberAction' {..} =
    object
      ( catMaybes
          [ ("KMSMasterKeyId" .=) <$> _crgmaKMSMasterKeyId,
            ("ProvisionedThroughputOverride" .=)
              <$> _crgmaProvisionedThroughputOverride,
            ("GlobalSecondaryIndexes" .=) <$> _crgmaGlobalSecondaryIndexes,
            Just ("RegionName" .= _crgmaRegionName)
          ]
      )
