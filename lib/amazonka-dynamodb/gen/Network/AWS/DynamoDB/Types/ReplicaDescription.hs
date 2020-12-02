{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaDescription where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.ReplicaStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the details of the replica.
--
--
--
-- /See:/ 'replicaDescription' smart constructor.
data ReplicaDescription = ReplicaDescription'
  { _rdReplicaStatus ::
      !(Maybe ReplicaStatus),
    _rdRegionName :: !(Maybe Text),
    _rdReplicaStatusPercentProgress :: !(Maybe Text),
    _rdReplicaStatusDescription :: !(Maybe Text),
    _rdReplicaInaccessibleDateTime :: !(Maybe POSIX),
    _rdKMSMasterKeyId :: !(Maybe Text),
    _rdProvisionedThroughputOverride ::
      !(Maybe ProvisionedThroughputOverride),
    _rdGlobalSecondaryIndexes ::
      !(Maybe [ReplicaGlobalSecondaryIndexDescription])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicaDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdReplicaStatus' - The current state of the replica:     * @CREATING@ - The replica is being created.     * @UPDATING@ - The replica is being updated.     * @DELETING@ - The replica is being deleted.     * @ACTIVE@ - The replica is ready for use.     * @REGION_DISABLED@ - The replica is inaccessible because the AWS Region has been disabled.     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The AWS KMS key used to encrypt the table is inaccessible.
--
-- * 'rdRegionName' - The name of the Region.
--
-- * 'rdReplicaStatusPercentProgress' - Specifies the progress of a Create, Update, or Delete action on the replica as a percentage.
--
-- * 'rdReplicaStatusDescription' - Detailed information about the replica status.
--
-- * 'rdReplicaInaccessibleDateTime' - The time at which the replica was first detected as inaccessible. To determine cause of inaccessibility check the @ReplicaStatus@ property.
--
-- * 'rdKMSMasterKeyId' - The AWS KMS customer master key (CMK) of the replica that will be used for AWS KMS encryption.
--
-- * 'rdProvisionedThroughputOverride' - Replica-specific provisioned throughput. If not described, uses the source table's provisioned throughput settings.
--
-- * 'rdGlobalSecondaryIndexes' - Replica-specific global secondary index settings.
replicaDescription ::
  ReplicaDescription
replicaDescription =
  ReplicaDescription'
    { _rdReplicaStatus = Nothing,
      _rdRegionName = Nothing,
      _rdReplicaStatusPercentProgress = Nothing,
      _rdReplicaStatusDescription = Nothing,
      _rdReplicaInaccessibleDateTime = Nothing,
      _rdKMSMasterKeyId = Nothing,
      _rdProvisionedThroughputOverride = Nothing,
      _rdGlobalSecondaryIndexes = Nothing
    }

-- | The current state of the replica:     * @CREATING@ - The replica is being created.     * @UPDATING@ - The replica is being updated.     * @DELETING@ - The replica is being deleted.     * @ACTIVE@ - The replica is ready for use.     * @REGION_DISABLED@ - The replica is inaccessible because the AWS Region has been disabled.     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The AWS KMS key used to encrypt the table is inaccessible.
rdReplicaStatus :: Lens' ReplicaDescription (Maybe ReplicaStatus)
rdReplicaStatus = lens _rdReplicaStatus (\s a -> s {_rdReplicaStatus = a})

-- | The name of the Region.
rdRegionName :: Lens' ReplicaDescription (Maybe Text)
rdRegionName = lens _rdRegionName (\s a -> s {_rdRegionName = a})

-- | Specifies the progress of a Create, Update, or Delete action on the replica as a percentage.
rdReplicaStatusPercentProgress :: Lens' ReplicaDescription (Maybe Text)
rdReplicaStatusPercentProgress = lens _rdReplicaStatusPercentProgress (\s a -> s {_rdReplicaStatusPercentProgress = a})

-- | Detailed information about the replica status.
rdReplicaStatusDescription :: Lens' ReplicaDescription (Maybe Text)
rdReplicaStatusDescription = lens _rdReplicaStatusDescription (\s a -> s {_rdReplicaStatusDescription = a})

-- | The time at which the replica was first detected as inaccessible. To determine cause of inaccessibility check the @ReplicaStatus@ property.
rdReplicaInaccessibleDateTime :: Lens' ReplicaDescription (Maybe UTCTime)
rdReplicaInaccessibleDateTime = lens _rdReplicaInaccessibleDateTime (\s a -> s {_rdReplicaInaccessibleDateTime = a}) . mapping _Time

-- | The AWS KMS customer master key (CMK) of the replica that will be used for AWS KMS encryption.
rdKMSMasterKeyId :: Lens' ReplicaDescription (Maybe Text)
rdKMSMasterKeyId = lens _rdKMSMasterKeyId (\s a -> s {_rdKMSMasterKeyId = a})

-- | Replica-specific provisioned throughput. If not described, uses the source table's provisioned throughput settings.
rdProvisionedThroughputOverride :: Lens' ReplicaDescription (Maybe ProvisionedThroughputOverride)
rdProvisionedThroughputOverride = lens _rdProvisionedThroughputOverride (\s a -> s {_rdProvisionedThroughputOverride = a})

-- | Replica-specific global secondary index settings.
rdGlobalSecondaryIndexes :: Lens' ReplicaDescription [ReplicaGlobalSecondaryIndexDescription]
rdGlobalSecondaryIndexes = lens _rdGlobalSecondaryIndexes (\s a -> s {_rdGlobalSecondaryIndexes = a}) . _Default . _Coerce

instance FromJSON ReplicaDescription where
  parseJSON =
    withObject
      "ReplicaDescription"
      ( \x ->
          ReplicaDescription'
            <$> (x .:? "ReplicaStatus")
            <*> (x .:? "RegionName")
            <*> (x .:? "ReplicaStatusPercentProgress")
            <*> (x .:? "ReplicaStatusDescription")
            <*> (x .:? "ReplicaInaccessibleDateTime")
            <*> (x .:? "KMSMasterKeyId")
            <*> (x .:? "ProvisionedThroughputOverride")
            <*> (x .:? "GlobalSecondaryIndexes" .!= mempty)
      )

instance Hashable ReplicaDescription

instance NFData ReplicaDescription
