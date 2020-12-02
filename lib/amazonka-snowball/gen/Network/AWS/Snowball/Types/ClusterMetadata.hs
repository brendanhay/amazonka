{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.ClusterState
import Network.AWS.Snowball.Types.JobResource
import Network.AWS.Snowball.Types.JobType
import Network.AWS.Snowball.Types.Notification
import Network.AWS.Snowball.Types.ShippingOption
import Network.AWS.Snowball.Types.SnowballType
import Network.AWS.Snowball.Types.TaxDocuments

-- | Contains metadata about a specific cluster.
--
--
--
-- /See:/ 'clusterMetadata' smart constructor.
data ClusterMetadata = ClusterMetadata'
  { _cmJobType ::
      !(Maybe JobType),
    _cmKMSKeyARN :: !(Maybe Text),
    _cmClusterState :: !(Maybe ClusterState),
    _cmNotification :: !(Maybe Notification),
    _cmForwardingAddressId :: !(Maybe Text),
    _cmAddressId :: !(Maybe Text),
    _cmSnowballType :: !(Maybe SnowballType),
    _cmShippingOption :: !(Maybe ShippingOption),
    _cmResources :: !(Maybe JobResource),
    _cmClusterId :: !(Maybe Text),
    _cmCreationDate :: !(Maybe POSIX),
    _cmDescription :: !(Maybe Text),
    _cmTaxDocuments :: !(Maybe TaxDocuments),
    _cmRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmJobType' - The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
--
-- * 'cmKMSKeyARN' - The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
--
-- * 'cmClusterState' - The current status of the cluster.
--
-- * 'cmNotification' - The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
--
-- * 'cmForwardingAddressId' - The ID of the address that you want a cluster shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
--
-- * 'cmAddressId' - The automatically generated ID for a specific address.
--
-- * 'cmSnowballType' - The type of AWS Snow device to use for this cluster.
--
-- * 'cmShippingOption' - The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snow devices are delivered in one to seven days.     * In the US, you have access to one-day shipping and two-day shipping.
--
-- * 'cmResources' - The arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
--
-- * 'cmClusterId' - The automatically generated ID for a cluster.
--
-- * 'cmCreationDate' - The creation date for this cluster.
--
-- * 'cmDescription' - The optional description of the cluster.
--
-- * 'cmTaxDocuments' - The tax documents required in your AWS Region.
--
-- * 'cmRoleARN' - The role ARN associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
clusterMetadata ::
  ClusterMetadata
clusterMetadata =
  ClusterMetadata'
    { _cmJobType = Nothing,
      _cmKMSKeyARN = Nothing,
      _cmClusterState = Nothing,
      _cmNotification = Nothing,
      _cmForwardingAddressId = Nothing,
      _cmAddressId = Nothing,
      _cmSnowballType = Nothing,
      _cmShippingOption = Nothing,
      _cmResources = Nothing,
      _cmClusterId = Nothing,
      _cmCreationDate = Nothing,
      _cmDescription = Nothing,
      _cmTaxDocuments = Nothing,
      _cmRoleARN = Nothing
    }

-- | The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
cmJobType :: Lens' ClusterMetadata (Maybe JobType)
cmJobType = lens _cmJobType (\s a -> s {_cmJobType = a})

-- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
cmKMSKeyARN :: Lens' ClusterMetadata (Maybe Text)
cmKMSKeyARN = lens _cmKMSKeyARN (\s a -> s {_cmKMSKeyARN = a})

-- | The current status of the cluster.
cmClusterState :: Lens' ClusterMetadata (Maybe ClusterState)
cmClusterState = lens _cmClusterState (\s a -> s {_cmClusterState = a})

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
cmNotification :: Lens' ClusterMetadata (Maybe Notification)
cmNotification = lens _cmNotification (\s a -> s {_cmNotification = a})

-- | The ID of the address that you want a cluster shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
cmForwardingAddressId :: Lens' ClusterMetadata (Maybe Text)
cmForwardingAddressId = lens _cmForwardingAddressId (\s a -> s {_cmForwardingAddressId = a})

-- | The automatically generated ID for a specific address.
cmAddressId :: Lens' ClusterMetadata (Maybe Text)
cmAddressId = lens _cmAddressId (\s a -> s {_cmAddressId = a})

-- | The type of AWS Snow device to use for this cluster.
cmSnowballType :: Lens' ClusterMetadata (Maybe SnowballType)
cmSnowballType = lens _cmSnowballType (\s a -> s {_cmSnowballType = a})

-- | The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snow devices are delivered in one to seven days.     * In the US, you have access to one-day shipping and two-day shipping.
cmShippingOption :: Lens' ClusterMetadata (Maybe ShippingOption)
cmShippingOption = lens _cmShippingOption (\s a -> s {_cmShippingOption = a})

-- | The arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
cmResources :: Lens' ClusterMetadata (Maybe JobResource)
cmResources = lens _cmResources (\s a -> s {_cmResources = a})

-- | The automatically generated ID for a cluster.
cmClusterId :: Lens' ClusterMetadata (Maybe Text)
cmClusterId = lens _cmClusterId (\s a -> s {_cmClusterId = a})

-- | The creation date for this cluster.
cmCreationDate :: Lens' ClusterMetadata (Maybe UTCTime)
cmCreationDate = lens _cmCreationDate (\s a -> s {_cmCreationDate = a}) . mapping _Time

-- | The optional description of the cluster.
cmDescription :: Lens' ClusterMetadata (Maybe Text)
cmDescription = lens _cmDescription (\s a -> s {_cmDescription = a})

-- | The tax documents required in your AWS Region.
cmTaxDocuments :: Lens' ClusterMetadata (Maybe TaxDocuments)
cmTaxDocuments = lens _cmTaxDocuments (\s a -> s {_cmTaxDocuments = a})

-- | The role ARN associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
cmRoleARN :: Lens' ClusterMetadata (Maybe Text)
cmRoleARN = lens _cmRoleARN (\s a -> s {_cmRoleARN = a})

instance FromJSON ClusterMetadata where
  parseJSON =
    withObject
      "ClusterMetadata"
      ( \x ->
          ClusterMetadata'
            <$> (x .:? "JobType")
            <*> (x .:? "KmsKeyARN")
            <*> (x .:? "ClusterState")
            <*> (x .:? "Notification")
            <*> (x .:? "ForwardingAddressId")
            <*> (x .:? "AddressId")
            <*> (x .:? "SnowballType")
            <*> (x .:? "ShippingOption")
            <*> (x .:? "Resources")
            <*> (x .:? "ClusterId")
            <*> (x .:? "CreationDate")
            <*> (x .:? "Description")
            <*> (x .:? "TaxDocuments")
            <*> (x .:? "RoleARN")
      )

instance Hashable ClusterMetadata

instance NFData ClusterMetadata
