{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.Sum

-- | The address that you want the Snowball or Snowballs associated with a specific job to be shipped to. Addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. Although no individual elements of the @Address@ are required, if the address is invalid or unsupported, then an exception is thrown.
--
--
--
-- /See:/ 'address' smart constructor.
data Address = Address'
  { _aIsRestricted         :: !(Maybe Bool)
  , _aStreet3              :: !(Maybe Text)
  , _aLandmark             :: !(Maybe Text)
  , _aPostalCode           :: !(Maybe Text)
  , _aCountry              :: !(Maybe Text)
  , _aStateOrProvince      :: !(Maybe Text)
  , _aStreet2              :: !(Maybe Text)
  , _aAddressId            :: !(Maybe Text)
  , _aCity                 :: !(Maybe Text)
  , _aPhoneNumber          :: !(Maybe Text)
  , _aCompany              :: !(Maybe Text)
  , _aName                 :: !(Maybe Text)
  , _aPrefectureOrDistrict :: !(Maybe Text)
  , _aStreet1              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aIsRestricted' - If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
--
-- * 'aStreet3' - The third line in a street address that a Snowball is to be delivered to.
--
-- * 'aLandmark' - This field is no longer used and the value is ignored.
--
-- * 'aPostalCode' - The postal code in an address that a Snowball is to be delivered to.
--
-- * 'aCountry' - The country in an address that a Snowball is to be delivered to.
--
-- * 'aStateOrProvince' - The state or province in an address that a Snowball is to be delivered to.
--
-- * 'aStreet2' - The second line in a street address that a Snowball is to be delivered to.
--
-- * 'aAddressId' - The unique ID for an address.
--
-- * 'aCity' - The city in an address that a Snowball is to be delivered to.
--
-- * 'aPhoneNumber' - The phone number associated with an address that a Snowball is to be delivered to.
--
-- * 'aCompany' - The name of the company to receive a Snowball at an address.
--
-- * 'aName' - The name of a person to receive a Snowball at an address.
--
-- * 'aPrefectureOrDistrict' - This field is no longer used and the value is ignored.
--
-- * 'aStreet1' - The first line in a street address that a Snowball is to be delivered to.
address
    :: Address
address =
  Address'
    { _aIsRestricted = Nothing
    , _aStreet3 = Nothing
    , _aLandmark = Nothing
    , _aPostalCode = Nothing
    , _aCountry = Nothing
    , _aStateOrProvince = Nothing
    , _aStreet2 = Nothing
    , _aAddressId = Nothing
    , _aCity = Nothing
    , _aPhoneNumber = Nothing
    , _aCompany = Nothing
    , _aName = Nothing
    , _aPrefectureOrDistrict = Nothing
    , _aStreet1 = Nothing
    }


-- | If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
aIsRestricted :: Lens' Address (Maybe Bool)
aIsRestricted = lens _aIsRestricted (\ s a -> s{_aIsRestricted = a})

-- | The third line in a street address that a Snowball is to be delivered to.
aStreet3 :: Lens' Address (Maybe Text)
aStreet3 = lens _aStreet3 (\ s a -> s{_aStreet3 = a})

-- | This field is no longer used and the value is ignored.
aLandmark :: Lens' Address (Maybe Text)
aLandmark = lens _aLandmark (\ s a -> s{_aLandmark = a})

-- | The postal code in an address that a Snowball is to be delivered to.
aPostalCode :: Lens' Address (Maybe Text)
aPostalCode = lens _aPostalCode (\ s a -> s{_aPostalCode = a})

-- | The country in an address that a Snowball is to be delivered to.
aCountry :: Lens' Address (Maybe Text)
aCountry = lens _aCountry (\ s a -> s{_aCountry = a})

-- | The state or province in an address that a Snowball is to be delivered to.
aStateOrProvince :: Lens' Address (Maybe Text)
aStateOrProvince = lens _aStateOrProvince (\ s a -> s{_aStateOrProvince = a})

-- | The second line in a street address that a Snowball is to be delivered to.
aStreet2 :: Lens' Address (Maybe Text)
aStreet2 = lens _aStreet2 (\ s a -> s{_aStreet2 = a})

-- | The unique ID for an address.
aAddressId :: Lens' Address (Maybe Text)
aAddressId = lens _aAddressId (\ s a -> s{_aAddressId = a})

-- | The city in an address that a Snowball is to be delivered to.
aCity :: Lens' Address (Maybe Text)
aCity = lens _aCity (\ s a -> s{_aCity = a})

-- | The phone number associated with an address that a Snowball is to be delivered to.
aPhoneNumber :: Lens' Address (Maybe Text)
aPhoneNumber = lens _aPhoneNumber (\ s a -> s{_aPhoneNumber = a})

-- | The name of the company to receive a Snowball at an address.
aCompany :: Lens' Address (Maybe Text)
aCompany = lens _aCompany (\ s a -> s{_aCompany = a})

-- | The name of a person to receive a Snowball at an address.
aName :: Lens' Address (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

-- | This field is no longer used and the value is ignored.
aPrefectureOrDistrict :: Lens' Address (Maybe Text)
aPrefectureOrDistrict = lens _aPrefectureOrDistrict (\ s a -> s{_aPrefectureOrDistrict = a})

-- | The first line in a street address that a Snowball is to be delivered to.
aStreet1 :: Lens' Address (Maybe Text)
aStreet1 = lens _aStreet1 (\ s a -> s{_aStreet1 = a})

instance FromJSON Address where
        parseJSON
          = withObject "Address"
              (\ x ->
                 Address' <$>
                   (x .:? "IsRestricted") <*> (x .:? "Street3") <*>
                     (x .:? "Landmark")
                     <*> (x .:? "PostalCode")
                     <*> (x .:? "Country")
                     <*> (x .:? "StateOrProvince")
                     <*> (x .:? "Street2")
                     <*> (x .:? "AddressId")
                     <*> (x .:? "City")
                     <*> (x .:? "PhoneNumber")
                     <*> (x .:? "Company")
                     <*> (x .:? "Name")
                     <*> (x .:? "PrefectureOrDistrict")
                     <*> (x .:? "Street1"))

instance Hashable Address where

instance NFData Address where

instance ToJSON Address where
        toJSON Address'{..}
          = object
              (catMaybes
                 [("IsRestricted" .=) <$> _aIsRestricted,
                  ("Street3" .=) <$> _aStreet3,
                  ("Landmark" .=) <$> _aLandmark,
                  ("PostalCode" .=) <$> _aPostalCode,
                  ("Country" .=) <$> _aCountry,
                  ("StateOrProvince" .=) <$> _aStateOrProvince,
                  ("Street2" .=) <$> _aStreet2,
                  ("AddressId" .=) <$> _aAddressId,
                  ("City" .=) <$> _aCity,
                  ("PhoneNumber" .=) <$> _aPhoneNumber,
                  ("Company" .=) <$> _aCompany, ("Name" .=) <$> _aName,
                  ("PrefectureOrDistrict" .=) <$>
                    _aPrefectureOrDistrict,
                  ("Street1" .=) <$> _aStreet1])

-- | Contains a cluster's state, a cluster's ID, and other important information.
--
--
--
-- /See:/ 'clusterListEntry' smart constructor.
data ClusterListEntry = ClusterListEntry'
  { _cleClusterState :: !(Maybe ClusterState)
  , _cleClusterId    :: !(Maybe Text)
  , _cleCreationDate :: !(Maybe POSIX)
  , _cleDescription  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cleClusterState' - The current state of this cluster. For information about the state of a specific node, see 'JobListEntry$JobState' .
--
-- * 'cleClusterId' - The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'cleCreationDate' - The creation date for this cluster.
--
-- * 'cleDescription' - Defines an optional description of the cluster, for example @Environmental Data Cluster-01@ .
clusterListEntry
    :: ClusterListEntry
clusterListEntry =
  ClusterListEntry'
    { _cleClusterState = Nothing
    , _cleClusterId = Nothing
    , _cleCreationDate = Nothing
    , _cleDescription = Nothing
    }


-- | The current state of this cluster. For information about the state of a specific node, see 'JobListEntry$JobState' .
cleClusterState :: Lens' ClusterListEntry (Maybe ClusterState)
cleClusterState = lens _cleClusterState (\ s a -> s{_cleClusterState = a})

-- | The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
cleClusterId :: Lens' ClusterListEntry (Maybe Text)
cleClusterId = lens _cleClusterId (\ s a -> s{_cleClusterId = a})

-- | The creation date for this cluster.
cleCreationDate :: Lens' ClusterListEntry (Maybe UTCTime)
cleCreationDate = lens _cleCreationDate (\ s a -> s{_cleCreationDate = a}) . mapping _Time

-- | Defines an optional description of the cluster, for example @Environmental Data Cluster-01@ .
cleDescription :: Lens' ClusterListEntry (Maybe Text)
cleDescription = lens _cleDescription (\ s a -> s{_cleDescription = a})

instance FromJSON ClusterListEntry where
        parseJSON
          = withObject "ClusterListEntry"
              (\ x ->
                 ClusterListEntry' <$>
                   (x .:? "ClusterState") <*> (x .:? "ClusterId") <*>
                     (x .:? "CreationDate")
                     <*> (x .:? "Description"))

instance Hashable ClusterListEntry where

instance NFData ClusterListEntry where

-- | Contains metadata about a specific cluster.
--
--
--
-- /See:/ 'clusterMetadata' smart constructor.
data ClusterMetadata = ClusterMetadata'
  { _cmJobType             :: !(Maybe JobType)
  , _cmKMSKeyARN           :: !(Maybe Text)
  , _cmClusterState        :: !(Maybe ClusterState)
  , _cmNotification        :: !(Maybe Notification)
  , _cmForwardingAddressId :: !(Maybe Text)
  , _cmAddressId           :: !(Maybe Text)
  , _cmSnowballType        :: !(Maybe SnowballType)
  , _cmShippingOption      :: !(Maybe ShippingOption)
  , _cmResources           :: !(Maybe JobResource)
  , _cmClusterId           :: !(Maybe Text)
  , _cmCreationDate        :: !(Maybe POSIX)
  , _cmDescription         :: !(Maybe Text)
  , _cmRoleARN             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmJobType' - The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
--
-- * 'cmKMSKeyARN' - The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster. This ARN was created using the <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
--
-- * 'cmClusterState' - The current status of the cluster.
--
-- * 'cmNotification' - The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
--
-- * 'cmForwardingAddressId' - The ID of the address that you want a cluster shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
--
-- * 'cmAddressId' - The automatically generated ID for a specific address.
--
-- * 'cmSnowballType' - The type of AWS Snowball appliance to use for this cluster. Currently, the only supported appliance type for cluster jobs is @EDGE@ .
--
-- * 'cmShippingOption' - The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each Snowball Edge appliance, rather it represents how quickly each appliance moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, appliances shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowball Edges shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowball Edges are delivered in one to seven days.     * In the US, you have access to one-day shipping and two-day shipping.
--
-- * 'cmResources' - The arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
--
-- * 'cmClusterId' - The automatically generated ID for a cluster.
--
-- * 'cmCreationDate' - The creation date for this cluster.
--
-- * 'cmDescription' - The optional description of the cluster.
--
-- * 'cmRoleARN' - The role ARN associated with this cluster. This ARN was created using the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
clusterMetadata
    :: ClusterMetadata
clusterMetadata =
  ClusterMetadata'
    { _cmJobType = Nothing
    , _cmKMSKeyARN = Nothing
    , _cmClusterState = Nothing
    , _cmNotification = Nothing
    , _cmForwardingAddressId = Nothing
    , _cmAddressId = Nothing
    , _cmSnowballType = Nothing
    , _cmShippingOption = Nothing
    , _cmResources = Nothing
    , _cmClusterId = Nothing
    , _cmCreationDate = Nothing
    , _cmDescription = Nothing
    , _cmRoleARN = Nothing
    }


-- | The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
cmJobType :: Lens' ClusterMetadata (Maybe JobType)
cmJobType = lens _cmJobType (\ s a -> s{_cmJobType = a})

-- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster. This ARN was created using the <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
cmKMSKeyARN :: Lens' ClusterMetadata (Maybe Text)
cmKMSKeyARN = lens _cmKMSKeyARN (\ s a -> s{_cmKMSKeyARN = a})

-- | The current status of the cluster.
cmClusterState :: Lens' ClusterMetadata (Maybe ClusterState)
cmClusterState = lens _cmClusterState (\ s a -> s{_cmClusterState = a})

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
cmNotification :: Lens' ClusterMetadata (Maybe Notification)
cmNotification = lens _cmNotification (\ s a -> s{_cmNotification = a})

-- | The ID of the address that you want a cluster shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
cmForwardingAddressId :: Lens' ClusterMetadata (Maybe Text)
cmForwardingAddressId = lens _cmForwardingAddressId (\ s a -> s{_cmForwardingAddressId = a})

-- | The automatically generated ID for a specific address.
cmAddressId :: Lens' ClusterMetadata (Maybe Text)
cmAddressId = lens _cmAddressId (\ s a -> s{_cmAddressId = a})

-- | The type of AWS Snowball appliance to use for this cluster. Currently, the only supported appliance type for cluster jobs is @EDGE@ .
cmSnowballType :: Lens' ClusterMetadata (Maybe SnowballType)
cmSnowballType = lens _cmSnowballType (\ s a -> s{_cmSnowballType = a})

-- | The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each Snowball Edge appliance, rather it represents how quickly each appliance moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, appliances shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowball Edges shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowball Edges are delivered in one to seven days.     * In the US, you have access to one-day shipping and two-day shipping.
cmShippingOption :: Lens' ClusterMetadata (Maybe ShippingOption)
cmShippingOption = lens _cmShippingOption (\ s a -> s{_cmShippingOption = a})

-- | The arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
cmResources :: Lens' ClusterMetadata (Maybe JobResource)
cmResources = lens _cmResources (\ s a -> s{_cmResources = a})

-- | The automatically generated ID for a cluster.
cmClusterId :: Lens' ClusterMetadata (Maybe Text)
cmClusterId = lens _cmClusterId (\ s a -> s{_cmClusterId = a})

-- | The creation date for this cluster.
cmCreationDate :: Lens' ClusterMetadata (Maybe UTCTime)
cmCreationDate = lens _cmCreationDate (\ s a -> s{_cmCreationDate = a}) . mapping _Time

-- | The optional description of the cluster.
cmDescription :: Lens' ClusterMetadata (Maybe Text)
cmDescription = lens _cmDescription (\ s a -> s{_cmDescription = a})

-- | The role ARN associated with this cluster. This ARN was created using the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
cmRoleARN :: Lens' ClusterMetadata (Maybe Text)
cmRoleARN = lens _cmRoleARN (\ s a -> s{_cmRoleARN = a})

instance FromJSON ClusterMetadata where
        parseJSON
          = withObject "ClusterMetadata"
              (\ x ->
                 ClusterMetadata' <$>
                   (x .:? "JobType") <*> (x .:? "KmsKeyARN") <*>
                     (x .:? "ClusterState")
                     <*> (x .:? "Notification")
                     <*> (x .:? "ForwardingAddressId")
                     <*> (x .:? "AddressId")
                     <*> (x .:? "SnowballType")
                     <*> (x .:? "ShippingOption")
                     <*> (x .:? "Resources")
                     <*> (x .:? "ClusterId")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Description")
                     <*> (x .:? "RoleARN"))

instance Hashable ClusterMetadata where

instance NFData ClusterMetadata where

-- | Defines the real-time status of a Snowball's data transfer while the appliance is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
--
--
--
-- /See:/ 'dataTransfer' smart constructor.
data DataTransfer = DataTransfer'
  { _dtTotalObjects       :: !(Maybe Integer)
  , _dtTotalBytes         :: !(Maybe Integer)
  , _dtObjectsTransferred :: !(Maybe Integer)
  , _dtBytesTransferred   :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DataTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTotalObjects' - The total number of objects for a transfer between a Snowball and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
--
-- * 'dtTotalBytes' - The total bytes of data for a transfer between a Snowball and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
--
-- * 'dtObjectsTransferred' - The number of objects transferred between a Snowball and Amazon S3.
--
-- * 'dtBytesTransferred' - The number of bytes transferred between a Snowball and Amazon S3.
dataTransfer
    :: DataTransfer
dataTransfer =
  DataTransfer'
    { _dtTotalObjects = Nothing
    , _dtTotalBytes = Nothing
    , _dtObjectsTransferred = Nothing
    , _dtBytesTransferred = Nothing
    }


-- | The total number of objects for a transfer between a Snowball and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
dtTotalObjects :: Lens' DataTransfer (Maybe Integer)
dtTotalObjects = lens _dtTotalObjects (\ s a -> s{_dtTotalObjects = a})

-- | The total bytes of data for a transfer between a Snowball and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
dtTotalBytes :: Lens' DataTransfer (Maybe Integer)
dtTotalBytes = lens _dtTotalBytes (\ s a -> s{_dtTotalBytes = a})

-- | The number of objects transferred between a Snowball and Amazon S3.
dtObjectsTransferred :: Lens' DataTransfer (Maybe Integer)
dtObjectsTransferred = lens _dtObjectsTransferred (\ s a -> s{_dtObjectsTransferred = a})

-- | The number of bytes transferred between a Snowball and Amazon S3.
dtBytesTransferred :: Lens' DataTransfer (Maybe Integer)
dtBytesTransferred = lens _dtBytesTransferred (\ s a -> s{_dtBytesTransferred = a})

instance FromJSON DataTransfer where
        parseJSON
          = withObject "DataTransfer"
              (\ x ->
                 DataTransfer' <$>
                   (x .:? "TotalObjects") <*> (x .:? "TotalBytes") <*>
                     (x .:? "ObjectsTransferred")
                     <*> (x .:? "BytesTransferred"))

instance Hashable DataTransfer where

instance NFData DataTransfer where

-- | The container for the 'EventTriggerDefinition$EventResourceARN' .
--
--
--
-- /See:/ 'eventTriggerDefinition' smart constructor.
newtype EventTriggerDefinition = EventTriggerDefinition'
  { _etdEventResourceARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventTriggerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etdEventResourceARN' - The Amazon Resource Name (ARN) for any local Amazon S3 resource that is an AWS Lambda function's event trigger associated with this job.
eventTriggerDefinition
    :: EventTriggerDefinition
eventTriggerDefinition =
  EventTriggerDefinition' {_etdEventResourceARN = Nothing}


-- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is an AWS Lambda function's event trigger associated with this job.
etdEventResourceARN :: Lens' EventTriggerDefinition (Maybe Text)
etdEventResourceARN = lens _etdEventResourceARN (\ s a -> s{_etdEventResourceARN = a})

instance FromJSON EventTriggerDefinition where
        parseJSON
          = withObject "EventTriggerDefinition"
              (\ x ->
                 EventTriggerDefinition' <$>
                   (x .:? "EventResourceARN"))

instance Hashable EventTriggerDefinition where

instance NFData EventTriggerDefinition where

instance ToJSON EventTriggerDefinition where
        toJSON EventTriggerDefinition'{..}
          = object
              (catMaybes
                 [("EventResourceARN" .=) <$> _etdEventResourceARN])

-- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of an export job.
--
--
--
-- /See:/ 'jobListEntry' smart constructor.
data JobListEntry = JobListEntry'
  { _jleJobType      :: !(Maybe JobType)
  , _jleJobId        :: !(Maybe Text)
  , _jleJobState     :: !(Maybe JobState)
  , _jleSnowballType :: !(Maybe SnowballType)
  , _jleCreationDate :: !(Maybe POSIX)
  , _jleDescription  :: !(Maybe Text)
  , _jleIsMaster     :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jleJobType' - The type of job.
--
-- * 'jleJobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'jleJobState' - The current state of this job.
--
-- * 'jleSnowballType' - The type of appliance used with this job.
--
-- * 'jleCreationDate' - The creation date for this job.
--
-- * 'jleDescription' - The optional description of this specific job, for example @Important Photos 2016-08-11@ .
--
-- * 'jleIsMaster' - A value that indicates that this job is a master job. A master job represents a successful request to create an export job. Master jobs aren't associated with any Snowballs. Instead, each master job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular master job are listed, because they are created after the master job is created.
jobListEntry
    :: JobListEntry
jobListEntry =
  JobListEntry'
    { _jleJobType = Nothing
    , _jleJobId = Nothing
    , _jleJobState = Nothing
    , _jleSnowballType = Nothing
    , _jleCreationDate = Nothing
    , _jleDescription = Nothing
    , _jleIsMaster = Nothing
    }


-- | The type of job.
jleJobType :: Lens' JobListEntry (Maybe JobType)
jleJobType = lens _jleJobType (\ s a -> s{_jleJobType = a})

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
jleJobId :: Lens' JobListEntry (Maybe Text)
jleJobId = lens _jleJobId (\ s a -> s{_jleJobId = a})

-- | The current state of this job.
jleJobState :: Lens' JobListEntry (Maybe JobState)
jleJobState = lens _jleJobState (\ s a -> s{_jleJobState = a})

-- | The type of appliance used with this job.
jleSnowballType :: Lens' JobListEntry (Maybe SnowballType)
jleSnowballType = lens _jleSnowballType (\ s a -> s{_jleSnowballType = a})

-- | The creation date for this job.
jleCreationDate :: Lens' JobListEntry (Maybe UTCTime)
jleCreationDate = lens _jleCreationDate (\ s a -> s{_jleCreationDate = a}) . mapping _Time

-- | The optional description of this specific job, for example @Important Photos 2016-08-11@ .
jleDescription :: Lens' JobListEntry (Maybe Text)
jleDescription = lens _jleDescription (\ s a -> s{_jleDescription = a})

-- | A value that indicates that this job is a master job. A master job represents a successful request to create an export job. Master jobs aren't associated with any Snowballs. Instead, each master job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular master job are listed, because they are created after the master job is created.
jleIsMaster :: Lens' JobListEntry (Maybe Bool)
jleIsMaster = lens _jleIsMaster (\ s a -> s{_jleIsMaster = a})

instance FromJSON JobListEntry where
        parseJSON
          = withObject "JobListEntry"
              (\ x ->
                 JobListEntry' <$>
                   (x .:? "JobType") <*> (x .:? "JobId") <*>
                     (x .:? "JobState")
                     <*> (x .:? "SnowballType")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Description")
                     <*> (x .:? "IsMaster"))

instance Hashable JobListEntry where

instance NFData JobListEntry where

-- | Contains job logs. Whenever Snowball is used to import data into or export data out of Amazon S3, you'll have the option of downloading a PDF job report. Job logs are returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type. The job logs can be accessed for up to 60 minutes after this request has been made. To access any of the job logs after 60 minutes have passed, you'll have to make another call to the @DescribeJob@ action.
--
--
-- For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snowball for your job part is being delivered to you.
--
-- The job report provides you insight into the state of your Amazon S3 data transfer. The report includes details about your job or job part for your records.
--
-- For deeper visibility into the status of your transferred objects, you can look at the two associated logs: a success log and a failure log. The logs are saved in comma-separated value (CSV) format, and the name of each log includes the ID of the job or job part that the log describes.
--
--
-- /See:/ 'jobLogs' smart constructor.
data JobLogs = JobLogs'
  { _jlJobFailureLogURI       :: !(Maybe Text)
  , _jlJobCompletionReportURI :: !(Maybe Text)
  , _jlJobSuccessLogURI       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jlJobFailureLogURI' - A link to an Amazon S3 presigned URL where the job failure log is located.
--
-- * 'jlJobCompletionReportURI' - A link to an Amazon S3 presigned URL where the job completion report is located.
--
-- * 'jlJobSuccessLogURI' - A link to an Amazon S3 presigned URL where the job success log is located.
jobLogs
    :: JobLogs
jobLogs =
  JobLogs'
    { _jlJobFailureLogURI = Nothing
    , _jlJobCompletionReportURI = Nothing
    , _jlJobSuccessLogURI = Nothing
    }


-- | A link to an Amazon S3 presigned URL where the job failure log is located.
jlJobFailureLogURI :: Lens' JobLogs (Maybe Text)
jlJobFailureLogURI = lens _jlJobFailureLogURI (\ s a -> s{_jlJobFailureLogURI = a})

-- | A link to an Amazon S3 presigned URL where the job completion report is located.
jlJobCompletionReportURI :: Lens' JobLogs (Maybe Text)
jlJobCompletionReportURI = lens _jlJobCompletionReportURI (\ s a -> s{_jlJobCompletionReportURI = a})

-- | A link to an Amazon S3 presigned URL where the job success log is located.
jlJobSuccessLogURI :: Lens' JobLogs (Maybe Text)
jlJobSuccessLogURI = lens _jlJobSuccessLogURI (\ s a -> s{_jlJobSuccessLogURI = a})

instance FromJSON JobLogs where
        parseJSON
          = withObject "JobLogs"
              (\ x ->
                 JobLogs' <$>
                   (x .:? "JobFailureLogURI") <*>
                     (x .:? "JobCompletionReportURI")
                     <*> (x .:? "JobSuccessLogURI"))

instance Hashable JobLogs where

instance NFData JobLogs where

-- | Contains information about a specific job including shipping information, job status, and other important metadata. This information is returned as a part of the response syntax of the @DescribeJob@ action.
--
--
--
-- /See:/ 'jobMetadata' smart constructor.
data JobMetadata = JobMetadata'
  { _jmJobType                    :: !(Maybe JobType)
  , _jmKMSKeyARN                  :: !(Maybe Text)
  , _jmJobId                      :: !(Maybe Text)
  , _jmJobLogInfo                 :: !(Maybe JobLogs)
  , _jmNotification               :: !(Maybe Notification)
  , _jmJobState                   :: !(Maybe JobState)
  , _jmForwardingAddressId        :: !(Maybe Text)
  , _jmShippingDetails            :: !(Maybe ShippingDetails)
  , _jmAddressId                  :: !(Maybe Text)
  , _jmSnowballType               :: !(Maybe SnowballType)
  , _jmDataTransferProgress       :: !(Maybe DataTransfer)
  , _jmResources                  :: !(Maybe JobResource)
  , _jmClusterId                  :: !(Maybe Text)
  , _jmCreationDate               :: !(Maybe POSIX)
  , _jmDescription                :: !(Maybe Text)
  , _jmRoleARN                    :: !(Maybe Text)
  , _jmSnowballCapacityPreference :: !(Maybe SnowballCapacity)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jmJobType' - The type of job.
--
-- * 'jmKMSKeyARN' - The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS KMS) key associated with this job. This ARN was created using the <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS KMS.
--
-- * 'jmJobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'jmJobLogInfo' - Links to Amazon S3 presigned URLs for the job report and logs. For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snowball for your job part is being delivered to you.
--
-- * 'jmNotification' - The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
--
-- * 'jmJobState' - The current status of the jobs.
--
-- * 'jmForwardingAddressId' - The ID of the address that you want a job shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
--
-- * 'jmShippingDetails' - A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
--
-- * 'jmAddressId' - The ID for the address that you want the Snowball shipped to.
--
-- * 'jmSnowballType' - The type of appliance used with this job.
--
-- * 'jmDataTransferProgress' - A value that defines the real-time status of a Snowball's data transfer while the appliance is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
--
-- * 'jmResources' - An array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
--
-- * 'jmClusterId' - The 39-character ID for the cluster, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'jmCreationDate' - The creation date for this job.
--
-- * 'jmDescription' - The description of the job, provided at job creation.
--
-- * 'jmRoleARN' - The role ARN associated with this job. This ARN was created using the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- * 'jmSnowballCapacityPreference' - The Snowball capacity preference for this job, specified at job creation. In US regions, you can choose between 50 TB and 80 TB Snowballs. All other regions use 80 TB capacity Snowballs.
jobMetadata
    :: JobMetadata
jobMetadata =
  JobMetadata'
    { _jmJobType = Nothing
    , _jmKMSKeyARN = Nothing
    , _jmJobId = Nothing
    , _jmJobLogInfo = Nothing
    , _jmNotification = Nothing
    , _jmJobState = Nothing
    , _jmForwardingAddressId = Nothing
    , _jmShippingDetails = Nothing
    , _jmAddressId = Nothing
    , _jmSnowballType = Nothing
    , _jmDataTransferProgress = Nothing
    , _jmResources = Nothing
    , _jmClusterId = Nothing
    , _jmCreationDate = Nothing
    , _jmDescription = Nothing
    , _jmRoleARN = Nothing
    , _jmSnowballCapacityPreference = Nothing
    }


-- | The type of job.
jmJobType :: Lens' JobMetadata (Maybe JobType)
jmJobType = lens _jmJobType (\ s a -> s{_jmJobType = a})

-- | The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS KMS) key associated with this job. This ARN was created using the <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS KMS.
jmKMSKeyARN :: Lens' JobMetadata (Maybe Text)
jmKMSKeyARN = lens _jmKMSKeyARN (\ s a -> s{_jmKMSKeyARN = a})

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
jmJobId :: Lens' JobMetadata (Maybe Text)
jmJobId = lens _jmJobId (\ s a -> s{_jmJobId = a})

-- | Links to Amazon S3 presigned URLs for the job report and logs. For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snowball for your job part is being delivered to you.
jmJobLogInfo :: Lens' JobMetadata (Maybe JobLogs)
jmJobLogInfo = lens _jmJobLogInfo (\ s a -> s{_jmJobLogInfo = a})

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
jmNotification :: Lens' JobMetadata (Maybe Notification)
jmNotification = lens _jmNotification (\ s a -> s{_jmNotification = a})

-- | The current status of the jobs.
jmJobState :: Lens' JobMetadata (Maybe JobState)
jmJobState = lens _jmJobState (\ s a -> s{_jmJobState = a})

-- | The ID of the address that you want a job shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
jmForwardingAddressId :: Lens' JobMetadata (Maybe Text)
jmForwardingAddressId = lens _jmForwardingAddressId (\ s a -> s{_jmForwardingAddressId = a})

-- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
jmShippingDetails :: Lens' JobMetadata (Maybe ShippingDetails)
jmShippingDetails = lens _jmShippingDetails (\ s a -> s{_jmShippingDetails = a})

-- | The ID for the address that you want the Snowball shipped to.
jmAddressId :: Lens' JobMetadata (Maybe Text)
jmAddressId = lens _jmAddressId (\ s a -> s{_jmAddressId = a})

-- | The type of appliance used with this job.
jmSnowballType :: Lens' JobMetadata (Maybe SnowballType)
jmSnowballType = lens _jmSnowballType (\ s a -> s{_jmSnowballType = a})

-- | A value that defines the real-time status of a Snowball's data transfer while the appliance is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
jmDataTransferProgress :: Lens' JobMetadata (Maybe DataTransfer)
jmDataTransferProgress = lens _jmDataTransferProgress (\ s a -> s{_jmDataTransferProgress = a})

-- | An array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
jmResources :: Lens' JobMetadata (Maybe JobResource)
jmResources = lens _jmResources (\ s a -> s{_jmResources = a})

-- | The 39-character ID for the cluster, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
jmClusterId :: Lens' JobMetadata (Maybe Text)
jmClusterId = lens _jmClusterId (\ s a -> s{_jmClusterId = a})

-- | The creation date for this job.
jmCreationDate :: Lens' JobMetadata (Maybe UTCTime)
jmCreationDate = lens _jmCreationDate (\ s a -> s{_jmCreationDate = a}) . mapping _Time

-- | The description of the job, provided at job creation.
jmDescription :: Lens' JobMetadata (Maybe Text)
jmDescription = lens _jmDescription (\ s a -> s{_jmDescription = a})

-- | The role ARN associated with this job. This ARN was created using the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
jmRoleARN :: Lens' JobMetadata (Maybe Text)
jmRoleARN = lens _jmRoleARN (\ s a -> s{_jmRoleARN = a})

-- | The Snowball capacity preference for this job, specified at job creation. In US regions, you can choose between 50 TB and 80 TB Snowballs. All other regions use 80 TB capacity Snowballs.
jmSnowballCapacityPreference :: Lens' JobMetadata (Maybe SnowballCapacity)
jmSnowballCapacityPreference = lens _jmSnowballCapacityPreference (\ s a -> s{_jmSnowballCapacityPreference = a})

instance FromJSON JobMetadata where
        parseJSON
          = withObject "JobMetadata"
              (\ x ->
                 JobMetadata' <$>
                   (x .:? "JobType") <*> (x .:? "KmsKeyARN") <*>
                     (x .:? "JobId")
                     <*> (x .:? "JobLogInfo")
                     <*> (x .:? "Notification")
                     <*> (x .:? "JobState")
                     <*> (x .:? "ForwardingAddressId")
                     <*> (x .:? "ShippingDetails")
                     <*> (x .:? "AddressId")
                     <*> (x .:? "SnowballType")
                     <*> (x .:? "DataTransferProgress")
                     <*> (x .:? "Resources")
                     <*> (x .:? "ClusterId")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Description")
                     <*> (x .:? "RoleARN")
                     <*> (x .:? "SnowballCapacityPreference"))

instance Hashable JobMetadata where

instance NFData JobMetadata where

-- | Contains an array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
--
--
--
-- /See:/ 'jobResource' smart constructor.
data JobResource = JobResource'
  { _jrLambdaResources :: !(Maybe [LambdaResource])
  , _jrS3Resources     :: !(Maybe [S3Resource])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jrLambdaResources' - The Python-language Lambda functions for this job.
--
-- * 'jrS3Resources' - An array of @S3Resource@ objects.
jobResource
    :: JobResource
jobResource =
  JobResource' {_jrLambdaResources = Nothing, _jrS3Resources = Nothing}


-- | The Python-language Lambda functions for this job.
jrLambdaResources :: Lens' JobResource [LambdaResource]
jrLambdaResources = lens _jrLambdaResources (\ s a -> s{_jrLambdaResources = a}) . _Default . _Coerce

-- | An array of @S3Resource@ objects.
jrS3Resources :: Lens' JobResource [S3Resource]
jrS3Resources = lens _jrS3Resources (\ s a -> s{_jrS3Resources = a}) . _Default . _Coerce

instance FromJSON JobResource where
        parseJSON
          = withObject "JobResource"
              (\ x ->
                 JobResource' <$>
                   (x .:? "LambdaResources" .!= mempty) <*>
                     (x .:? "S3Resources" .!= mempty))

instance Hashable JobResource where

instance NFData JobResource where

instance ToJSON JobResource where
        toJSON JobResource'{..}
          = object
              (catMaybes
                 [("LambdaResources" .=) <$> _jrLambdaResources,
                  ("S3Resources" .=) <$> _jrS3Resources])

-- | Contains a key range. For export jobs, a @S3Resource@ object can have an optional @KeyRange@ value. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
--
--
-- /See:/ 'keyRange' smart constructor.
data KeyRange = KeyRange'
  { _krEndMarker   :: !(Maybe Text)
  , _krBeginMarker :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeyRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'krEndMarker' - The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
--
-- * 'krBeginMarker' - The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
keyRange
    :: KeyRange
keyRange = KeyRange' {_krEndMarker = Nothing, _krBeginMarker = Nothing}


-- | The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
krEndMarker :: Lens' KeyRange (Maybe Text)
krEndMarker = lens _krEndMarker (\ s a -> s{_krEndMarker = a})

-- | The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
krBeginMarker :: Lens' KeyRange (Maybe Text)
krBeginMarker = lens _krBeginMarker (\ s a -> s{_krBeginMarker = a})

instance FromJSON KeyRange where
        parseJSON
          = withObject "KeyRange"
              (\ x ->
                 KeyRange' <$>
                   (x .:? "EndMarker") <*> (x .:? "BeginMarker"))

instance Hashable KeyRange where

instance NFData KeyRange where

instance ToJSON KeyRange where
        toJSON KeyRange'{..}
          = object
              (catMaybes
                 [("EndMarker" .=) <$> _krEndMarker,
                  ("BeginMarker" .=) <$> _krBeginMarker])

-- | Identifies
--
--
--
-- /See:/ 'lambdaResource' smart constructor.
data LambdaResource = LambdaResource'
  { _lrEventTriggers :: !(Maybe [EventTriggerDefinition])
  , _lrLambdaARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrEventTriggers' - The array of ARNs for 'S3Resource' objects to trigger the 'LambdaResource' objects associated with this job.
--
-- * 'lrLambdaARN' - An Amazon Resource Name (ARN) that represents an AWS Lambda function to be triggered by PUT object actions on the associated local Amazon S3 resource.
lambdaResource
    :: LambdaResource
lambdaResource =
  LambdaResource' {_lrEventTriggers = Nothing, _lrLambdaARN = Nothing}


-- | The array of ARNs for 'S3Resource' objects to trigger the 'LambdaResource' objects associated with this job.
lrEventTriggers :: Lens' LambdaResource [EventTriggerDefinition]
lrEventTriggers = lens _lrEventTriggers (\ s a -> s{_lrEventTriggers = a}) . _Default . _Coerce

-- | An Amazon Resource Name (ARN) that represents an AWS Lambda function to be triggered by PUT object actions on the associated local Amazon S3 resource.
lrLambdaARN :: Lens' LambdaResource (Maybe Text)
lrLambdaARN = lens _lrLambdaARN (\ s a -> s{_lrLambdaARN = a})

instance FromJSON LambdaResource where
        parseJSON
          = withObject "LambdaResource"
              (\ x ->
                 LambdaResource' <$>
                   (x .:? "EventTriggers" .!= mempty) <*>
                     (x .:? "LambdaArn"))

instance Hashable LambdaResource where

instance NFData LambdaResource where

instance ToJSON LambdaResource where
        toJSON LambdaResource'{..}
          = object
              (catMaybes
                 [("EventTriggers" .=) <$> _lrEventTriggers,
                  ("LambdaArn" .=) <$> _lrLambdaARN])

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
--
--
-- When the notification settings are defined during job creation, you can choose to notify based on a specific set of job states using the @JobStatesToNotify@ array of strings, or you can specify that you want to have Amazon SNS notifications sent out for all job states with @NotifyAll@ set to true.
--
--
-- /See:/ 'notification' smart constructor.
data Notification = Notification'
  { _nNotifyAll         :: !(Maybe Bool)
  , _nSNSTopicARN       :: !(Maybe Text)
  , _nJobStatesToNotify :: !(Maybe [JobState])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Notification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nNotifyAll' - Any change in job state will trigger a notification for this job.
--
-- * 'nSNSTopicARN' - The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <http://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action. You can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <http://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
--
-- * 'nJobStatesToNotify' - The list of job states that will trigger a notification for this job.
notification
    :: Notification
notification =
  Notification'
    { _nNotifyAll = Nothing
    , _nSNSTopicARN = Nothing
    , _nJobStatesToNotify = Nothing
    }


-- | Any change in job state will trigger a notification for this job.
nNotifyAll :: Lens' Notification (Maybe Bool)
nNotifyAll = lens _nNotifyAll (\ s a -> s{_nNotifyAll = a})

-- | The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <http://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action. You can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <http://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
nSNSTopicARN :: Lens' Notification (Maybe Text)
nSNSTopicARN = lens _nSNSTopicARN (\ s a -> s{_nSNSTopicARN = a})

-- | The list of job states that will trigger a notification for this job.
nJobStatesToNotify :: Lens' Notification [JobState]
nJobStatesToNotify = lens _nJobStatesToNotify (\ s a -> s{_nJobStatesToNotify = a}) . _Default . _Coerce

instance FromJSON Notification where
        parseJSON
          = withObject "Notification"
              (\ x ->
                 Notification' <$>
                   (x .:? "NotifyAll") <*> (x .:? "SnsTopicARN") <*>
                     (x .:? "JobStatesToNotify" .!= mempty))

instance Hashable Notification where

instance NFData Notification where

instance ToJSON Notification where
        toJSON Notification'{..}
          = object
              (catMaybes
                 [("NotifyAll" .=) <$> _nNotifyAll,
                  ("SnsTopicARN" .=) <$> _nSNSTopicARN,
                  ("JobStatesToNotify" .=) <$> _nJobStatesToNotify])

-- | Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into. For export jobs, this object can have an optional @KeyRange@ value. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
--
--
-- /See:/ 's3Resource' smart constructor.
data S3Resource = S3Resource'
  { _srKeyRange  :: !(Maybe KeyRange)
  , _srBucketARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srKeyRange' - For export jobs, you can provide an optional @KeyRange@ within a specific Amazon S3 bucket. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
-- * 'srBucketARN' - The Amazon Resource Name (ARN) of an Amazon S3 bucket.
s3Resource
    :: S3Resource
s3Resource = S3Resource' {_srKeyRange = Nothing, _srBucketARN = Nothing}


-- | For export jobs, you can provide an optional @KeyRange@ within a specific Amazon S3 bucket. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
srKeyRange :: Lens' S3Resource (Maybe KeyRange)
srKeyRange = lens _srKeyRange (\ s a -> s{_srKeyRange = a})

-- | The Amazon Resource Name (ARN) of an Amazon S3 bucket.
srBucketARN :: Lens' S3Resource (Maybe Text)
srBucketARN = lens _srBucketARN (\ s a -> s{_srBucketARN = a})

instance FromJSON S3Resource where
        parseJSON
          = withObject "S3Resource"
              (\ x ->
                 S3Resource' <$>
                   (x .:? "KeyRange") <*> (x .:? "BucketArn"))

instance Hashable S3Resource where

instance NFData S3Resource where

instance ToJSON S3Resource where
        toJSON S3Resource'{..}
          = object
              (catMaybes
                 [("KeyRange" .=) <$> _srKeyRange,
                  ("BucketArn" .=) <$> _srBucketARN])

-- | The @Status@ and @TrackingNumber@ information for an inbound or outbound shipment.
--
--
--
-- /See:/ 'shipment' smart constructor.
data Shipment = Shipment'
  { _sStatus         :: !(Maybe Text)
  , _sTrackingNumber :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Shipment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - Status information for a shipment.
--
-- * 'sTrackingNumber' - The tracking number for this job. Using this tracking number with your region's carrier's website, you can track a Snowball as the carrier transports it. For India, the carrier is Amazon Logistics. For all other regions, UPS is the carrier.
shipment
    :: Shipment
shipment = Shipment' {_sStatus = Nothing, _sTrackingNumber = Nothing}


-- | Status information for a shipment.
sStatus :: Lens' Shipment (Maybe Text)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a})

-- | The tracking number for this job. Using this tracking number with your region's carrier's website, you can track a Snowball as the carrier transports it. For India, the carrier is Amazon Logistics. For all other regions, UPS is the carrier.
sTrackingNumber :: Lens' Shipment (Maybe Text)
sTrackingNumber = lens _sTrackingNumber (\ s a -> s{_sTrackingNumber = a})

instance FromJSON Shipment where
        parseJSON
          = withObject "Shipment"
              (\ x ->
                 Shipment' <$>
                   (x .:? "Status") <*> (x .:? "TrackingNumber"))

instance Hashable Shipment where

instance NFData Shipment where

-- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
--
--
--
-- /See:/ 'shippingDetails' smart constructor.
data ShippingDetails = ShippingDetails'
  { _sdShippingOption   :: !(Maybe ShippingOption)
  , _sdOutboundShipment :: !(Maybe Shipment)
  , _sdInboundShipment  :: !(Maybe Shipment)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ShippingDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdShippingOption' - The shipping speed for a particular job. This speed doesn't dictate how soon you'll get the Snowball from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowballs are delivered in one to seven days.     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
--
-- * 'sdOutboundShipment' - The @Status@ and @TrackingNumber@ values for a Snowball being delivered to the address that you specified for a particular job.
--
-- * 'sdInboundShipment' - The @Status@ and @TrackingNumber@ values for a Snowball being returned to AWS for a particular job.
shippingDetails
    :: ShippingDetails
shippingDetails =
  ShippingDetails'
    { _sdShippingOption = Nothing
    , _sdOutboundShipment = Nothing
    , _sdInboundShipment = Nothing
    }


-- | The shipping speed for a particular job. This speed doesn't dictate how soon you'll get the Snowball from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowballs are delivered in one to seven days.     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
sdShippingOption :: Lens' ShippingDetails (Maybe ShippingOption)
sdShippingOption = lens _sdShippingOption (\ s a -> s{_sdShippingOption = a})

-- | The @Status@ and @TrackingNumber@ values for a Snowball being delivered to the address that you specified for a particular job.
sdOutboundShipment :: Lens' ShippingDetails (Maybe Shipment)
sdOutboundShipment = lens _sdOutboundShipment (\ s a -> s{_sdOutboundShipment = a})

-- | The @Status@ and @TrackingNumber@ values for a Snowball being returned to AWS for a particular job.
sdInboundShipment :: Lens' ShippingDetails (Maybe Shipment)
sdInboundShipment = lens _sdInboundShipment (\ s a -> s{_sdInboundShipment = a})

instance FromJSON ShippingDetails where
        parseJSON
          = withObject "ShippingDetails"
              (\ x ->
                 ShippingDetails' <$>
                   (x .:? "ShippingOption") <*>
                     (x .:? "OutboundShipment")
                     <*> (x .:? "InboundShipment"))

instance Hashable ShippingDetails where

instance NFData ShippingDetails where
