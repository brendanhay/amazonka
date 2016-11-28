{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Snowball.Types.Sum

-- | The address that you want the Snowball or Snowballs associated with a specific job to be shipped to. Addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. Although no individual elements of the @Address@ are required, if the address is invalid or unsupported, then an exception is thrown.
--
--
--
-- /See:/ 'address' smart constructor.
data Address = Address'
    { _aStreet3              :: !(Maybe Text)
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aStreet3' - The third line in a street address that a Snowball is to be delivered to.
--
-- * 'aLandmark' - A landmark listed in an address that a Snowball is to be delivered to.
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
-- * 'aPrefectureOrDistrict' - The prefecture or district in an address that a Snowball is to be delivered to.
--
-- * 'aStreet1' - The first line in a street address that a Snowball is to be delivered to.
address
    :: Address
address =
    Address'
    { _aStreet3 = Nothing
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

-- | The third line in a street address that a Snowball is to be delivered to.
aStreet3 :: Lens' Address (Maybe Text)
aStreet3 = lens _aStreet3 (\ s a -> s{_aStreet3 = a});

-- | A landmark listed in an address that a Snowball is to be delivered to.
aLandmark :: Lens' Address (Maybe Text)
aLandmark = lens _aLandmark (\ s a -> s{_aLandmark = a});

-- | The postal code in an address that a Snowball is to be delivered to.
aPostalCode :: Lens' Address (Maybe Text)
aPostalCode = lens _aPostalCode (\ s a -> s{_aPostalCode = a});

-- | The country in an address that a Snowball is to be delivered to.
aCountry :: Lens' Address (Maybe Text)
aCountry = lens _aCountry (\ s a -> s{_aCountry = a});

-- | The state or province in an address that a Snowball is to be delivered to.
aStateOrProvince :: Lens' Address (Maybe Text)
aStateOrProvince = lens _aStateOrProvince (\ s a -> s{_aStateOrProvince = a});

-- | The second line in a street address that a Snowball is to be delivered to.
aStreet2 :: Lens' Address (Maybe Text)
aStreet2 = lens _aStreet2 (\ s a -> s{_aStreet2 = a});

-- | The unique ID for an address.
aAddressId :: Lens' Address (Maybe Text)
aAddressId = lens _aAddressId (\ s a -> s{_aAddressId = a});

-- | The city in an address that a Snowball is to be delivered to.
aCity :: Lens' Address (Maybe Text)
aCity = lens _aCity (\ s a -> s{_aCity = a});

-- | The phone number associated with an address that a Snowball is to be delivered to.
aPhoneNumber :: Lens' Address (Maybe Text)
aPhoneNumber = lens _aPhoneNumber (\ s a -> s{_aPhoneNumber = a});

-- | The name of the company to receive a Snowball at an address.
aCompany :: Lens' Address (Maybe Text)
aCompany = lens _aCompany (\ s a -> s{_aCompany = a});

-- | The name of a person to receive a Snowball at an address.
aName :: Lens' Address (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | The prefecture or district in an address that a Snowball is to be delivered to.
aPrefectureOrDistrict :: Lens' Address (Maybe Text)
aPrefectureOrDistrict = lens _aPrefectureOrDistrict (\ s a -> s{_aPrefectureOrDistrict = a});

-- | The first line in a street address that a Snowball is to be delivered to.
aStreet1 :: Lens' Address (Maybe Text)
aStreet1 = lens _aStreet1 (\ s a -> s{_aStreet1 = a});

instance FromJSON Address where
        parseJSON
          = withObject "Address"
              (\ x ->
                 Address' <$>
                   (x .:? "Street3") <*> (x .:? "Landmark") <*>
                     (x .:? "PostalCode")
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

instance Hashable Address

instance NFData Address

instance ToJSON Address where
        toJSON Address'{..}
          = object
              (catMaybes
                 [("Street3" .=) <$> _aStreet3,
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

-- | Defines the real-time status of a Snowball's data transfer while the appliance is at AWS. Note that this data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
--
--
--
-- /See:/ 'dataTransfer' smart constructor.
data DataTransfer = DataTransfer'
    { _dtTotalObjects       :: !(Maybe Integer)
    , _dtTotalBytes         :: !(Maybe Integer)
    , _dtObjectsTransferred :: !(Maybe Integer)
    , _dtBytesTransferred   :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
dtTotalObjects = lens _dtTotalObjects (\ s a -> s{_dtTotalObjects = a});

-- | The total bytes of data for a transfer between a Snowball and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
dtTotalBytes :: Lens' DataTransfer (Maybe Integer)
dtTotalBytes = lens _dtTotalBytes (\ s a -> s{_dtTotalBytes = a});

-- | The number of objects transferred between a Snowball and Amazon S3.
dtObjectsTransferred :: Lens' DataTransfer (Maybe Integer)
dtObjectsTransferred = lens _dtObjectsTransferred (\ s a -> s{_dtObjectsTransferred = a});

-- | The number of bytes transferred between a Snowball and Amazon S3.
dtBytesTransferred :: Lens' DataTransfer (Maybe Integer)
dtBytesTransferred = lens _dtBytesTransferred (\ s a -> s{_dtBytesTransferred = a});

instance FromJSON DataTransfer where
        parseJSON
          = withObject "DataTransfer"
              (\ x ->
                 DataTransfer' <$>
                   (x .:? "TotalObjects") <*> (x .:? "TotalBytes") <*>
                     (x .:? "ObjectsTransferred")
                     <*> (x .:? "BytesTransferred"))

instance Hashable DataTransfer

instance NFData DataTransfer

-- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of an export job.
--
--
--
-- /See:/ 'jobListEntry' smart constructor.
data JobListEntry = JobListEntry'
    { _jleJobId    :: !(Maybe Text)
    , _jleJobState :: !(Maybe JobState)
    , _jleIsMaster :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'JobListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jleJobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'jleJobState' - The current state of this job.
--
-- * 'jleIsMaster' - A value that indicates that this job is a master job. A master job represents a successful request to create an export job. Master jobs aren't associated with any Snowballs. Instead, each master job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular master job are listed, because they are created after the master job is created.
jobListEntry
    :: JobListEntry
jobListEntry =
    JobListEntry'
    { _jleJobId = Nothing
    , _jleJobState = Nothing
    , _jleIsMaster = Nothing
    }

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
jleJobId :: Lens' JobListEntry (Maybe Text)
jleJobId = lens _jleJobId (\ s a -> s{_jleJobId = a});

-- | The current state of this job.
jleJobState :: Lens' JobListEntry (Maybe JobState)
jleJobState = lens _jleJobState (\ s a -> s{_jleJobState = a});

-- | A value that indicates that this job is a master job. A master job represents a successful request to create an export job. Master jobs aren't associated with any Snowballs. Instead, each master job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular master job are listed, because they are created after the master job is created.
jleIsMaster :: Lens' JobListEntry (Maybe Bool)
jleIsMaster = lens _jleIsMaster (\ s a -> s{_jleIsMaster = a});

instance FromJSON JobListEntry where
        parseJSON
          = withObject "JobListEntry"
              (\ x ->
                 JobListEntry' <$>
                   (x .:? "JobId") <*> (x .:? "JobState") <*>
                     (x .:? "IsMaster"))

instance Hashable JobListEntry

instance NFData JobListEntry

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
jlJobFailureLogURI = lens _jlJobFailureLogURI (\ s a -> s{_jlJobFailureLogURI = a});

-- | A link to an Amazon S3 presigned URL where the job completion report is located.
jlJobCompletionReportURI :: Lens' JobLogs (Maybe Text)
jlJobCompletionReportURI = lens _jlJobCompletionReportURI (\ s a -> s{_jlJobCompletionReportURI = a});

-- | A link to an Amazon S3 presigned URL where the job success log is located.
jlJobSuccessLogURI :: Lens' JobLogs (Maybe Text)
jlJobSuccessLogURI = lens _jlJobSuccessLogURI (\ s a -> s{_jlJobSuccessLogURI = a});

instance FromJSON JobLogs where
        parseJSON
          = withObject "JobLogs"
              (\ x ->
                 JobLogs' <$>
                   (x .:? "JobFailureLogURI") <*>
                     (x .:? "JobCompletionReportURI")
                     <*> (x .:? "JobSuccessLogURI"))

instance Hashable JobLogs

instance NFData JobLogs

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
    , _jmShippingDetails            :: !(Maybe ShippingDetails)
    , _jmAddressId                  :: !(Maybe Text)
    , _jmDataTransferProgress       :: !(Maybe DataTransfer)
    , _jmResources                  :: !(Maybe JobResource)
    , _jmCreationDate               :: !(Maybe POSIX)
    , _jmDescription                :: !(Maybe Text)
    , _jmRoleARN                    :: !(Maybe Text)
    , _jmSnowballCapacityPreference :: !(Maybe SnowballCapacity)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'JobMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jmJobType' - The type of job.
--
-- * 'jmKMSKeyARN' - The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS KMS) key associated with this job. This ARN was created using the @CreateKey@ API action in AWS KMS.
--
-- * 'jmJobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'jmJobLogInfo' - Links to Amazon S3 presigned URLs for the job report and logs. For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snowball for your job part is being delivered to you.
--
-- * 'jmNotification' - The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
--
-- * 'jmJobState' - The current state of the jobs.
--
-- * 'jmShippingDetails' - A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
--
-- * 'jmAddressId' - The ID for the address that you want the Snowball shipped to.
--
-- * 'jmDataTransferProgress' - A value that defines the real-time status of a Snowball's data transfer while the appliance is at AWS. Note that this data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
--
-- * 'jmResources' - An array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
--
-- * 'jmCreationDate' - The creation date for this job.
--
-- * 'jmDescription' - The description of the job, provided at job creation.
--
-- * 'jmRoleARN' - The role ARN associated with this job. This ARN was created using the @CreateRole@ API action in AWS Identity and Access Management (IAM).
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
    , _jmShippingDetails = Nothing
    , _jmAddressId = Nothing
    , _jmDataTransferProgress = Nothing
    , _jmResources = Nothing
    , _jmCreationDate = Nothing
    , _jmDescription = Nothing
    , _jmRoleARN = Nothing
    , _jmSnowballCapacityPreference = Nothing
    }

-- | The type of job.
jmJobType :: Lens' JobMetadata (Maybe JobType)
jmJobType = lens _jmJobType (\ s a -> s{_jmJobType = a});

-- | The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS KMS) key associated with this job. This ARN was created using the @CreateKey@ API action in AWS KMS.
jmKMSKeyARN :: Lens' JobMetadata (Maybe Text)
jmKMSKeyARN = lens _jmKMSKeyARN (\ s a -> s{_jmKMSKeyARN = a});

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
jmJobId :: Lens' JobMetadata (Maybe Text)
jmJobId = lens _jmJobId (\ s a -> s{_jmJobId = a});

-- | Links to Amazon S3 presigned URLs for the job report and logs. For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snowball for your job part is being delivered to you.
jmJobLogInfo :: Lens' JobMetadata (Maybe JobLogs)
jmJobLogInfo = lens _jmJobLogInfo (\ s a -> s{_jmJobLogInfo = a});

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
jmNotification :: Lens' JobMetadata (Maybe Notification)
jmNotification = lens _jmNotification (\ s a -> s{_jmNotification = a});

-- | The current state of the jobs.
jmJobState :: Lens' JobMetadata (Maybe JobState)
jmJobState = lens _jmJobState (\ s a -> s{_jmJobState = a});

-- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
jmShippingDetails :: Lens' JobMetadata (Maybe ShippingDetails)
jmShippingDetails = lens _jmShippingDetails (\ s a -> s{_jmShippingDetails = a});

-- | The ID for the address that you want the Snowball shipped to.
jmAddressId :: Lens' JobMetadata (Maybe Text)
jmAddressId = lens _jmAddressId (\ s a -> s{_jmAddressId = a});

-- | A value that defines the real-time status of a Snowball's data transfer while the appliance is at AWS. Note that this data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
jmDataTransferProgress :: Lens' JobMetadata (Maybe DataTransfer)
jmDataTransferProgress = lens _jmDataTransferProgress (\ s a -> s{_jmDataTransferProgress = a});

-- | An array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
jmResources :: Lens' JobMetadata (Maybe JobResource)
jmResources = lens _jmResources (\ s a -> s{_jmResources = a});

-- | The creation date for this job.
jmCreationDate :: Lens' JobMetadata (Maybe UTCTime)
jmCreationDate = lens _jmCreationDate (\ s a -> s{_jmCreationDate = a}) . mapping _Time;

-- | The description of the job, provided at job creation.
jmDescription :: Lens' JobMetadata (Maybe Text)
jmDescription = lens _jmDescription (\ s a -> s{_jmDescription = a});

-- | The role ARN associated with this job. This ARN was created using the @CreateRole@ API action in AWS Identity and Access Management (IAM).
jmRoleARN :: Lens' JobMetadata (Maybe Text)
jmRoleARN = lens _jmRoleARN (\ s a -> s{_jmRoleARN = a});

-- | The Snowball capacity preference for this job, specified at job creation. In US regions, you can choose between 50 TB and 80 TB Snowballs. All other regions use 80 TB capacity Snowballs.
jmSnowballCapacityPreference :: Lens' JobMetadata (Maybe SnowballCapacity)
jmSnowballCapacityPreference = lens _jmSnowballCapacityPreference (\ s a -> s{_jmSnowballCapacityPreference = a});

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
                     <*> (x .:? "ShippingDetails")
                     <*> (x .:? "AddressId")
                     <*> (x .:? "DataTransferProgress")
                     <*> (x .:? "Resources")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Description")
                     <*> (x .:? "RoleARN")
                     <*> (x .:? "SnowballCapacityPreference"))

instance Hashable JobMetadata

instance NFData JobMetadata

-- | Contains an array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
--
--
--
-- /See:/ 'jobResource' smart constructor.
newtype JobResource = JobResource'
    { _jrS3Resources :: Maybe [S3Resource]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'JobResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jrS3Resources' - An array of @S3Resource@ objects.
jobResource
    :: JobResource
jobResource =
    JobResource'
    { _jrS3Resources = Nothing
    }

-- | An array of @S3Resource@ objects.
jrS3Resources :: Lens' JobResource [S3Resource]
jrS3Resources = lens _jrS3Resources (\ s a -> s{_jrS3Resources = a}) . _Default . _Coerce;

instance FromJSON JobResource where
        parseJSON
          = withObject "JobResource"
              (\ x ->
                 JobResource' <$> (x .:? "S3Resources" .!= mempty))

instance Hashable JobResource

instance NFData JobResource

instance ToJSON JobResource where
        toJSON JobResource'{..}
          = object
              (catMaybes [("S3Resources" .=) <$> _jrS3Resources])

-- | Contains a key range. For export jobs, a @S3Resource@ object can have an optional @KeyRange@ value. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
--
--
-- /See:/ 'keyRange' smart constructor.
data KeyRange = KeyRange'
    { _krEndMarker   :: !(Maybe Text)
    , _krBeginMarker :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'krEndMarker' - The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
--
-- * 'krBeginMarker' - The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
keyRange
    :: KeyRange
keyRange =
    KeyRange'
    { _krEndMarker = Nothing
    , _krBeginMarker = Nothing
    }

-- | The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
krEndMarker :: Lens' KeyRange (Maybe Text)
krEndMarker = lens _krEndMarker (\ s a -> s{_krEndMarker = a});

-- | The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
krBeginMarker :: Lens' KeyRange (Maybe Text)
krBeginMarker = lens _krBeginMarker (\ s a -> s{_krBeginMarker = a});

instance FromJSON KeyRange where
        parseJSON
          = withObject "KeyRange"
              (\ x ->
                 KeyRange' <$>
                   (x .:? "EndMarker") <*> (x .:? "BeginMarker"))

instance Hashable KeyRange

instance NFData KeyRange

instance ToJSON KeyRange where
        toJSON KeyRange'{..}
          = object
              (catMaybes
                 [("EndMarker" .=) <$> _krEndMarker,
                  ("BeginMarker" .=) <$> _krBeginMarker])

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Notification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nNotifyAll' - Any change in job state will trigger a notification for this job.
--
-- * 'nSNSTopicARN' - The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <http://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action. Note that you can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <http://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
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
nNotifyAll = lens _nNotifyAll (\ s a -> s{_nNotifyAll = a});

-- | The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <http://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action. Note that you can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <http://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
nSNSTopicARN :: Lens' Notification (Maybe Text)
nSNSTopicARN = lens _nSNSTopicARN (\ s a -> s{_nSNSTopicARN = a});

-- | The list of job states that will trigger a notification for this job.
nJobStatesToNotify :: Lens' Notification [JobState]
nJobStatesToNotify = lens _nJobStatesToNotify (\ s a -> s{_nJobStatesToNotify = a}) . _Default . _Coerce;

instance FromJSON Notification where
        parseJSON
          = withObject "Notification"
              (\ x ->
                 Notification' <$>
                   (x .:? "NotifyAll") <*> (x .:? "SnsTopicARN") <*>
                     (x .:? "JobStatesToNotify" .!= mempty))

instance Hashable Notification

instance NFData Notification

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srKeyRange' - For export jobs, you can provide an optional @KeyRange@ within a specific Amazon S3 bucket. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
-- * 'srBucketARN' - The Amazon Resource Name (ARN) of an Amazon S3 bucket.
s3Resource
    :: S3Resource
s3Resource =
    S3Resource'
    { _srKeyRange = Nothing
    , _srBucketARN = Nothing
    }

-- | For export jobs, you can provide an optional @KeyRange@ within a specific Amazon S3 bucket. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
srKeyRange :: Lens' S3Resource (Maybe KeyRange)
srKeyRange = lens _srKeyRange (\ s a -> s{_srKeyRange = a});

-- | The Amazon Resource Name (ARN) of an Amazon S3 bucket.
srBucketARN :: Lens' S3Resource (Maybe Text)
srBucketARN = lens _srBucketARN (\ s a -> s{_srBucketARN = a});

instance FromJSON S3Resource where
        parseJSON
          = withObject "S3Resource"
              (\ x ->
                 S3Resource' <$>
                   (x .:? "KeyRange") <*> (x .:? "BucketArn"))

instance Hashable S3Resource

instance NFData S3Resource

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Shipment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - Status information for a shipment. Valid statuses include @NEW@ , @IN_TRANSIT@ , and @DELIVERED@ .
--
-- * 'sTrackingNumber' - The tracking number for this job. Using this tracking number with your region's carrier's website, you can track a Snowball as the carrier transports it. For India, the carrier is Amazon Logistics. For all other regions, UPS is the carrier.
shipment
    :: Shipment
shipment =
    Shipment'
    { _sStatus = Nothing
    , _sTrackingNumber = Nothing
    }

-- | Status information for a shipment. Valid statuses include @NEW@ , @IN_TRANSIT@ , and @DELIVERED@ .
sStatus :: Lens' Shipment (Maybe Text)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a});

-- | The tracking number for this job. Using this tracking number with your region's carrier's website, you can track a Snowball as the carrier transports it. For India, the carrier is Amazon Logistics. For all other regions, UPS is the carrier.
sTrackingNumber :: Lens' Shipment (Maybe Text)
sTrackingNumber = lens _sTrackingNumber (\ s a -> s{_sTrackingNumber = a});

instance FromJSON Shipment where
        parseJSON
          = withObject "Shipment"
              (\ x ->
                 Shipment' <$>
                   (x .:? "Status") <*> (x .:? "TrackingNumber"))

instance Hashable Shipment

instance NFData Shipment

-- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
--
--
--
-- /See:/ 'shippingDetails' smart constructor.
data ShippingDetails = ShippingDetails'
    { _sdShippingOption   :: !(Maybe ShippingOption)
    , _sdOutboundShipment :: !(Maybe Shipment)
    , _sdInboundShipment  :: !(Maybe Shipment)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdShippingOption' - The shipping speed for a particular job. Note that this speed does not dictate how soon you'll get the Snowball from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowballs are delivered in one to seven days.     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
--
-- * 'sdOutboundShipment' - The @Status@ and @TrackingNumber@ values for a Snowball being returned to AWS for a particular job.
--
-- * 'sdInboundShipment' - The @Status@ and @TrackingNumber@ values for a Snowball being delivered to the address that you specified for a particular job.
shippingDetails
    :: ShippingDetails
shippingDetails =
    ShippingDetails'
    { _sdShippingOption = Nothing
    , _sdOutboundShipment = Nothing
    , _sdInboundShipment = Nothing
    }

-- | The shipping speed for a particular job. Note that this speed does not dictate how soon you'll get the Snowball from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowballs are delivered in one to seven days.     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
sdShippingOption :: Lens' ShippingDetails (Maybe ShippingOption)
sdShippingOption = lens _sdShippingOption (\ s a -> s{_sdShippingOption = a});

-- | The @Status@ and @TrackingNumber@ values for a Snowball being returned to AWS for a particular job.
sdOutboundShipment :: Lens' ShippingDetails (Maybe Shipment)
sdOutboundShipment = lens _sdOutboundShipment (\ s a -> s{_sdOutboundShipment = a});

-- | The @Status@ and @TrackingNumber@ values for a Snowball being delivered to the address that you specified for a particular job.
sdInboundShipment :: Lens' ShippingDetails (Maybe Shipment)
sdInboundShipment = lens _sdInboundShipment (\ s a -> s{_sdInboundShipment = a});

instance FromJSON ShippingDetails where
        parseJSON
          = withObject "ShippingDetails"
              (\ x ->
                 ShippingDetails' <$>
                   (x .:? "ShippingOption") <*>
                     (x .:? "OutboundShipment")
                     <*> (x .:? "InboundShipment"))

instance Hashable ShippingDetails

instance NFData ShippingDetails
