{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Product where

import Network.AWS.AlexaBusiness.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An address book with attributes.
--
--
--
-- /See:/ 'addressBook' smart constructor.
data AddressBook = AddressBook'
  { _abAddressBookARN :: !(Maybe Text)
  , _abName           :: !(Maybe Text)
  , _abDescription    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddressBook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abAddressBookARN' - The ARN of the address book.
--
-- * 'abName' - The name of the address book.
--
-- * 'abDescription' - The description of the address book.
addressBook
    :: AddressBook
addressBook =
  AddressBook'
    {_abAddressBookARN = Nothing, _abName = Nothing, _abDescription = Nothing}


-- | The ARN of the address book.
abAddressBookARN :: Lens' AddressBook (Maybe Text)
abAddressBookARN = lens _abAddressBookARN (\ s a -> s{_abAddressBookARN = a})

-- | The name of the address book.
abName :: Lens' AddressBook (Maybe Text)
abName = lens _abName (\ s a -> s{_abName = a})

-- | The description of the address book.
abDescription :: Lens' AddressBook (Maybe Text)
abDescription = lens _abDescription (\ s a -> s{_abDescription = a})

instance FromJSON AddressBook where
        parseJSON
          = withObject "AddressBook"
              (\ x ->
                 AddressBook' <$>
                   (x .:? "AddressBookArn") <*> (x .:? "Name") <*>
                     (x .:? "Description"))

instance Hashable AddressBook where

instance NFData AddressBook where

-- | Information related to an address book.
--
--
--
-- /See:/ 'addressBookData' smart constructor.
data AddressBookData = AddressBookData'
  { _abdAddressBookARN :: !(Maybe Text)
  , _abdName           :: !(Maybe Text)
  , _abdDescription    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddressBookData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abdAddressBookARN' - The ARN of the address book.
--
-- * 'abdName' - The name of the address book.
--
-- * 'abdDescription' - The description of the address book.
addressBookData
    :: AddressBookData
addressBookData =
  AddressBookData'
    { _abdAddressBookARN = Nothing
    , _abdName = Nothing
    , _abdDescription = Nothing
    }


-- | The ARN of the address book.
abdAddressBookARN :: Lens' AddressBookData (Maybe Text)
abdAddressBookARN = lens _abdAddressBookARN (\ s a -> s{_abdAddressBookARN = a})

-- | The name of the address book.
abdName :: Lens' AddressBookData (Maybe Text)
abdName = lens _abdName (\ s a -> s{_abdName = a})

-- | The description of the address book.
abdDescription :: Lens' AddressBookData (Maybe Text)
abdDescription = lens _abdDescription (\ s a -> s{_abdDescription = a})

instance FromJSON AddressBookData where
        parseJSON
          = withObject "AddressBookData"
              (\ x ->
                 AddressBookData' <$>
                   (x .:? "AddressBookArn") <*> (x .:? "Name") <*>
                     (x .:? "Description"))

instance Hashable AddressBookData where

instance NFData AddressBookData where

-- | Usage report with specified parameters.
--
--
--
-- /See:/ 'businessReport' smart constructor.
data BusinessReport = BusinessReport'
  { _brStatus       :: !(Maybe BusinessReportStatus)
  , _brFailureCode  :: !(Maybe BusinessReportFailureCode)
  , _brDeliveryTime :: !(Maybe POSIX)
  , _brDownloadURL  :: !(Maybe Text)
  , _brS3Location   :: !(Maybe BusinessReportS3Location)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brStatus' - The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
--
-- * 'brFailureCode' - The failure code.
--
-- * 'brDeliveryTime' - The time of report delivery.
--
-- * 'brDownloadURL' - The download link where a user can download the report.
--
-- * 'brS3Location' - The S3 location of the output reports.
businessReport
    :: BusinessReport
businessReport =
  BusinessReport'
    { _brStatus = Nothing
    , _brFailureCode = Nothing
    , _brDeliveryTime = Nothing
    , _brDownloadURL = Nothing
    , _brS3Location = Nothing
    }


-- | The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
brStatus :: Lens' BusinessReport (Maybe BusinessReportStatus)
brStatus = lens _brStatus (\ s a -> s{_brStatus = a})

-- | The failure code.
brFailureCode :: Lens' BusinessReport (Maybe BusinessReportFailureCode)
brFailureCode = lens _brFailureCode (\ s a -> s{_brFailureCode = a})

-- | The time of report delivery.
brDeliveryTime :: Lens' BusinessReport (Maybe UTCTime)
brDeliveryTime = lens _brDeliveryTime (\ s a -> s{_brDeliveryTime = a}) . mapping _Time

-- | The download link where a user can download the report.
brDownloadURL :: Lens' BusinessReport (Maybe Text)
brDownloadURL = lens _brDownloadURL (\ s a -> s{_brDownloadURL = a})

-- | The S3 location of the output reports.
brS3Location :: Lens' BusinessReport (Maybe BusinessReportS3Location)
brS3Location = lens _brS3Location (\ s a -> s{_brS3Location = a})

instance FromJSON BusinessReport where
        parseJSON
          = withObject "BusinessReport"
              (\ x ->
                 BusinessReport' <$>
                   (x .:? "Status") <*> (x .:? "FailureCode") <*>
                     (x .:? "DeliveryTime")
                     <*> (x .:? "DownloadUrl")
                     <*> (x .:? "S3Location"))

instance Hashable BusinessReport where

instance NFData BusinessReport where

-- | The content range of the report.
--
--
--
-- /See:/ 'businessReportContentRange' smart constructor.
newtype BusinessReportContentRange = BusinessReportContentRange'
  { _brcrInterval :: Maybe BusinessReportInterval
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReportContentRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brcrInterval' - The interval of the content range.
businessReportContentRange
    :: BusinessReportContentRange
businessReportContentRange =
  BusinessReportContentRange' {_brcrInterval = Nothing}


-- | The interval of the content range.
brcrInterval :: Lens' BusinessReportContentRange (Maybe BusinessReportInterval)
brcrInterval = lens _brcrInterval (\ s a -> s{_brcrInterval = a})

instance FromJSON BusinessReportContentRange where
        parseJSON
          = withObject "BusinessReportContentRange"
              (\ x ->
                 BusinessReportContentRange' <$> (x .:? "Interval"))

instance Hashable BusinessReportContentRange where

instance NFData BusinessReportContentRange where

instance ToJSON BusinessReportContentRange where
        toJSON BusinessReportContentRange'{..}
          = object
              (catMaybes [("Interval" .=) <$> _brcrInterval])

-- | The recurrence of the reports.
--
--
--
-- /See:/ 'businessReportRecurrence' smart constructor.
newtype BusinessReportRecurrence = BusinessReportRecurrence'
  { _brrStartDate :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReportRecurrence' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brrStartDate' - The start date.
businessReportRecurrence
    :: BusinessReportRecurrence
businessReportRecurrence = BusinessReportRecurrence' {_brrStartDate = Nothing}


-- | The start date.
brrStartDate :: Lens' BusinessReportRecurrence (Maybe Text)
brrStartDate = lens _brrStartDate (\ s a -> s{_brrStartDate = a})

instance FromJSON BusinessReportRecurrence where
        parseJSON
          = withObject "BusinessReportRecurrence"
              (\ x ->
                 BusinessReportRecurrence' <$> (x .:? "StartDate"))

instance Hashable BusinessReportRecurrence where

instance NFData BusinessReportRecurrence where

instance ToJSON BusinessReportRecurrence where
        toJSON BusinessReportRecurrence'{..}
          = object
              (catMaybes [("StartDate" .=) <$> _brrStartDate])

-- | The S3 location of the output reports.
--
--
--
-- /See:/ 'businessReportS3Location' smart constructor.
data BusinessReportS3Location = BusinessReportS3Location'
  { _brslPath       :: !(Maybe Text)
  , _brslBucketName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReportS3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brslPath' - The path of the business report.
--
-- * 'brslBucketName' - The S3 bucket name of the output reports.
businessReportS3Location
    :: BusinessReportS3Location
businessReportS3Location =
  BusinessReportS3Location' {_brslPath = Nothing, _brslBucketName = Nothing}


-- | The path of the business report.
brslPath :: Lens' BusinessReportS3Location (Maybe Text)
brslPath = lens _brslPath (\ s a -> s{_brslPath = a})

-- | The S3 bucket name of the output reports.
brslBucketName :: Lens' BusinessReportS3Location (Maybe Text)
brslBucketName = lens _brslBucketName (\ s a -> s{_brslBucketName = a})

instance FromJSON BusinessReportS3Location where
        parseJSON
          = withObject "BusinessReportS3Location"
              (\ x ->
                 BusinessReportS3Location' <$>
                   (x .:? "Path") <*> (x .:? "BucketName"))

instance Hashable BusinessReportS3Location where

instance NFData BusinessReportS3Location where

-- | The schedule of the usage report.
--
--
--
-- /See:/ 'businessReportSchedule' smart constructor.
data BusinessReportSchedule = BusinessReportSchedule'
  { _brsS3KeyPrefix        :: !(Maybe Text)
  , _brsLastBusinessReport :: !(Maybe BusinessReport)
  , _brsFormat             :: !(Maybe BusinessReportFormat)
  , _brsRecurrence         :: !(Maybe BusinessReportRecurrence)
  , _brsScheduleName       :: !(Maybe Text)
  , _brsScheduleARN        :: !(Maybe Text)
  , _brsContentRange       :: !(Maybe BusinessReportContentRange)
  , _brsS3BucketName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReportSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsS3KeyPrefix' - The S3 key where the report is delivered.
--
-- * 'brsLastBusinessReport' - The details of the last business report delivery for a specified time interval.
--
-- * 'brsFormat' - The format of the generated report (individual CSV files or zipped files of individual files).
--
-- * 'brsRecurrence' - The recurrence of the reports.
--
-- * 'brsScheduleName' - The name identifier of the schedule.
--
-- * 'brsScheduleARN' - The ARN of the business report schedule.
--
-- * 'brsContentRange' - The content range of the reports.
--
-- * 'brsS3BucketName' - The S3 bucket name of the output reports.
businessReportSchedule
    :: BusinessReportSchedule
businessReportSchedule =
  BusinessReportSchedule'
    { _brsS3KeyPrefix = Nothing
    , _brsLastBusinessReport = Nothing
    , _brsFormat = Nothing
    , _brsRecurrence = Nothing
    , _brsScheduleName = Nothing
    , _brsScheduleARN = Nothing
    , _brsContentRange = Nothing
    , _brsS3BucketName = Nothing
    }


-- | The S3 key where the report is delivered.
brsS3KeyPrefix :: Lens' BusinessReportSchedule (Maybe Text)
brsS3KeyPrefix = lens _brsS3KeyPrefix (\ s a -> s{_brsS3KeyPrefix = a})

-- | The details of the last business report delivery for a specified time interval.
brsLastBusinessReport :: Lens' BusinessReportSchedule (Maybe BusinessReport)
brsLastBusinessReport = lens _brsLastBusinessReport (\ s a -> s{_brsLastBusinessReport = a})

-- | The format of the generated report (individual CSV files or zipped files of individual files).
brsFormat :: Lens' BusinessReportSchedule (Maybe BusinessReportFormat)
brsFormat = lens _brsFormat (\ s a -> s{_brsFormat = a})

-- | The recurrence of the reports.
brsRecurrence :: Lens' BusinessReportSchedule (Maybe BusinessReportRecurrence)
brsRecurrence = lens _brsRecurrence (\ s a -> s{_brsRecurrence = a})

-- | The name identifier of the schedule.
brsScheduleName :: Lens' BusinessReportSchedule (Maybe Text)
brsScheduleName = lens _brsScheduleName (\ s a -> s{_brsScheduleName = a})

-- | The ARN of the business report schedule.
brsScheduleARN :: Lens' BusinessReportSchedule (Maybe Text)
brsScheduleARN = lens _brsScheduleARN (\ s a -> s{_brsScheduleARN = a})

-- | The content range of the reports.
brsContentRange :: Lens' BusinessReportSchedule (Maybe BusinessReportContentRange)
brsContentRange = lens _brsContentRange (\ s a -> s{_brsContentRange = a})

-- | The S3 bucket name of the output reports.
brsS3BucketName :: Lens' BusinessReportSchedule (Maybe Text)
brsS3BucketName = lens _brsS3BucketName (\ s a -> s{_brsS3BucketName = a})

instance FromJSON BusinessReportSchedule where
        parseJSON
          = withObject "BusinessReportSchedule"
              (\ x ->
                 BusinessReportSchedule' <$>
                   (x .:? "S3KeyPrefix") <*>
                     (x .:? "LastBusinessReport")
                     <*> (x .:? "Format")
                     <*> (x .:? "Recurrence")
                     <*> (x .:? "ScheduleName")
                     <*> (x .:? "ScheduleArn")
                     <*> (x .:? "ContentRange")
                     <*> (x .:? "S3BucketName"))

instance Hashable BusinessReportSchedule where

instance NFData BusinessReportSchedule where

-- | The skill store category that is shown. Alexa skills are assigned a specific skill category during creation, such as News, Social, and Sports.
--
--
--
-- /See:/ 'category' smart constructor.
data Category = Category'
  { _cCategoryName :: !(Maybe Text)
  , _cCategoryId   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Category' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCategoryName' - The name of the skill store category.
--
-- * 'cCategoryId' - The ID of the skill store category.
category
    :: Category
category = Category' {_cCategoryName = Nothing, _cCategoryId = Nothing}


-- | The name of the skill store category.
cCategoryName :: Lens' Category (Maybe Text)
cCategoryName = lens _cCategoryName (\ s a -> s{_cCategoryName = a})

-- | The ID of the skill store category.
cCategoryId :: Lens' Category (Maybe Natural)
cCategoryId = lens _cCategoryId (\ s a -> s{_cCategoryId = a}) . mapping _Nat

instance FromJSON Category where
        parseJSON
          = withObject "Category"
              (\ x ->
                 Category' <$>
                   (x .:? "CategoryName") <*> (x .:? "CategoryId"))

instance Hashable Category where

instance NFData Category where

-- | The default conference provider that is used if no other scheduled meetings are detected.
--
--
--
-- /See:/ 'conferencePreference' smart constructor.
newtype ConferencePreference = ConferencePreference'
  { _cpDefaultConferenceProviderARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConferencePreference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpDefaultConferenceProviderARN' - The ARN of the default conference provider.
conferencePreference
    :: ConferencePreference
conferencePreference =
  ConferencePreference' {_cpDefaultConferenceProviderARN = Nothing}


-- | The ARN of the default conference provider.
cpDefaultConferenceProviderARN :: Lens' ConferencePreference (Maybe Text)
cpDefaultConferenceProviderARN = lens _cpDefaultConferenceProviderARN (\ s a -> s{_cpDefaultConferenceProviderARN = a})

instance FromJSON ConferencePreference where
        parseJSON
          = withObject "ConferencePreference"
              (\ x ->
                 ConferencePreference' <$>
                   (x .:? "DefaultConferenceProviderArn"))

instance Hashable ConferencePreference where

instance NFData ConferencePreference where

instance ToJSON ConferencePreference where
        toJSON ConferencePreference'{..}
          = object
              (catMaybes
                 [("DefaultConferenceProviderArn" .=) <$>
                    _cpDefaultConferenceProviderARN])

-- | An entity that provides a conferencing solution. Alexa for Business acts as the voice interface and mediator that connects users to their preferred conference provider. Examples of conference providers include Amazon Chime, Zoom, Cisco, and Polycom.
--
--
--
-- /See:/ 'conferenceProvider' smart constructor.
data ConferenceProvider = ConferenceProvider'
  { _cpMeetingSetting :: !(Maybe MeetingSetting)
  , _cpARN            :: !(Maybe Text)
  , _cpPSTNDialIn     :: !(Maybe PSTNDialIn)
  , _cpName           :: !(Maybe Text)
  , _cpType           :: !(Maybe ConferenceProviderType)
  , _cpIPDialIn       :: !(Maybe IPDialIn)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConferenceProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpMeetingSetting' - The meeting settings for the conference provider.
--
-- * 'cpARN' - The ARN of the newly created conference provider.
--
-- * 'cpPSTNDialIn' - The information for PSTN conferencing.
--
-- * 'cpName' - The name of the conference provider.
--
-- * 'cpType' - The type of conference providers.
--
-- * 'cpIPDialIn' - The IP endpoint and protocol for calling.
conferenceProvider
    :: ConferenceProvider
conferenceProvider =
  ConferenceProvider'
    { _cpMeetingSetting = Nothing
    , _cpARN = Nothing
    , _cpPSTNDialIn = Nothing
    , _cpName = Nothing
    , _cpType = Nothing
    , _cpIPDialIn = Nothing
    }


-- | The meeting settings for the conference provider.
cpMeetingSetting :: Lens' ConferenceProvider (Maybe MeetingSetting)
cpMeetingSetting = lens _cpMeetingSetting (\ s a -> s{_cpMeetingSetting = a})

-- | The ARN of the newly created conference provider.
cpARN :: Lens' ConferenceProvider (Maybe Text)
cpARN = lens _cpARN (\ s a -> s{_cpARN = a})

-- | The information for PSTN conferencing.
cpPSTNDialIn :: Lens' ConferenceProvider (Maybe PSTNDialIn)
cpPSTNDialIn = lens _cpPSTNDialIn (\ s a -> s{_cpPSTNDialIn = a})

-- | The name of the conference provider.
cpName :: Lens' ConferenceProvider (Maybe Text)
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | The type of conference providers.
cpType :: Lens' ConferenceProvider (Maybe ConferenceProviderType)
cpType = lens _cpType (\ s a -> s{_cpType = a})

-- | The IP endpoint and protocol for calling.
cpIPDialIn :: Lens' ConferenceProvider (Maybe IPDialIn)
cpIPDialIn = lens _cpIPDialIn (\ s a -> s{_cpIPDialIn = a})

instance FromJSON ConferenceProvider where
        parseJSON
          = withObject "ConferenceProvider"
              (\ x ->
                 ConferenceProvider' <$>
                   (x .:? "MeetingSetting") <*> (x .:? "Arn") <*>
                     (x .:? "PSTNDialIn")
                     <*> (x .:? "Name")
                     <*> (x .:? "Type")
                     <*> (x .:? "IPDialIn"))

instance Hashable ConferenceProvider where

instance NFData ConferenceProvider where

-- | A contact with attributes.
--
--
--
-- /See:/ 'contact' smart constructor.
data Contact = Contact'
  { _cLastName    :: !(Maybe Text)
  , _cContactARN  :: !(Maybe Text)
  , _cPhoneNumber :: !(Maybe (Sensitive Text))
  , _cFirstName   :: !(Maybe Text)
  , _cDisplayName :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Contact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLastName' - The last name of the contact, used to call the contact on the device.
--
-- * 'cContactARN' - The ARN of the contact.
--
-- * 'cPhoneNumber' - The phone number of the contact.
--
-- * 'cFirstName' - The first name of the contact, used to call the contact on the device.
--
-- * 'cDisplayName' - The name of the contact to display on the console.
contact
    :: Contact
contact =
  Contact'
    { _cLastName = Nothing
    , _cContactARN = Nothing
    , _cPhoneNumber = Nothing
    , _cFirstName = Nothing
    , _cDisplayName = Nothing
    }


-- | The last name of the contact, used to call the contact on the device.
cLastName :: Lens' Contact (Maybe Text)
cLastName = lens _cLastName (\ s a -> s{_cLastName = a})

-- | The ARN of the contact.
cContactARN :: Lens' Contact (Maybe Text)
cContactARN = lens _cContactARN (\ s a -> s{_cContactARN = a})

-- | The phone number of the contact.
cPhoneNumber :: Lens' Contact (Maybe Text)
cPhoneNumber = lens _cPhoneNumber (\ s a -> s{_cPhoneNumber = a}) . mapping _Sensitive

-- | The first name of the contact, used to call the contact on the device.
cFirstName :: Lens' Contact (Maybe Text)
cFirstName = lens _cFirstName (\ s a -> s{_cFirstName = a})

-- | The name of the contact to display on the console.
cDisplayName :: Lens' Contact (Maybe Text)
cDisplayName = lens _cDisplayName (\ s a -> s{_cDisplayName = a})

instance FromJSON Contact where
        parseJSON
          = withObject "Contact"
              (\ x ->
                 Contact' <$>
                   (x .:? "LastName") <*> (x .:? "ContactArn") <*>
                     (x .:? "PhoneNumber")
                     <*> (x .:? "FirstName")
                     <*> (x .:? "DisplayName"))

instance Hashable Contact where

instance NFData Contact where

-- | Information related to a contact.
--
--
--
-- /See:/ 'contactData' smart constructor.
data ContactData = ContactData'
  { _cdLastName    :: !(Maybe Text)
  , _cdContactARN  :: !(Maybe Text)
  , _cdPhoneNumber :: !(Maybe (Sensitive Text))
  , _cdFirstName   :: !(Maybe Text)
  , _cdDisplayName :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContactData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdLastName' - The last name of the contact, used to call the contact on the device.
--
-- * 'cdContactARN' - The ARN of the contact.
--
-- * 'cdPhoneNumber' - The phone number of the contact.
--
-- * 'cdFirstName' - The first name of the contact, used to call the contact on the device.
--
-- * 'cdDisplayName' - The name of the contact to display on the console.
contactData
    :: ContactData
contactData =
  ContactData'
    { _cdLastName = Nothing
    , _cdContactARN = Nothing
    , _cdPhoneNumber = Nothing
    , _cdFirstName = Nothing
    , _cdDisplayName = Nothing
    }


-- | The last name of the contact, used to call the contact on the device.
cdLastName :: Lens' ContactData (Maybe Text)
cdLastName = lens _cdLastName (\ s a -> s{_cdLastName = a})

-- | The ARN of the contact.
cdContactARN :: Lens' ContactData (Maybe Text)
cdContactARN = lens _cdContactARN (\ s a -> s{_cdContactARN = a})

-- | The phone number of the contact.
cdPhoneNumber :: Lens' ContactData (Maybe Text)
cdPhoneNumber = lens _cdPhoneNumber (\ s a -> s{_cdPhoneNumber = a}) . mapping _Sensitive

-- | The first name of the contact, used to call the contact on the device.
cdFirstName :: Lens' ContactData (Maybe Text)
cdFirstName = lens _cdFirstName (\ s a -> s{_cdFirstName = a})

-- | The name of the contact to display on the console.
cdDisplayName :: Lens' ContactData (Maybe Text)
cdDisplayName = lens _cdDisplayName (\ s a -> s{_cdDisplayName = a})

instance FromJSON ContactData where
        parseJSON
          = withObject "ContactData"
              (\ x ->
                 ContactData' <$>
                   (x .:? "LastName") <*> (x .:? "ContactArn") <*>
                     (x .:? "PhoneNumber")
                     <*> (x .:? "FirstName")
                     <*> (x .:? "DisplayName"))

instance Hashable ContactData where

instance NFData ContactData where

-- | The details about the developer that published the skill.
--
--
--
-- /See:/ 'developerInfo' smart constructor.
data DeveloperInfo = DeveloperInfo'
  { _diEmail         :: !(Maybe Text)
  , _diURL           :: !(Maybe Text)
  , _diPrivacyPolicy :: !(Maybe Text)
  , _diDeveloperName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeveloperInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diEmail' - The email of the developer.
--
-- * 'diURL' - The website of the developer.
--
-- * 'diPrivacyPolicy' - The URL of the privacy policy.
--
-- * 'diDeveloperName' - The name of the developer.
developerInfo
    :: DeveloperInfo
developerInfo =
  DeveloperInfo'
    { _diEmail = Nothing
    , _diURL = Nothing
    , _diPrivacyPolicy = Nothing
    , _diDeveloperName = Nothing
    }


-- | The email of the developer.
diEmail :: Lens' DeveloperInfo (Maybe Text)
diEmail = lens _diEmail (\ s a -> s{_diEmail = a})

-- | The website of the developer.
diURL :: Lens' DeveloperInfo (Maybe Text)
diURL = lens _diURL (\ s a -> s{_diURL = a})

-- | The URL of the privacy policy.
diPrivacyPolicy :: Lens' DeveloperInfo (Maybe Text)
diPrivacyPolicy = lens _diPrivacyPolicy (\ s a -> s{_diPrivacyPolicy = a})

-- | The name of the developer.
diDeveloperName :: Lens' DeveloperInfo (Maybe Text)
diDeveloperName = lens _diDeveloperName (\ s a -> s{_diDeveloperName = a})

instance FromJSON DeveloperInfo where
        parseJSON
          = withObject "DeveloperInfo"
              (\ x ->
                 DeveloperInfo' <$>
                   (x .:? "Email") <*> (x .:? "Url") <*>
                     (x .:? "PrivacyPolicy")
                     <*> (x .:? "DeveloperName"))

instance Hashable DeveloperInfo where

instance NFData DeveloperInfo where

-- | A device with attributes.
--
--
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dDeviceStatus       :: !(Maybe DeviceStatus)
  , _dDeviceStatusInfo   :: !(Maybe DeviceStatusInfo)
  , _dDeviceARN          :: !(Maybe Text)
  , _dMACAddress         :: !(Maybe Text)
  , _dDeviceName         :: !(Maybe Text)
  , _dRoomARN            :: !(Maybe Text)
  , _dSoftwareVersion    :: !(Maybe Text)
  , _dDeviceType         :: !(Maybe Text)
  , _dDeviceSerialNumber :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeviceStatus' - The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
--
-- * 'dDeviceStatusInfo' - Detailed information about a device's status.
--
-- * 'dDeviceARN' - The ARN of a device.
--
-- * 'dMACAddress' - The MAC address of a device.
--
-- * 'dDeviceName' - The name of a device.
--
-- * 'dRoomARN' - The room ARN of a device.
--
-- * 'dSoftwareVersion' - The software version of a device.
--
-- * 'dDeviceType' - The type of a device.
--
-- * 'dDeviceSerialNumber' - The serial number of a device.
device
    :: Device
device =
  Device'
    { _dDeviceStatus = Nothing
    , _dDeviceStatusInfo = Nothing
    , _dDeviceARN = Nothing
    , _dMACAddress = Nothing
    , _dDeviceName = Nothing
    , _dRoomARN = Nothing
    , _dSoftwareVersion = Nothing
    , _dDeviceType = Nothing
    , _dDeviceSerialNumber = Nothing
    }


-- | The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
dDeviceStatus :: Lens' Device (Maybe DeviceStatus)
dDeviceStatus = lens _dDeviceStatus (\ s a -> s{_dDeviceStatus = a})

-- | Detailed information about a device's status.
dDeviceStatusInfo :: Lens' Device (Maybe DeviceStatusInfo)
dDeviceStatusInfo = lens _dDeviceStatusInfo (\ s a -> s{_dDeviceStatusInfo = a})

-- | The ARN of a device.
dDeviceARN :: Lens' Device (Maybe Text)
dDeviceARN = lens _dDeviceARN (\ s a -> s{_dDeviceARN = a})

-- | The MAC address of a device.
dMACAddress :: Lens' Device (Maybe Text)
dMACAddress = lens _dMACAddress (\ s a -> s{_dMACAddress = a})

-- | The name of a device.
dDeviceName :: Lens' Device (Maybe Text)
dDeviceName = lens _dDeviceName (\ s a -> s{_dDeviceName = a})

-- | The room ARN of a device.
dRoomARN :: Lens' Device (Maybe Text)
dRoomARN = lens _dRoomARN (\ s a -> s{_dRoomARN = a})

-- | The software version of a device.
dSoftwareVersion :: Lens' Device (Maybe Text)
dSoftwareVersion = lens _dSoftwareVersion (\ s a -> s{_dSoftwareVersion = a})

-- | The type of a device.
dDeviceType :: Lens' Device (Maybe Text)
dDeviceType = lens _dDeviceType (\ s a -> s{_dDeviceType = a})

-- | The serial number of a device.
dDeviceSerialNumber :: Lens' Device (Maybe Text)
dDeviceSerialNumber = lens _dDeviceSerialNumber (\ s a -> s{_dDeviceSerialNumber = a})

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "DeviceStatus") <*> (x .:? "DeviceStatusInfo")
                     <*> (x .:? "DeviceArn")
                     <*> (x .:? "MacAddress")
                     <*> (x .:? "DeviceName")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "SoftwareVersion")
                     <*> (x .:? "DeviceType")
                     <*> (x .:? "DeviceSerialNumber"))

instance Hashable Device where

instance NFData Device where

-- | Device attributes.
--
--
--
-- /See:/ 'deviceData' smart constructor.
data DeviceData = DeviceData'
  { _ddDeviceStatus       :: !(Maybe DeviceStatus)
  , _ddDeviceStatusInfo   :: !(Maybe DeviceStatusInfo)
  , _ddDeviceARN          :: !(Maybe Text)
  , _ddMACAddress         :: !(Maybe Text)
  , _ddDeviceName         :: !(Maybe Text)
  , _ddRoomARN            :: !(Maybe Text)
  , _ddSoftwareVersion    :: !(Maybe Text)
  , _ddDeviceType         :: !(Maybe Text)
  , _ddRoomName           :: !(Maybe Text)
  , _ddDeviceSerialNumber :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDeviceStatus' - The status of a device.
--
-- * 'ddDeviceStatusInfo' - Detailed information about a device's status.
--
-- * 'ddDeviceARN' - The ARN of a device.
--
-- * 'ddMACAddress' - The MAC address of a device.
--
-- * 'ddDeviceName' - The name of a device.
--
-- * 'ddRoomARN' - The room ARN associated with a device.
--
-- * 'ddSoftwareVersion' - The software version of a device.
--
-- * 'ddDeviceType' - The type of a device.
--
-- * 'ddRoomName' - The name of the room associated with a device.
--
-- * 'ddDeviceSerialNumber' - The serial number of a device.
deviceData
    :: DeviceData
deviceData =
  DeviceData'
    { _ddDeviceStatus = Nothing
    , _ddDeviceStatusInfo = Nothing
    , _ddDeviceARN = Nothing
    , _ddMACAddress = Nothing
    , _ddDeviceName = Nothing
    , _ddRoomARN = Nothing
    , _ddSoftwareVersion = Nothing
    , _ddDeviceType = Nothing
    , _ddRoomName = Nothing
    , _ddDeviceSerialNumber = Nothing
    }


-- | The status of a device.
ddDeviceStatus :: Lens' DeviceData (Maybe DeviceStatus)
ddDeviceStatus = lens _ddDeviceStatus (\ s a -> s{_ddDeviceStatus = a})

-- | Detailed information about a device's status.
ddDeviceStatusInfo :: Lens' DeviceData (Maybe DeviceStatusInfo)
ddDeviceStatusInfo = lens _ddDeviceStatusInfo (\ s a -> s{_ddDeviceStatusInfo = a})

-- | The ARN of a device.
ddDeviceARN :: Lens' DeviceData (Maybe Text)
ddDeviceARN = lens _ddDeviceARN (\ s a -> s{_ddDeviceARN = a})

-- | The MAC address of a device.
ddMACAddress :: Lens' DeviceData (Maybe Text)
ddMACAddress = lens _ddMACAddress (\ s a -> s{_ddMACAddress = a})

-- | The name of a device.
ddDeviceName :: Lens' DeviceData (Maybe Text)
ddDeviceName = lens _ddDeviceName (\ s a -> s{_ddDeviceName = a})

-- | The room ARN associated with a device.
ddRoomARN :: Lens' DeviceData (Maybe Text)
ddRoomARN = lens _ddRoomARN (\ s a -> s{_ddRoomARN = a})

-- | The software version of a device.
ddSoftwareVersion :: Lens' DeviceData (Maybe Text)
ddSoftwareVersion = lens _ddSoftwareVersion (\ s a -> s{_ddSoftwareVersion = a})

-- | The type of a device.
ddDeviceType :: Lens' DeviceData (Maybe Text)
ddDeviceType = lens _ddDeviceType (\ s a -> s{_ddDeviceType = a})

-- | The name of the room associated with a device.
ddRoomName :: Lens' DeviceData (Maybe Text)
ddRoomName = lens _ddRoomName (\ s a -> s{_ddRoomName = a})

-- | The serial number of a device.
ddDeviceSerialNumber :: Lens' DeviceData (Maybe Text)
ddDeviceSerialNumber = lens _ddDeviceSerialNumber (\ s a -> s{_ddDeviceSerialNumber = a})

instance FromJSON DeviceData where
        parseJSON
          = withObject "DeviceData"
              (\ x ->
                 DeviceData' <$>
                   (x .:? "DeviceStatus") <*> (x .:? "DeviceStatusInfo")
                     <*> (x .:? "DeviceArn")
                     <*> (x .:? "MacAddress")
                     <*> (x .:? "DeviceName")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "SoftwareVersion")
                     <*> (x .:? "DeviceType")
                     <*> (x .:? "RoomName")
                     <*> (x .:? "DeviceSerialNumber"))

instance Hashable DeviceData where

instance NFData DeviceData where

-- | The list of device events.
--
--
--
-- /See:/ 'deviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { _deValue     :: !(Maybe Text)
  , _deType      :: !(Maybe DeviceEventType)
  , _deTimestamp :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deValue' - The value of the event.
--
-- * 'deType' - The type of device event.
--
-- * 'deTimestamp' - The time (in epoch) when the event occurred.
deviceEvent
    :: DeviceEvent
deviceEvent =
  DeviceEvent' {_deValue = Nothing, _deType = Nothing, _deTimestamp = Nothing}


-- | The value of the event.
deValue :: Lens' DeviceEvent (Maybe Text)
deValue = lens _deValue (\ s a -> s{_deValue = a})

-- | The type of device event.
deType :: Lens' DeviceEvent (Maybe DeviceEventType)
deType = lens _deType (\ s a -> s{_deType = a})

-- | The time (in epoch) when the event occurred.
deTimestamp :: Lens' DeviceEvent (Maybe UTCTime)
deTimestamp = lens _deTimestamp (\ s a -> s{_deTimestamp = a}) . mapping _Time

instance FromJSON DeviceEvent where
        parseJSON
          = withObject "DeviceEvent"
              (\ x ->
                 DeviceEvent' <$>
                   (x .:? "Value") <*> (x .:? "Type") <*>
                     (x .:? "Timestamp"))

instance Hashable DeviceEvent where

instance NFData DeviceEvent where

-- | Details of a device
