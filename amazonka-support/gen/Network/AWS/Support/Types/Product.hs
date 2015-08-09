{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.Support.Types.Sum

-- | An attachment to a case communication. The attachment consists of the
-- file name and the content of the file.
--
-- /See:/ 'attachment' smart constructor.
data Attachment = Attachment'
    { _aData     :: !(Maybe Base64)
    , _aFileName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Attachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aData'
--
-- * 'aFileName'
attachment
    :: Attachment
attachment =
    Attachment'
    { _aData = Nothing
    , _aFileName = Nothing
    }

-- | The content of the attachment file.
aData :: Lens' Attachment (Maybe ByteString)
aData = lens _aData (\ s a -> s{_aData = a}) . mapping _Base64;

-- | The name of the attachment file.
aFileName :: Lens' Attachment (Maybe Text)
aFileName = lens _aFileName (\ s a -> s{_aFileName = a});

instance FromJSON Attachment where
        parseJSON
          = withObject "Attachment"
              (\ x ->
                 Attachment' <$>
                   (x .:? "data") <*> (x .:? "fileName"))

instance ToJSON Attachment where
        toJSON Attachment'{..}
          = object ["data" .= _aData, "fileName" .= _aFileName]

-- | The file name and ID of an attachment to a case communication. You can
-- use the ID to retrieve the attachment with the DescribeAttachment
-- operation.
--
-- /See:/ 'attachmentDetails' smart constructor.
data AttachmentDetails = AttachmentDetails'
    { _adAttachmentId :: !(Maybe Text)
    , _adFileName     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachmentDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAttachmentId'
--
-- * 'adFileName'
attachmentDetails
    :: AttachmentDetails
attachmentDetails =
    AttachmentDetails'
    { _adAttachmentId = Nothing
    , _adFileName = Nothing
    }

-- | The ID of the attachment.
adAttachmentId :: Lens' AttachmentDetails (Maybe Text)
adAttachmentId = lens _adAttachmentId (\ s a -> s{_adAttachmentId = a});

-- | The file name of the attachment.
adFileName :: Lens' AttachmentDetails (Maybe Text)
adFileName = lens _adFileName (\ s a -> s{_adFileName = a});

instance FromJSON AttachmentDetails where
        parseJSON
          = withObject "AttachmentDetails"
              (\ x ->
                 AttachmentDetails' <$>
                   (x .:? "attachmentId") <*> (x .:? "fileName"))

-- | A JSON-formatted object that contains the metadata for a support case.
-- It is contained the response from a DescribeCases request.
-- __CaseDetails__ contains the following fields:
--
-- 1.  __CaseID.__ The AWS Support case ID requested or returned in the
--     call. The case ID is an alphanumeric string formatted as shown in
--     this example: case-/12345678910-2013-c4c1d2bf33c5cf47/.
-- 2.  __CategoryCode.__ The category of problem for the AWS Support case.
--     Corresponds to the CategoryCode values returned by a call to
--     DescribeServices.
-- 3.  __DisplayId.__ The identifier for the case on pages in the AWS
--     Support Center.
-- 4.  __Language.__ The ISO 639-1 code for the language in which AWS
--     provides support. AWS Support currently supports English (\"en\")
--     and Japanese (\"ja\"). Language parameters must be passed explicitly
--     for operations that take them.
-- 5.  __RecentCommunications.__ One or more Communication objects. Fields
--     of these objects are 'Attachments', 'Body', 'CaseId', 'SubmittedBy',
--     and 'TimeCreated'.
-- 6.  __NextToken.__ A resumption point for pagination.
-- 7.  __ServiceCode.__ The identifier for the AWS service that corresponds
--     to the service code defined in the call to DescribeServices.
-- 8.  __SeverityCode.__ The severity code assigned to the case. Contains
--     one of the values returned by the call to DescribeSeverityLevels.
-- 9.  __Status.__ The status of the case in the AWS Support Center.
-- 10. __Subject.__ The subject line of the case.
-- 11. __SubmittedBy.__ The email address of the account that submitted the
--     case.
-- 12. __TimeCreated.__ The time the case was created, in ISO-8601 format.
--
-- /See:/ 'caseDetails' smart constructor.
data CaseDetails = CaseDetails'
    { _cdSubject              :: !(Maybe Text)
    , _cdStatus               :: !(Maybe Text)
    , _cdRecentCommunications :: !(Maybe RecentCaseCommunications)
    , _cdSeverityCode         :: !(Maybe Text)
    , _cdCaseId               :: !(Maybe Text)
    , _cdCcEmailAddresses     :: !(Maybe [Text])
    , _cdDisplayId            :: !(Maybe Text)
    , _cdSubmittedBy          :: !(Maybe Text)
    , _cdLanguage             :: !(Maybe Text)
    , _cdCategoryCode         :: !(Maybe Text)
    , _cdTimeCreated          :: !(Maybe Text)
    , _cdServiceCode          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CaseDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdSubject'
--
-- * 'cdStatus'
--
-- * 'cdRecentCommunications'
--
-- * 'cdSeverityCode'
--
-- * 'cdCaseId'
--
-- * 'cdCcEmailAddresses'
--
-- * 'cdDisplayId'
--
-- * 'cdSubmittedBy'
--
-- * 'cdLanguage'
--
-- * 'cdCategoryCode'
--
-- * 'cdTimeCreated'
--
-- * 'cdServiceCode'
caseDetails
    :: CaseDetails
caseDetails =
    CaseDetails'
    { _cdSubject = Nothing
    , _cdStatus = Nothing
    , _cdRecentCommunications = Nothing
    , _cdSeverityCode = Nothing
    , _cdCaseId = Nothing
    , _cdCcEmailAddresses = Nothing
    , _cdDisplayId = Nothing
    , _cdSubmittedBy = Nothing
    , _cdLanguage = Nothing
    , _cdCategoryCode = Nothing
    , _cdTimeCreated = Nothing
    , _cdServiceCode = Nothing
    }

-- | The subject line for the case in the AWS Support Center.
cdSubject :: Lens' CaseDetails (Maybe Text)
cdSubject = lens _cdSubject (\ s a -> s{_cdSubject = a});

-- | The status of the case.
cdStatus :: Lens' CaseDetails (Maybe Text)
cdStatus = lens _cdStatus (\ s a -> s{_cdStatus = a});

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also
-- includes a 'nextToken' that you can use to retrieve earlier
-- communications.
cdRecentCommunications :: Lens' CaseDetails (Maybe RecentCaseCommunications)
cdRecentCommunications = lens _cdRecentCommunications (\ s a -> s{_cdRecentCommunications = a});

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
cdSeverityCode :: Lens' CaseDetails (Maybe Text)
cdSeverityCode = lens _cdSeverityCode (\ s a -> s{_cdSeverityCode = a});

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
cdCaseId :: Lens' CaseDetails (Maybe Text)
cdCaseId = lens _cdCaseId (\ s a -> s{_cdCaseId = a});

-- | The email addresses that receive copies of communication about the case.
cdCcEmailAddresses :: Lens' CaseDetails [Text]
cdCcEmailAddresses = lens _cdCcEmailAddresses (\ s a -> s{_cdCcEmailAddresses = a}) . _Default . _Coerce;

-- | The ID displayed for the case in the AWS Support Center. This is a
-- numeric string.
cdDisplayId :: Lens' CaseDetails (Maybe Text)
cdDisplayId = lens _cdDisplayId (\ s a -> s{_cdDisplayId = a});

-- | The email address of the account that submitted the case.
cdSubmittedBy :: Lens' CaseDetails (Maybe Text)
cdSubmittedBy = lens _cdSubmittedBy (\ s a -> s{_cdSubmittedBy = a});

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
cdLanguage :: Lens' CaseDetails (Maybe Text)
cdLanguage = lens _cdLanguage (\ s a -> s{_cdLanguage = a});

-- | The category of problem for the AWS Support case.
cdCategoryCode :: Lens' CaseDetails (Maybe Text)
cdCategoryCode = lens _cdCategoryCode (\ s a -> s{_cdCategoryCode = a});

-- | The time that the case was case created in the AWS Support Center.
cdTimeCreated :: Lens' CaseDetails (Maybe Text)
cdTimeCreated = lens _cdTimeCreated (\ s a -> s{_cdTimeCreated = a});

-- | The code for the AWS service returned by the call to DescribeServices.
cdServiceCode :: Lens' CaseDetails (Maybe Text)
cdServiceCode = lens _cdServiceCode (\ s a -> s{_cdServiceCode = a});

instance FromJSON CaseDetails where
        parseJSON
          = withObject "CaseDetails"
              (\ x ->
                 CaseDetails' <$>
                   (x .:? "subject") <*> (x .:? "status") <*>
                     (x .:? "recentCommunications")
                     <*> (x .:? "severityCode")
                     <*> (x .:? "caseId")
                     <*> (x .:? "ccEmailAddresses" .!= mempty)
                     <*> (x .:? "displayId")
                     <*> (x .:? "submittedBy")
                     <*> (x .:? "language")
                     <*> (x .:? "categoryCode")
                     <*> (x .:? "timeCreated")
                     <*> (x .:? "serviceCode"))

-- | A JSON-formatted name\/value pair that represents the category name and
-- category code of the problem, selected from the DescribeServices
-- response for each AWS service.
--
-- /See:/ 'category' smart constructor.
data Category = Category'
    { _cName :: !(Maybe Text)
    , _cCode :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Category' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cName'
--
-- * 'cCode'
category
    :: Category
category =
    Category'
    { _cName = Nothing
    , _cCode = Nothing
    }

-- | The category name for the support case.
cName :: Lens' Category (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a});

-- | The category code for the support case.
cCode :: Lens' Category (Maybe Text)
cCode = lens _cCode (\ s a -> s{_cCode = a});

instance FromJSON Category where
        parseJSON
          = withObject "Category"
              (\ x ->
                 Category' <$> (x .:? "name") <*> (x .:? "code"))

-- | A communication associated with an AWS Support case. The communication
-- consists of the case ID, the message body, attachment information, the
-- account email address, and the date and time of the communication.
--
-- /See:/ 'communication' smart constructor.
data Communication = Communication'
    { _cBody          :: !(Maybe Text)
    , _cCaseId        :: !(Maybe Text)
    , _cSubmittedBy   :: !(Maybe Text)
    , _cTimeCreated   :: !(Maybe Text)
    , _cAttachmentSet :: !(Maybe [AttachmentDetails])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Communication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cBody'
--
-- * 'cCaseId'
--
-- * 'cSubmittedBy'
--
-- * 'cTimeCreated'
--
-- * 'cAttachmentSet'
communication
    :: Communication
communication =
    Communication'
    { _cBody = Nothing
    , _cCaseId = Nothing
    , _cSubmittedBy = Nothing
    , _cTimeCreated = Nothing
    , _cAttachmentSet = Nothing
    }

-- | The text of the communication between the customer and AWS Support.
cBody :: Lens' Communication (Maybe Text)
cBody = lens _cBody (\ s a -> s{_cBody = a});

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
cCaseId :: Lens' Communication (Maybe Text)
cCaseId = lens _cCaseId (\ s a -> s{_cCaseId = a});

-- | The email address of the account that submitted the AWS Support case.
cSubmittedBy :: Lens' Communication (Maybe Text)
cSubmittedBy = lens _cSubmittedBy (\ s a -> s{_cSubmittedBy = a});

-- | The time the communication was created.
cTimeCreated :: Lens' Communication (Maybe Text)
cTimeCreated = lens _cTimeCreated (\ s a -> s{_cTimeCreated = a});

-- | Information about the attachments to the case communication.
cAttachmentSet :: Lens' Communication [AttachmentDetails]
cAttachmentSet = lens _cAttachmentSet (\ s a -> s{_cAttachmentSet = a}) . _Default . _Coerce;

instance FromJSON Communication where
        parseJSON
          = withObject "Communication"
              (\ x ->
                 Communication' <$>
                   (x .:? "body") <*> (x .:? "caseId") <*>
                     (x .:? "submittedBy")
                     <*> (x .:? "timeCreated")
                     <*> (x .:? "attachmentSet" .!= mempty))

-- | The five most recent communications associated with the case.
--
-- /See:/ 'recentCaseCommunications' smart constructor.
data RecentCaseCommunications = RecentCaseCommunications'
    { _rccNextToken      :: !(Maybe Text)
    , _rccCommunications :: !(Maybe [Communication])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecentCaseCommunications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rccNextToken'
--
-- * 'rccCommunications'
recentCaseCommunications
    :: RecentCaseCommunications
recentCaseCommunications =
    RecentCaseCommunications'
    { _rccNextToken = Nothing
    , _rccCommunications = Nothing
    }

-- | A resumption point for pagination.
rccNextToken :: Lens' RecentCaseCommunications (Maybe Text)
rccNextToken = lens _rccNextToken (\ s a -> s{_rccNextToken = a});

-- | The five most recent communications associated with the case.
rccCommunications :: Lens' RecentCaseCommunications [Communication]
rccCommunications = lens _rccCommunications (\ s a -> s{_rccCommunications = a}) . _Default . _Coerce;

instance FromJSON RecentCaseCommunications where
        parseJSON
          = withObject "RecentCaseCommunications"
              (\ x ->
                 RecentCaseCommunications' <$>
                   (x .:? "nextToken") <*>
                     (x .:? "communications" .!= mempty))

-- | A code and name pair that represent a severity level that can be applied
-- to a support case.
--
-- /See:/ 'severityLevel' smart constructor.
data SeverityLevel = SeverityLevel'
    { _slName :: !(Maybe Text)
    , _slCode :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SeverityLevel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slName'
--
-- * 'slCode'
severityLevel
    :: SeverityLevel
severityLevel =
    SeverityLevel'
    { _slName = Nothing
    , _slCode = Nothing
    }

-- | The name of the severity level that corresponds to the severity level
-- code.
slName :: Lens' SeverityLevel (Maybe Text)
slName = lens _slName (\ s a -> s{_slName = a});

-- | One of four values: \"low,\" \"medium,\" \"high,\" and \"urgent\". These
-- values correspond to response times returned to the caller in
-- 'SeverityLevel.name'.
slCode :: Lens' SeverityLevel (Maybe Text)
slCode = lens _slCode (\ s a -> s{_slCode = a});

instance FromJSON SeverityLevel where
        parseJSON
          = withObject "SeverityLevel"
              (\ x ->
                 SeverityLevel' <$> (x .:? "name") <*> (x .:? "code"))

-- | Information about an AWS service returned by the DescribeServices
-- operation.
--
-- /See:/ 'supportService' smart constructor.
data SupportService = SupportService'
    { _ssCategories :: !(Maybe [Category])
    , _ssName       :: !(Maybe Text)
    , _ssCode       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SupportService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssCategories'
--
-- * 'ssName'
--
-- * 'ssCode'
supportService
    :: SupportService
supportService =
    SupportService'
    { _ssCategories = Nothing
    , _ssName = Nothing
    , _ssCode = Nothing
    }

-- | A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to AWS Support when you call
-- CreateCase.
ssCategories :: Lens' SupportService [Category]
ssCategories = lens _ssCategories (\ s a -> s{_ssCategories = a}) . _Default . _Coerce;

-- | The friendly name for an AWS service. The 'Code' element contains the
-- corresponding code.
ssName :: Lens' SupportService (Maybe Text)
ssName = lens _ssName (\ s a -> s{_ssName = a});

-- | The code for an AWS service returned by the DescribeServices response.
-- The 'Name' element contains the corresponding friendly name.
ssCode :: Lens' SupportService (Maybe Text)
ssCode = lens _ssCode (\ s a -> s{_ssCode = a});

instance FromJSON SupportService where
        parseJSON
          = withObject "SupportService"
              (\ x ->
                 SupportService' <$>
                   (x .:? "categories" .!= mempty) <*> (x .:? "name")
                     <*> (x .:? "code"))

-- | The container for summary information that relates to the category of
-- the Trusted Advisor check.
--
-- /See:/ 'trustedAdvisorCategorySpecificSummary' smart constructor.
newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary'
    { _tacssCostOptimizing :: Maybe TrustedAdvisorCostOptimizingSummary
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrustedAdvisorCategorySpecificSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacssCostOptimizing'
trustedAdvisorCategorySpecificSummary
    :: TrustedAdvisorCategorySpecificSummary
trustedAdvisorCategorySpecificSummary =
    TrustedAdvisorCategorySpecificSummary'
    { _tacssCostOptimizing = Nothing
    }

-- | The summary information about cost savings for a Trusted Advisor check
-- that is in the Cost Optimizing category.
tacssCostOptimizing :: Lens' TrustedAdvisorCategorySpecificSummary (Maybe TrustedAdvisorCostOptimizingSummary)
tacssCostOptimizing = lens _tacssCostOptimizing (\ s a -> s{_tacssCostOptimizing = a});

instance FromJSON
         TrustedAdvisorCategorySpecificSummary where
        parseJSON
          = withObject "TrustedAdvisorCategorySpecificSummary"
              (\ x ->
                 TrustedAdvisorCategorySpecificSummary' <$>
                   (x .:? "costOptimizing"))

-- | The description and metadata for a Trusted Advisor check.
--
-- /See:/ 'trustedAdvisorCheckDescription' smart constructor.
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription'
    { _tacdId          :: !Text
    , _tacdName        :: !Text
    , _tacdDescription :: !Text
    , _tacdCategory    :: !Text
    , _tacdMetadata    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrustedAdvisorCheckDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacdId'
--
-- * 'tacdName'
--
-- * 'tacdDescription'
--
-- * 'tacdCategory'
--
-- * 'tacdMetadata'
trustedAdvisorCheckDescription
    :: Text -- ^ 'tacdId'
    -> Text -- ^ 'tacdName'
    -> Text -- ^ 'tacdDescription'
    -> Text -- ^ 'tacdCategory'
    -> TrustedAdvisorCheckDescription
trustedAdvisorCheckDescription pId_ pName_ pDescription_ pCategory_ =
    TrustedAdvisorCheckDescription'
    { _tacdId = pId_
    , _tacdName = pName_
    , _tacdDescription = pDescription_
    , _tacdCategory = pCategory_
    , _tacdMetadata = mempty
    }

-- | The unique identifier for the Trusted Advisor check.
tacdId :: Lens' TrustedAdvisorCheckDescription Text
tacdId = lens _tacdId (\ s a -> s{_tacdId = a});

-- | The display name for the Trusted Advisor check.
tacdName :: Lens' TrustedAdvisorCheckDescription Text
tacdName = lens _tacdName (\ s a -> s{_tacdName = a});

-- | The description of the Trusted Advisor check, which includes the alert
-- criteria and recommended actions (contains HTML markup).
tacdDescription :: Lens' TrustedAdvisorCheckDescription Text
tacdDescription = lens _tacdDescription (\ s a -> s{_tacdDescription = a});

-- | The category of the Trusted Advisor check.
tacdCategory :: Lens' TrustedAdvisorCheckDescription Text
tacdCategory = lens _tacdCategory (\ s a -> s{_tacdCategory = a});

-- | The column headings for the data returned by the Trusted Advisor check.
-- The order of the headings corresponds to the order of the data in the
-- __Metadata__ element of the TrustedAdvisorResourceDetail for the check.
-- __Metadata__ contains all the data that is shown in the Excel download,
-- even in those cases where the UI shows just summary data.
tacdMetadata :: Lens' TrustedAdvisorCheckDescription [Text]
tacdMetadata = lens _tacdMetadata (\ s a -> s{_tacdMetadata = a}) . _Coerce;

instance FromJSON TrustedAdvisorCheckDescription
         where
        parseJSON
          = withObject "TrustedAdvisorCheckDescription"
              (\ x ->
                 TrustedAdvisorCheckDescription' <$>
                   (x .: "id") <*> (x .: "name") <*>
                     (x .: "description")
                     <*> (x .: "category")
                     <*> (x .:? "metadata" .!= mempty))

-- | The refresh status of a Trusted Advisor check.
--
-- /See:/ 'trustedAdvisorCheckRefreshStatus' smart constructor.
data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus'
    { _tacrsCheckId                    :: !Text
    , _tacrsStatus                     :: !Text
    , _tacrsMillisUntilNextRefreshable :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrustedAdvisorCheckRefreshStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacrsCheckId'
--
-- * 'tacrsStatus'
--
-- * 'tacrsMillisUntilNextRefreshable'
trustedAdvisorCheckRefreshStatus
    :: Text -- ^ 'tacrsCheckId'
    -> Text -- ^ 'tacrsStatus'
    -> Integer -- ^ 'tacrsMillisUntilNextRefreshable'
    -> TrustedAdvisorCheckRefreshStatus
trustedAdvisorCheckRefreshStatus pCheckId_ pStatus_ pMillisUntilNextRefreshable_ =
    TrustedAdvisorCheckRefreshStatus'
    { _tacrsCheckId = pCheckId_
    , _tacrsStatus = pStatus_
    , _tacrsMillisUntilNextRefreshable = pMillisUntilNextRefreshable_
    }

-- | The unique identifier for the Trusted Advisor check.
tacrsCheckId :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsCheckId = lens _tacrsCheckId (\ s a -> s{_tacrsCheckId = a});

-- | The status of the Trusted Advisor check for which a refresh has been
-- requested: \"none\", \"enqueued\", \"processing\", \"success\", or
-- \"abandoned\".
tacrsStatus :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsStatus = lens _tacrsStatus (\ s a -> s{_tacrsStatus = a});

-- | The amount of time, in milliseconds, until the Trusted Advisor check is
-- eligible for refresh.
tacrsMillisUntilNextRefreshable :: Lens' TrustedAdvisorCheckRefreshStatus Integer
tacrsMillisUntilNextRefreshable = lens _tacrsMillisUntilNextRefreshable (\ s a -> s{_tacrsMillisUntilNextRefreshable = a});

instance FromJSON TrustedAdvisorCheckRefreshStatus
         where
        parseJSON
          = withObject "TrustedAdvisorCheckRefreshStatus"
              (\ x ->
                 TrustedAdvisorCheckRefreshStatus' <$>
                   (x .: "checkId") <*> (x .: "status") <*>
                     (x .: "millisUntilNextRefreshable"))

-- | The results of a Trusted Advisor check returned by
-- DescribeTrustedAdvisorCheckResult.
--
-- /See:/ 'trustedAdvisorCheckResult' smart constructor.
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult'
    { _tacrCheckId                 :: !Text
    , _tacrTimestamp               :: !Text
    , _tacrStatus                  :: !Text
    , _tacrResourcesSummary        :: !TrustedAdvisorResourcesSummary
    , _tacrCategorySpecificSummary :: !TrustedAdvisorCategorySpecificSummary
    , _tacrFlaggedResources        :: ![TrustedAdvisorResourceDetail]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrustedAdvisorCheckResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacrCheckId'
--
-- * 'tacrTimestamp'
--
-- * 'tacrStatus'
--
-- * 'tacrResourcesSummary'
--
-- * 'tacrCategorySpecificSummary'
--
-- * 'tacrFlaggedResources'
trustedAdvisorCheckResult
    :: Text -- ^ 'tacrCheckId'
    -> Text -- ^ 'tacrTimestamp'
    -> Text -- ^ 'tacrStatus'
    -> TrustedAdvisorResourcesSummary -- ^ 'tacrResourcesSummary'
    -> TrustedAdvisorCategorySpecificSummary -- ^ 'tacrCategorySpecificSummary'
    -> TrustedAdvisorCheckResult
trustedAdvisorCheckResult pCheckId_ pTimestamp_ pStatus_ pResourcesSummary_ pCategorySpecificSummary_ =
    TrustedAdvisorCheckResult'
    { _tacrCheckId = pCheckId_
    , _tacrTimestamp = pTimestamp_
    , _tacrStatus = pStatus_
    , _tacrResourcesSummary = pResourcesSummary_
    , _tacrCategorySpecificSummary = pCategorySpecificSummary_
    , _tacrFlaggedResources = mempty
    }

-- | The unique identifier for the Trusted Advisor check.
tacrCheckId :: Lens' TrustedAdvisorCheckResult Text
tacrCheckId = lens _tacrCheckId (\ s a -> s{_tacrCheckId = a});

-- | The time of the last refresh of the check.
tacrTimestamp :: Lens' TrustedAdvisorCheckResult Text
tacrTimestamp = lens _tacrTimestamp (\ s a -> s{_tacrTimestamp = a});

-- | The alert status of the check: \"ok\" (green), \"warning\" (yellow),
-- \"error\" (red), or \"not_available\".
tacrStatus :: Lens' TrustedAdvisorCheckResult Text
tacrStatus = lens _tacrStatus (\ s a -> s{_tacrStatus = a});

-- | Undocumented member.
tacrResourcesSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorResourcesSummary
tacrResourcesSummary = lens _tacrResourcesSummary (\ s a -> s{_tacrResourcesSummary = a});

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacrCategorySpecificSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorCategorySpecificSummary
tacrCategorySpecificSummary = lens _tacrCategorySpecificSummary (\ s a -> s{_tacrCategorySpecificSummary = a});

-- | The details about each resource listed in the check result.
tacrFlaggedResources :: Lens' TrustedAdvisorCheckResult [TrustedAdvisorResourceDetail]
tacrFlaggedResources = lens _tacrFlaggedResources (\ s a -> s{_tacrFlaggedResources = a}) . _Coerce;

instance FromJSON TrustedAdvisorCheckResult where
        parseJSON
          = withObject "TrustedAdvisorCheckResult"
              (\ x ->
                 TrustedAdvisorCheckResult' <$>
                   (x .: "checkId") <*> (x .: "timestamp") <*>
                     (x .: "status")
                     <*> (x .: "resourcesSummary")
                     <*> (x .: "categorySpecificSummary")
                     <*> (x .:? "flaggedResources" .!= mempty))

-- | A summary of a Trusted Advisor check result, including the alert status,
-- last refresh, and number of resources examined.
--
-- /See:/ 'trustedAdvisorCheckSummary' smart constructor.
data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary'
    { _tacsHasFlaggedResources     :: !(Maybe Bool)
    , _tacsCheckId                 :: !Text
    , _tacsTimestamp               :: !Text
    , _tacsStatus                  :: !Text
    , _tacsResourcesSummary        :: !TrustedAdvisorResourcesSummary
    , _tacsCategorySpecificSummary :: !TrustedAdvisorCategorySpecificSummary
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrustedAdvisorCheckSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacsHasFlaggedResources'
--
-- * 'tacsCheckId'
--
-- * 'tacsTimestamp'
--
-- * 'tacsStatus'
--
-- * 'tacsResourcesSummary'
--
-- * 'tacsCategorySpecificSummary'
trustedAdvisorCheckSummary
    :: Text -- ^ 'tacsCheckId'
    -> Text -- ^ 'tacsTimestamp'
    -> Text -- ^ 'tacsStatus'
    -> TrustedAdvisorResourcesSummary -- ^ 'tacsResourcesSummary'
    -> TrustedAdvisorCategorySpecificSummary -- ^ 'tacsCategorySpecificSummary'
    -> TrustedAdvisorCheckSummary
trustedAdvisorCheckSummary pCheckId_ pTimestamp_ pStatus_ pResourcesSummary_ pCategorySpecificSummary_ =
    TrustedAdvisorCheckSummary'
    { _tacsHasFlaggedResources = Nothing
    , _tacsCheckId = pCheckId_
    , _tacsTimestamp = pTimestamp_
    , _tacsStatus = pStatus_
    , _tacsResourcesSummary = pResourcesSummary_
    , _tacsCategorySpecificSummary = pCategorySpecificSummary_
    }

-- | Specifies whether the Trusted Advisor check has flagged resources.
tacsHasFlaggedResources :: Lens' TrustedAdvisorCheckSummary (Maybe Bool)
tacsHasFlaggedResources = lens _tacsHasFlaggedResources (\ s a -> s{_tacsHasFlaggedResources = a});

-- | The unique identifier for the Trusted Advisor check.
tacsCheckId :: Lens' TrustedAdvisorCheckSummary Text
tacsCheckId = lens _tacsCheckId (\ s a -> s{_tacsCheckId = a});

-- | The time of the last refresh of the check.
tacsTimestamp :: Lens' TrustedAdvisorCheckSummary Text
tacsTimestamp = lens _tacsTimestamp (\ s a -> s{_tacsTimestamp = a});

-- | The alert status of the check: \"ok\" (green), \"warning\" (yellow),
-- \"error\" (red), or \"not_available\".
tacsStatus :: Lens' TrustedAdvisorCheckSummary Text
tacsStatus = lens _tacsStatus (\ s a -> s{_tacsStatus = a});

-- | Undocumented member.
tacsResourcesSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorResourcesSummary
tacsResourcesSummary = lens _tacsResourcesSummary (\ s a -> s{_tacsResourcesSummary = a});

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacsCategorySpecificSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorCategorySpecificSummary
tacsCategorySpecificSummary = lens _tacsCategorySpecificSummary (\ s a -> s{_tacsCategorySpecificSummary = a});

instance FromJSON TrustedAdvisorCheckSummary where
        parseJSON
          = withObject "TrustedAdvisorCheckSummary"
              (\ x ->
                 TrustedAdvisorCheckSummary' <$>
                   (x .:? "hasFlaggedResources") <*> (x .: "checkId")
                     <*> (x .: "timestamp")
                     <*> (x .: "status")
                     <*> (x .: "resourcesSummary")
                     <*> (x .: "categorySpecificSummary"))

-- | The estimated cost savings that might be realized if the recommended
-- actions are taken.
--
-- /See:/ 'trustedAdvisorCostOptimizingSummary' smart constructor.
data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary'
    { _tacosEstimatedMonthlySavings        :: !Double
    , _tacosEstimatedPercentMonthlySavings :: !Double
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrustedAdvisorCostOptimizingSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacosEstimatedMonthlySavings'
--
-- * 'tacosEstimatedPercentMonthlySavings'
trustedAdvisorCostOptimizingSummary
    :: Double -- ^ 'tacosEstimatedMonthlySavings'
    -> Double -- ^ 'tacosEstimatedPercentMonthlySavings'
    -> TrustedAdvisorCostOptimizingSummary
trustedAdvisorCostOptimizingSummary pEstimatedMonthlySavings_ pEstimatedPercentMonthlySavings_ =
    TrustedAdvisorCostOptimizingSummary'
    { _tacosEstimatedMonthlySavings = pEstimatedMonthlySavings_
    , _tacosEstimatedPercentMonthlySavings = pEstimatedPercentMonthlySavings_
    }

-- | The estimated monthly savings that might be realized if the recommended
-- actions are taken.
tacosEstimatedMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary Double
tacosEstimatedMonthlySavings = lens _tacosEstimatedMonthlySavings (\ s a -> s{_tacosEstimatedMonthlySavings = a});

-- | The estimated percentage of savings that might be realized if the
-- recommended actions are taken.
tacosEstimatedPercentMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary Double
tacosEstimatedPercentMonthlySavings = lens _tacosEstimatedPercentMonthlySavings (\ s a -> s{_tacosEstimatedPercentMonthlySavings = a});

instance FromJSON TrustedAdvisorCostOptimizingSummary
         where
        parseJSON
          = withObject "TrustedAdvisorCostOptimizingSummary"
              (\ x ->
                 TrustedAdvisorCostOptimizingSummary' <$>
                   (x .: "estimatedMonthlySavings") <*>
                     (x .: "estimatedPercentMonthlySavings"))

-- | Contains information about a resource identified by a Trusted Advisor
-- check.
--
-- /See:/ 'trustedAdvisorResourceDetail' smart constructor.
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail'
    { _tardIsSuppressed :: !(Maybe Bool)
    , _tardStatus       :: !Text
    , _tardRegion       :: !Text
    , _tardResourceId   :: !Text
    , _tardMetadata     :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrustedAdvisorResourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tardIsSuppressed'
--
-- * 'tardStatus'
--
-- * 'tardRegion'
--
-- * 'tardResourceId'
--
-- * 'tardMetadata'
trustedAdvisorResourceDetail
    :: Text -- ^ 'tardStatus'
    -> Text -- ^ 'tardRegion'
    -> Text -- ^ 'tardResourceId'
    -> TrustedAdvisorResourceDetail
trustedAdvisorResourceDetail pStatus_ pRegion_ pResourceId_ =
    TrustedAdvisorResourceDetail'
    { _tardIsSuppressed = Nothing
    , _tardStatus = pStatus_
    , _tardRegion = pRegion_
    , _tardResourceId = pResourceId_
    , _tardMetadata = mempty
    }

-- | Specifies whether the AWS resource was ignored by Trusted Advisor
-- because it was marked as suppressed by the user.
tardIsSuppressed :: Lens' TrustedAdvisorResourceDetail (Maybe Bool)
tardIsSuppressed = lens _tardIsSuppressed (\ s a -> s{_tardIsSuppressed = a});

-- | The status code for the resource identified in the Trusted Advisor
-- check.
tardStatus :: Lens' TrustedAdvisorResourceDetail Text
tardStatus = lens _tardStatus (\ s a -> s{_tardStatus = a});

-- | The AWS region in which the identified resource is located.
tardRegion :: Lens' TrustedAdvisorResourceDetail Text
tardRegion = lens _tardRegion (\ s a -> s{_tardRegion = a});

-- | The unique identifier for the identified resource.
tardResourceId :: Lens' TrustedAdvisorResourceDetail Text
tardResourceId = lens _tardResourceId (\ s a -> s{_tardResourceId = a});

-- | Additional information about the identified resource. The exact metadata
-- and its order can be obtained by inspecting the
-- TrustedAdvisorCheckDescription object returned by the call to
-- DescribeTrustedAdvisorChecks. __Metadata__ contains all the data that is
-- shown in the Excel download, even in those cases where the UI shows just
-- summary data.
tardMetadata :: Lens' TrustedAdvisorResourceDetail [Text]
tardMetadata = lens _tardMetadata (\ s a -> s{_tardMetadata = a}) . _Coerce;

instance FromJSON TrustedAdvisorResourceDetail where
        parseJSON
          = withObject "TrustedAdvisorResourceDetail"
              (\ x ->
                 TrustedAdvisorResourceDetail' <$>
                   (x .:? "isSuppressed") <*> (x .: "status") <*>
                     (x .: "region")
                     <*> (x .: "resourceId")
                     <*> (x .:? "metadata" .!= mempty))

-- | Details about AWS resources that were analyzed in a call to Trusted
-- Advisor DescribeTrustedAdvisorCheckSummaries.
--
-- /See:/ 'trustedAdvisorResourcesSummary' smart constructor.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary'
    { _tarsResourcesProcessed  :: !Integer
    , _tarsResourcesFlagged    :: !Integer
    , _tarsResourcesIgnored    :: !Integer
    , _tarsResourcesSuppressed :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrustedAdvisorResourcesSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tarsResourcesProcessed'
--
-- * 'tarsResourcesFlagged'
--
-- * 'tarsResourcesIgnored'
--
-- * 'tarsResourcesSuppressed'
trustedAdvisorResourcesSummary
    :: Integer -- ^ 'tarsResourcesProcessed'
    -> Integer -- ^ 'tarsResourcesFlagged'
    -> Integer -- ^ 'tarsResourcesIgnored'
    -> Integer -- ^ 'tarsResourcesSuppressed'
    -> TrustedAdvisorResourcesSummary
trustedAdvisorResourcesSummary pResourcesProcessed_ pResourcesFlagged_ pResourcesIgnored_ pResourcesSuppressed_ =
    TrustedAdvisorResourcesSummary'
    { _tarsResourcesProcessed = pResourcesProcessed_
    , _tarsResourcesFlagged = pResourcesFlagged_
    , _tarsResourcesIgnored = pResourcesIgnored_
    , _tarsResourcesSuppressed = pResourcesSuppressed_
    }

-- | The number of AWS resources that were analyzed by the Trusted Advisor
-- check.
tarsResourcesProcessed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesProcessed = lens _tarsResourcesProcessed (\ s a -> s{_tarsResourcesProcessed = a});

-- | The number of AWS resources that were flagged (listed) by the Trusted
-- Advisor check.
tarsResourcesFlagged :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesFlagged = lens _tarsResourcesFlagged (\ s a -> s{_tarsResourcesFlagged = a});

-- | The number of AWS resources ignored by Trusted Advisor because
-- information was unavailable.
tarsResourcesIgnored :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesIgnored = lens _tarsResourcesIgnored (\ s a -> s{_tarsResourcesIgnored = a});

-- | The number of AWS resources ignored by Trusted Advisor because they were
-- marked as suppressed by the user.
tarsResourcesSuppressed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesSuppressed = lens _tarsResourcesSuppressed (\ s a -> s{_tarsResourcesSuppressed = a});

instance FromJSON TrustedAdvisorResourcesSummary
         where
        parseJSON
          = withObject "TrustedAdvisorResourcesSummary"
              (\ x ->
                 TrustedAdvisorResourcesSummary' <$>
                   (x .: "resourcesProcessed") <*>
                     (x .: "resourcesFlagged")
                     <*> (x .: "resourcesIgnored")
                     <*> (x .: "resourcesSuppressed"))
