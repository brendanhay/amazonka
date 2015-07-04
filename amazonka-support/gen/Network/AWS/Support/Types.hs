{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.Support.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.Support.Types
    (
    -- * Service
      Support

    -- * Errors
    , _AttachmentSetExpired
    , _AttachmentLimitExceeded
    , _DescribeAttachmentLimitExceeded
    , _CaseIdNotFound
    , _AttachmentSetIdNotFound
    , _AttachmentSetSizeLimitExceeded
    , _AttachmentIdNotFound
    , _InternalServerError
    , _CaseCreationLimitExceeded

    -- * Attachment
    , Attachment
    , attachment
    , attData
    , attFileName

    -- * AttachmentDetails
    , AttachmentDetails
    , attachmentDetails
    , adAttachmentId
    , adFileName

    -- * CaseDetails
    , CaseDetails
    , caseDetails
    , cdSubject
    , cdStatus
    , cdRecentCommunications
    , cdSeverityCode
    , cdCaseId
    , cdCcEmailAddresses
    , cdDisplayId
    , cdSubmittedBy
    , cdLanguage
    , cdCategoryCode
    , cdTimeCreated
    , cdServiceCode

    -- * Category
    , Category
    , category
    , catName
    , catCode

    -- * Communication
    , Communication
    , communication
    , comBody
    , comCaseId
    , comSubmittedBy
    , comTimeCreated
    , comAttachmentSet

    -- * RecentCaseCommunications
    , RecentCaseCommunications
    , recentCaseCommunications
    , rccNextToken
    , rccCommunications

    -- * SeverityLevel
    , SeverityLevel
    , severityLevel
    , slName
    , slCode

    -- * SupportService
    , SupportService
    , supportService
    , ssCategories
    , ssName
    , ssCode

    -- * TrustedAdvisorCategorySpecificSummary
    , TrustedAdvisorCategorySpecificSummary
    , trustedAdvisorCategorySpecificSummary
    , tacssCostOptimizing

    -- * TrustedAdvisorCheckDescription
    , TrustedAdvisorCheckDescription
    , trustedAdvisorCheckDescription
    , tacdId
    , tacdName
    , tacdDescription
    , tacdCategory
    , tacdMetadata

    -- * TrustedAdvisorCheckRefreshStatus
    , TrustedAdvisorCheckRefreshStatus
    , trustedAdvisorCheckRefreshStatus
    , tacrsCheckId
    , tacrsStatus
    , tacrsMillisUntilNextRefreshable

    -- * TrustedAdvisorCheckResult
    , TrustedAdvisorCheckResult
    , trustedAdvisorCheckResult
    , tacrCheckId
    , tacrTimestamp
    , tacrStatus
    , tacrResourcesSummary
    , tacrCategorySpecificSummary
    , tacrFlaggedResources

    -- * TrustedAdvisorCheckSummary
    , TrustedAdvisorCheckSummary
    , trustedAdvisorCheckSummary
    , tacsHasFlaggedResources
    , tacsCheckId
    , tacsTimestamp
    , tacsStatus
    , tacsResourcesSummary
    , tacsCategorySpecificSummary

    -- * TrustedAdvisorCostOptimizingSummary
    , TrustedAdvisorCostOptimizingSummary
    , trustedAdvisorCostOptimizingSummary
    , tacosEstimatedMonthlySavings
    , tacosEstimatedPercentMonthlySavings

    -- * TrustedAdvisorResourceDetail
    , TrustedAdvisorResourceDetail
    , trustedAdvisorResourceDetail
    , tardIsSuppressed
    , tardStatus
    , tardRegion
    , tardResourceId
    , tardMetadata

    -- * TrustedAdvisorResourcesSummary
    , TrustedAdvisorResourcesSummary
    , trustedAdvisorResourcesSummary
    , tarsResourcesProcessed
    , tarsResourcesFlagged
    , tarsResourcesIgnored
    , tarsResourcesSuppressed
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2013-04-15@ of the Amazon Support SDK.
data Support

instance AWSService Support where
    type Sg Support = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Support"
            , _svcPrefix = "support"
            , _svcVersion = "2013-04-15"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The expiration time of the attachment set has passed. The set expires 1
-- hour after it is created.
_AttachmentSetExpired :: AWSError a => Getting (First ServiceError) a ServiceError
_AttachmentSetExpired = _ServiceError . hasCode "AttachmentSetExpired"

-- | The limit for the number of attachment sets created in a short period of
-- time has been exceeded.
_AttachmentLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_AttachmentLimitExceeded = _ServiceError . hasCode "AttachmentLimitExceeded"

-- | The limit for the number of DescribeAttachment requests in a short
-- period of time has been exceeded.
_DescribeAttachmentLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_DescribeAttachmentLimitExceeded =
    _ServiceError . hasCode "DescribeAttachmentLimitExceeded"

-- | The requested @CaseId@ could not be located.
_CaseIdNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_CaseIdNotFound = _ServiceError . hasCode "CaseIdNotFound"

-- | An attachment set with the specified ID could not be found.
_AttachmentSetIdNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_AttachmentSetIdNotFound = _ServiceError . hasCode "AttachmentSetIdNotFound"

-- | A limit for the size of an attachment set has been exceeded. The limits
-- are 3 attachments and 5 MB per attachment.
_AttachmentSetSizeLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_AttachmentSetSizeLimitExceeded =
    _ServiceError . hasCode "AttachmentSetSizeLimitExceeded"

-- | An attachment with the specified ID could not be found.
_AttachmentIdNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_AttachmentIdNotFound = _ServiceError . hasCode "AttachmentIdNotFound"

-- | An internal server error occurred.
_InternalServerError :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _ServiceError . hasCode "InternalServerError"

-- | The case creation limit for the account has been exceeded.
_CaseCreationLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_CaseCreationLimitExceeded =
    _ServiceError . hasCode "CaseCreationLimitExceeded"

-- | An attachment to a case communication. The attachment consists of the
-- file name and the content of the file.
--
-- /See:/ 'attachment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attData'
--
-- * 'attFileName'
data Attachment = Attachment'
    { _attData     :: !(Maybe Base64)
    , _attFileName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Attachment' smart constructor.
attachment :: Attachment
attachment =
    Attachment'
    { _attData = Nothing
    , _attFileName = Nothing
    }

-- | The content of the attachment file.
attData :: Lens' Attachment (Maybe Base64)
attData = lens _attData (\ s a -> s{_attData = a});

-- | The name of the attachment file.
attFileName :: Lens' Attachment (Maybe Text)
attFileName = lens _attFileName (\ s a -> s{_attFileName = a});

instance FromJSON Attachment where
        parseJSON
          = withObject "Attachment"
              (\ x ->
                 Attachment' <$>
                   (x .:? "data") <*> (x .:? "fileName"))

instance ToJSON Attachment where
        toJSON Attachment'{..}
          = object
              ["data" .= _attData, "fileName" .= _attFileName]

-- | The file name and ID of an attachment to a case communication. You can
-- use the ID to retrieve the attachment with the DescribeAttachment
-- operation.
--
-- /See:/ 'attachmentDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adAttachmentId'
--
-- * 'adFileName'
data AttachmentDetails = AttachmentDetails'
    { _adAttachmentId :: !(Maybe Text)
    , _adFileName     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachmentDetails' smart constructor.
attachmentDetails :: AttachmentDetails
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
--     of these objects are @Attachments@, @Body@, @CaseId@, @SubmittedBy@,
--     and @TimeCreated@.
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
--
-- The fields accessible through corresponding lenses are:
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

-- | 'CaseDetails' smart constructor.
caseDetails :: CaseDetails
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
-- includes a @nextToken@ that you can use to retrieve earlier
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
cdCcEmailAddresses = lens _cdCcEmailAddresses (\ s a -> s{_cdCcEmailAddresses = a}) . _Default;

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'catName'
--
-- * 'catCode'
data Category = Category'
    { _catName :: !(Maybe Text)
    , _catCode :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Category' smart constructor.
category :: Category
category =
    Category'
    { _catName = Nothing
    , _catCode = Nothing
    }

-- | The category name for the support case.
catName :: Lens' Category (Maybe Text)
catName = lens _catName (\ s a -> s{_catName = a});

-- | The category code for the support case.
catCode :: Lens' Category (Maybe Text)
catCode = lens _catCode (\ s a -> s{_catCode = a});

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'comBody'
--
-- * 'comCaseId'
--
-- * 'comSubmittedBy'
--
-- * 'comTimeCreated'
--
-- * 'comAttachmentSet'
data Communication = Communication'
    { _comBody          :: !(Maybe Text)
    , _comCaseId        :: !(Maybe Text)
    , _comSubmittedBy   :: !(Maybe Text)
    , _comTimeCreated   :: !(Maybe Text)
    , _comAttachmentSet :: !(Maybe [AttachmentDetails])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Communication' smart constructor.
communication :: Communication
communication =
    Communication'
    { _comBody = Nothing
    , _comCaseId = Nothing
    , _comSubmittedBy = Nothing
    , _comTimeCreated = Nothing
    , _comAttachmentSet = Nothing
    }

-- | The text of the communication between the customer and AWS Support.
comBody :: Lens' Communication (Maybe Text)
comBody = lens _comBody (\ s a -> s{_comBody = a});

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
comCaseId :: Lens' Communication (Maybe Text)
comCaseId = lens _comCaseId (\ s a -> s{_comCaseId = a});

-- | The email address of the account that submitted the AWS Support case.
comSubmittedBy :: Lens' Communication (Maybe Text)
comSubmittedBy = lens _comSubmittedBy (\ s a -> s{_comSubmittedBy = a});

-- | The time the communication was created.
comTimeCreated :: Lens' Communication (Maybe Text)
comTimeCreated = lens _comTimeCreated (\ s a -> s{_comTimeCreated = a});

-- | Information about the attachments to the case communication.
comAttachmentSet :: Lens' Communication [AttachmentDetails]
comAttachmentSet = lens _comAttachmentSet (\ s a -> s{_comAttachmentSet = a}) . _Default;

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rccNextToken'
--
-- * 'rccCommunications'
data RecentCaseCommunications = RecentCaseCommunications'
    { _rccNextToken      :: !(Maybe Text)
    , _rccCommunications :: !(Maybe [Communication])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RecentCaseCommunications' smart constructor.
recentCaseCommunications :: RecentCaseCommunications
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
rccCommunications = lens _rccCommunications (\ s a -> s{_rccCommunications = a}) . _Default;

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slName'
--
-- * 'slCode'
data SeverityLevel = SeverityLevel'
    { _slName :: !(Maybe Text)
    , _slCode :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SeverityLevel' smart constructor.
severityLevel :: SeverityLevel
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
-- @SeverityLevel.name@.
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssCategories'
--
-- * 'ssName'
--
-- * 'ssCode'
data SupportService = SupportService'
    { _ssCategories :: !(Maybe [Category])
    , _ssName       :: !(Maybe Text)
    , _ssCode       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SupportService' smart constructor.
supportService :: SupportService
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
ssCategories = lens _ssCategories (\ s a -> s{_ssCategories = a}) . _Default;

-- | The friendly name for an AWS service. The @Code@ element contains the
-- corresponding code.
ssName :: Lens' SupportService (Maybe Text)
ssName = lens _ssName (\ s a -> s{_ssName = a});

-- | The code for an AWS service returned by the DescribeServices response.
-- The @Name@ element contains the corresponding friendly name.
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tacssCostOptimizing'
newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary'
    { _tacssCostOptimizing :: Maybe TrustedAdvisorCostOptimizingSummary
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TrustedAdvisorCategorySpecificSummary' smart constructor.
trustedAdvisorCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
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
--
-- The fields accessible through corresponding lenses are:
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
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription'
    { _tacdId          :: !Text
    , _tacdName        :: !Text
    , _tacdDescription :: !Text
    , _tacdCategory    :: !Text
    , _tacdMetadata    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TrustedAdvisorCheckDescription' smart constructor.
trustedAdvisorCheckDescription :: Text -> Text -> Text -> Text -> TrustedAdvisorCheckDescription
trustedAdvisorCheckDescription pId pName pDescription pCategory =
    TrustedAdvisorCheckDescription'
    { _tacdId = pId
    , _tacdName = pName
    , _tacdDescription = pDescription
    , _tacdCategory = pCategory
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
tacdMetadata = lens _tacdMetadata (\ s a -> s{_tacdMetadata = a});

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tacrsCheckId'
--
-- * 'tacrsStatus'
--
-- * 'tacrsMillisUntilNextRefreshable'
data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus'
    { _tacrsCheckId                    :: !Text
    , _tacrsStatus                     :: !Text
    , _tacrsMillisUntilNextRefreshable :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TrustedAdvisorCheckRefreshStatus' smart constructor.
trustedAdvisorCheckRefreshStatus :: Text -> Text -> Integer -> TrustedAdvisorCheckRefreshStatus
trustedAdvisorCheckRefreshStatus pCheckId pStatus pMillisUntilNextRefreshable =
    TrustedAdvisorCheckRefreshStatus'
    { _tacrsCheckId = pCheckId
    , _tacrsStatus = pStatus
    , _tacrsMillisUntilNextRefreshable = pMillisUntilNextRefreshable
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
--
-- The fields accessible through corresponding lenses are:
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
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult'
    { _tacrCheckId                 :: !Text
    , _tacrTimestamp               :: !Text
    , _tacrStatus                  :: !Text
    , _tacrResourcesSummary        :: !TrustedAdvisorResourcesSummary
    , _tacrCategorySpecificSummary :: !TrustedAdvisorCategorySpecificSummary
    , _tacrFlaggedResources        :: ![TrustedAdvisorResourceDetail]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TrustedAdvisorCheckResult' smart constructor.
trustedAdvisorCheckResult :: Text -> Text -> Text -> TrustedAdvisorResourcesSummary -> TrustedAdvisorCategorySpecificSummary -> TrustedAdvisorCheckResult
trustedAdvisorCheckResult pCheckId pTimestamp pStatus pResourcesSummary pCategorySpecificSummary =
    TrustedAdvisorCheckResult'
    { _tacrCheckId = pCheckId
    , _tacrTimestamp = pTimestamp
    , _tacrStatus = pStatus
    , _tacrResourcesSummary = pResourcesSummary
    , _tacrCategorySpecificSummary = pCategorySpecificSummary
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

-- | FIXME: Undocumented member.
tacrResourcesSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorResourcesSummary
tacrResourcesSummary = lens _tacrResourcesSummary (\ s a -> s{_tacrResourcesSummary = a});

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacrCategorySpecificSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorCategorySpecificSummary
tacrCategorySpecificSummary = lens _tacrCategorySpecificSummary (\ s a -> s{_tacrCategorySpecificSummary = a});

-- | The details about each resource listed in the check result.
tacrFlaggedResources :: Lens' TrustedAdvisorCheckResult [TrustedAdvisorResourceDetail]
tacrFlaggedResources = lens _tacrFlaggedResources (\ s a -> s{_tacrFlaggedResources = a});

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
--
-- The fields accessible through corresponding lenses are:
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
data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary'
    { _tacsHasFlaggedResources     :: !(Maybe Bool)
    , _tacsCheckId                 :: !Text
    , _tacsTimestamp               :: !Text
    , _tacsStatus                  :: !Text
    , _tacsResourcesSummary        :: !TrustedAdvisorResourcesSummary
    , _tacsCategorySpecificSummary :: !TrustedAdvisorCategorySpecificSummary
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TrustedAdvisorCheckSummary' smart constructor.
trustedAdvisorCheckSummary :: Text -> Text -> Text -> TrustedAdvisorResourcesSummary -> TrustedAdvisorCategorySpecificSummary -> TrustedAdvisorCheckSummary
trustedAdvisorCheckSummary pCheckId pTimestamp pStatus pResourcesSummary pCategorySpecificSummary =
    TrustedAdvisorCheckSummary'
    { _tacsHasFlaggedResources = Nothing
    , _tacsCheckId = pCheckId
    , _tacsTimestamp = pTimestamp
    , _tacsStatus = pStatus
    , _tacsResourcesSummary = pResourcesSummary
    , _tacsCategorySpecificSummary = pCategorySpecificSummary
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

-- | FIXME: Undocumented member.
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tacosEstimatedMonthlySavings'
--
-- * 'tacosEstimatedPercentMonthlySavings'
data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary'
    { _tacosEstimatedMonthlySavings        :: !Double
    , _tacosEstimatedPercentMonthlySavings :: !Double
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TrustedAdvisorCostOptimizingSummary' smart constructor.
trustedAdvisorCostOptimizingSummary :: Double -> Double -> TrustedAdvisorCostOptimizingSummary
trustedAdvisorCostOptimizingSummary pEstimatedMonthlySavings pEstimatedPercentMonthlySavings =
    TrustedAdvisorCostOptimizingSummary'
    { _tacosEstimatedMonthlySavings = pEstimatedMonthlySavings
    , _tacosEstimatedPercentMonthlySavings = pEstimatedPercentMonthlySavings
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
--
-- The fields accessible through corresponding lenses are:
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
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail'
    { _tardIsSuppressed :: !(Maybe Bool)
    , _tardStatus       :: !Text
    , _tardRegion       :: !Text
    , _tardResourceId   :: !Text
    , _tardMetadata     :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TrustedAdvisorResourceDetail' smart constructor.
trustedAdvisorResourceDetail :: Text -> Text -> Text -> TrustedAdvisorResourceDetail
trustedAdvisorResourceDetail pStatus pRegion pResourceId =
    TrustedAdvisorResourceDetail'
    { _tardIsSuppressed = Nothing
    , _tardStatus = pStatus
    , _tardRegion = pRegion
    , _tardResourceId = pResourceId
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
tardMetadata = lens _tardMetadata (\ s a -> s{_tardMetadata = a});

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tarsResourcesProcessed'
--
-- * 'tarsResourcesFlagged'
--
-- * 'tarsResourcesIgnored'
--
-- * 'tarsResourcesSuppressed'
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary'
    { _tarsResourcesProcessed  :: !Integer
    , _tarsResourcesFlagged    :: !Integer
    , _tarsResourcesIgnored    :: !Integer
    , _tarsResourcesSuppressed :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TrustedAdvisorResourcesSummary' smart constructor.
trustedAdvisorResourcesSummary :: Integer -> Integer -> Integer -> Integer -> TrustedAdvisorResourcesSummary
trustedAdvisorResourcesSummary pResourcesProcessed pResourcesFlagged pResourcesIgnored pResourcesSuppressed =
    TrustedAdvisorResourcesSummary'
    { _tarsResourcesProcessed = pResourcesProcessed
    , _tarsResourcesFlagged = pResourcesFlagged
    , _tarsResourcesIgnored = pResourcesIgnored
    , _tarsResourcesSuppressed = pResourcesSuppressed
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
