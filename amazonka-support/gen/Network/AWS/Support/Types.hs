{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Support is a one-on-one, fast-response support channel that is staffed
-- with experienced support personnel. The service helps customers get the
-- most from the products and features provided by Amazon Web Services. There
-- are four levels, or tiers, of AWS Support: Basic, Developer, Business, and
-- Enterprise. The Basic tier is free of charge and offers support for account
-- and billing questions and service limit increases. The other tiers offer an
-- unlimited number of technical support cases with pay-by-the-month pricing
-- and no long-term contracts, providing developers and businesses flexibility
-- to choose the level of support that meets their needs.
module Network.AWS.Support.Types
    (
    -- * Service
      Support
    -- ** Errors
    , SupportError (..)
    , _AttachmentIdNotFound
    , _AttachmentLimitExceeded
    , _AttachmentSetExpired
    , _AttachmentSetIdNotFound
    , _AttachmentSetSizeLimitExceeded
    , _CaseCreationLimitExceeded
    , _CaseIdNotFound
    , _DescribeAttachmentLimitExceeded
    , _InternalServerError
    , _SupportClient
    , _SupportSerializer
    , _SupportService

    -- * TrustedAdvisorCategorySpecificSummary
    , TrustedAdvisorCategorySpecificSummary
    , trustedAdvisorCategorySpecificSummary
    , tacssCostOptimizing

    -- * Attachment
    , Attachment
    , attachment
    , aFileName
    , aData

    -- * AttachmentDetails
    , AttachmentDetails
    , attachmentDetails
    , adAttachmentId
    , adFileName

    -- * CaseDetails
    , CaseDetails
    , caseDetails
    , cdCaseId
    , cdDisplayId
    , cdSubject
    , cdStatus
    , cdServiceCode
    , cdCategoryCode
    , cdSeverityCode
    , cdSubmittedBy
    , cdTimeCreated
    , cdRecentCommunications
    , cdCcEmailAddresses
    , cdLanguage

    -- * Category
    , Category
    , category
    , c1Code
    , c1Name

    -- * Communication
    , Communication
    , communication
    , cCaseId
    , cBody
    , cSubmittedBy
    , cTimeCreated
    , cAttachmentSet

    -- * RecentCaseCommunications
    , RecentCaseCommunications
    , recentCaseCommunications
    , rccCommunications
    , rccNextToken

    -- * Service'
    , Service'
    , service'
    , sCode
    , sName
    , sCategories

    -- * SeverityLevel
    , SeverityLevel
    , severityLevel
    , slCode
    , slName

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
    , tacsCheckId
    , tacsTimestamp
    , tacsStatus
    , tacsHasFlaggedResources
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
    , tardStatus
    , tardRegion
    , tardResourceId
    , tardIsSuppressed
    , tardMetadata

    -- * TrustedAdvisorResourcesSummary
    , TrustedAdvisorResourcesSummary
    , trustedAdvisorResourcesSummary
    , tarsResourcesProcessed
    , tarsResourcesFlagged
    , tarsResourcesIgnored
    , tarsResourcesSuppressed
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-04-15@) of the
-- @AWS Support@ service.
data Support deriving (Typeable)

instance AWSService Support where
    type Sg Support = V4
    type Er Support = SupportError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "support"
        , _svcVersion  = "2013-04-15"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'Support' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data SupportError
      -- | An attachment with the specified ID could not be found.
    = AttachmentIdNotFound
        { _ainfMessage :: Maybe Text
        }
      -- | The limit for the number of attachment sets created in a short
      -- period of time has been exceeded.
    | AttachmentLimitExceeded
        { _aleMessage :: Maybe Text
        }
      -- | The expiration time of the attachment set has passed. The set
      -- expires 1 hour after it is created.
    | AttachmentSetExpired
        { _aseMessage :: Maybe Text
        }
      -- | An attachment set with the specified ID could not be found.
    | AttachmentSetIdNotFound
        { _asinfMessage :: Maybe Text
        }
      -- | A limit for the size of an attachment set has been exceeded. The
      -- limits are 3 attachments and 5 MB per attachment.
    | AttachmentSetSizeLimitExceeded
        { _assleMessage :: Maybe Text
        }
      -- | The case creation limit for the account has been exceeded.
    | CaseCreationLimitExceeded
        { _ccleMessage :: Maybe Text
        }
      -- | The requested CaseId could not be located.
    | CaseIdNotFound
        { _cinfMessage :: Maybe Text
        }
      -- | The limit for the number of DescribeAttachment requests in a
      -- short period of time has been exceeded.
    | DescribeAttachmentLimitExceeded
        { _daleMessage :: Maybe Text
        }
      -- | An internal server error occurred.
    | InternalServerError
        { _iseMessage :: Maybe Text
        }
    | SupportClient HttpException
    | SupportSerializer String
    | SupportService String
      deriving (Show, Typeable, Generic)

instance AWSError SupportError where
    awsError = const "SupportError"

instance AWSServiceError SupportError where
    serviceError    = SupportService
    clientError     = SupportClient
    serializerError = SupportSerializer

instance Exception SupportError

-- | An attachment with the specified ID could not be found.
--
-- See: 'AttachmentIdNotFound'
_AttachmentIdNotFound :: Prism' SupportError (Maybe Text)
_AttachmentIdNotFound = prism
    AttachmentIdNotFound
    (\case
        AttachmentIdNotFound p1 -> Right p1
        x -> Left x)

-- | The limit for the number of attachment sets created in a short period of
-- time has been exceeded.
--
-- See: 'AttachmentLimitExceeded'
_AttachmentLimitExceeded :: Prism' SupportError (Maybe Text)
_AttachmentLimitExceeded = prism
    AttachmentLimitExceeded
    (\case
        AttachmentLimitExceeded p1 -> Right p1
        x -> Left x)

-- | The expiration time of the attachment set has passed. The set expires 1
-- hour after it is created.
--
-- See: 'AttachmentSetExpired'
_AttachmentSetExpired :: Prism' SupportError (Maybe Text)
_AttachmentSetExpired = prism
    AttachmentSetExpired
    (\case
        AttachmentSetExpired p1 -> Right p1
        x -> Left x)

-- | An attachment set with the specified ID could not be found.
--
-- See: 'AttachmentSetIdNotFound'
_AttachmentSetIdNotFound :: Prism' SupportError (Maybe Text)
_AttachmentSetIdNotFound = prism
    AttachmentSetIdNotFound
    (\case
        AttachmentSetIdNotFound p1 -> Right p1
        x -> Left x)

-- | A limit for the size of an attachment set has been exceeded. The limits are
-- 3 attachments and 5 MB per attachment.
--
-- See: 'AttachmentSetSizeLimitExceeded'
_AttachmentSetSizeLimitExceeded :: Prism' SupportError (Maybe Text)
_AttachmentSetSizeLimitExceeded = prism
    AttachmentSetSizeLimitExceeded
    (\case
        AttachmentSetSizeLimitExceeded p1 -> Right p1
        x -> Left x)

-- | The case creation limit for the account has been exceeded.
--
-- See: 'CaseCreationLimitExceeded'
_CaseCreationLimitExceeded :: Prism' SupportError (Maybe Text)
_CaseCreationLimitExceeded = prism
    CaseCreationLimitExceeded
    (\case
        CaseCreationLimitExceeded p1 -> Right p1
        x -> Left x)

-- | The requested CaseId could not be located.
--
-- See: 'CaseIdNotFound'
_CaseIdNotFound :: Prism' SupportError (Maybe Text)
_CaseIdNotFound = prism
    CaseIdNotFound
    (\case
        CaseIdNotFound p1 -> Right p1
        x -> Left x)

-- | The limit for the number of DescribeAttachment requests in a short period
-- of time has been exceeded.
--
-- See: 'DescribeAttachmentLimitExceeded'
_DescribeAttachmentLimitExceeded :: Prism' SupportError (Maybe Text)
_DescribeAttachmentLimitExceeded = prism
    DescribeAttachmentLimitExceeded
    (\case
        DescribeAttachmentLimitExceeded p1 -> Right p1
        x -> Left x)

-- | An internal server error occurred.
--
-- See: 'InternalServerError'
_InternalServerError :: Prism' SupportError (Maybe Text)
_InternalServerError = prism
    InternalServerError
    (\case
        InternalServerError p1 -> Right p1
        x -> Left x)

-- | See: 'SupportClient'
_SupportClient :: Prism' SupportError HttpException
_SupportClient = prism
    SupportClient
    (\case
        SupportClient p1 -> Right p1
        x -> Left x)

-- | See: 'SupportSerializer'
_SupportSerializer :: Prism' SupportError String
_SupportSerializer = prism
    SupportSerializer
    (\case
        SupportSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'SupportService'
_SupportService :: Prism' SupportError String
_SupportService = prism
    SupportService
    (\case
        SupportService p1 -> Right p1
        x -> Left x)

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary
    { _tacssCostOptimizing :: Maybe TrustedAdvisorCostOptimizingSummary
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorCategorySpecificSummary' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CostOptimizing ::@ @Maybe TrustedAdvisorCostOptimizingSummary@
--
trustedAdvisorCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
trustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary
    { _tacssCostOptimizing = Nothing
    }

-- | The summary information about cost savings for a Trusted Advisor check that
-- is in the Cost Optimizing category.
tacssCostOptimizing :: Lens' TrustedAdvisorCategorySpecificSummary (Maybe TrustedAdvisorCostOptimizingSummary)
tacssCostOptimizing =
    lens _tacssCostOptimizing (\s a -> s { _tacssCostOptimizing = a })

instance FromJSON TrustedAdvisorCategorySpecificSummary

instance ToJSON TrustedAdvisorCategorySpecificSummary

-- | An attachment to a case communication. The attachment consists of the file
-- name and the content of the file.
data Attachment = Attachment
    { _aFileName :: Maybe Text
    , _aData :: Maybe Base64
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Attachment' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @FileName ::@ @Maybe Text@
--
-- * @Data ::@ @Maybe Base64@
--
attachment :: Attachment
attachment = Attachment
    { _aFileName = Nothing
    , _aData = Nothing
    }

-- | The name of the attachment file.
aFileName :: Lens' Attachment (Maybe Text)
aFileName = lens _aFileName (\s a -> s { _aFileName = a })

-- | The content of the attachment file.
aData :: Lens' Attachment (Maybe Base64)
aData = lens _aData (\s a -> s { _aData = a })

instance FromJSON Attachment

instance ToJSON Attachment

-- | The file name and ID of an attachment to a case communication. You can use
-- the ID to retrieve the attachment with the DescribeAttachment operation.
data AttachmentDetails = AttachmentDetails
    { _adAttachmentId :: Maybe Text
    , _adFileName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AttachmentDetails' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttachmentId ::@ @Maybe Text@
--
-- * @FileName ::@ @Maybe Text@
--
attachmentDetails :: AttachmentDetails
attachmentDetails = AttachmentDetails
    { _adAttachmentId = Nothing
    , _adFileName = Nothing
    }

-- | The ID of the attachment.
adAttachmentId :: Lens' AttachmentDetails (Maybe Text)
adAttachmentId = lens _adAttachmentId (\s a -> s { _adAttachmentId = a })

-- | The file name of the attachment.
adFileName :: Lens' AttachmentDetails (Maybe Text)
adFileName = lens _adFileName (\s a -> s { _adFileName = a })

instance FromJSON AttachmentDetails

instance ToJSON AttachmentDetails

-- | A JSON-formatted object that contains the metadata for a support case. It
-- is contained the response from a DescribeCases request. CaseDetails
-- contains the following fields: CaseID. The AWS Support case ID requested or
-- returned in the call. The case ID is an alphanumeric string formatted as
-- shown in this example: case-12345678910-2013-c4c1d2bf33c5cf47.
-- CategoryCode. The category of problem for the AWS Support case. Corresponds
-- to the CategoryCode values returned by a call to DescribeServices.
-- DisplayId. The identifier for the case on pages in the AWS Support Center.
-- Language. The ISO 639-1 code for the language in which AWS provides
-- support. AWS Support currently supports English ("en") and Japanese ("ja").
-- Language parameters must be passed explicitly for operations that take
-- them. RecentCommunications. One or more Communication objects. Fields of
-- these objects are Attachments, Body, CaseId, SubmittedBy, and TimeCreated.
-- NextToken. A resumption point for pagination. ServiceCode. The identifier
-- for the AWS service that corresponds to the service code defined in the
-- call to DescribeServices. SeverityCode. The severity code assigned to the
-- case. Contains one of the values returned by the call to
-- DescribeSeverityLevels. Status. The status of the case in the AWS Support
-- Center. Subject. The subject line of the case. SubmittedBy. The email
-- address of the account that submitted the case. TimeCreated. The time the
-- case was created, in ISO-8601 format.
data CaseDetails = CaseDetails
    { _cdCaseId :: Maybe Text
    , _cdDisplayId :: Maybe Text
    , _cdSubject :: Maybe Text
    , _cdStatus :: Maybe Text
    , _cdServiceCode :: Maybe Text
    , _cdCategoryCode :: Maybe Text
    , _cdSeverityCode :: Maybe Text
    , _cdSubmittedBy :: Maybe Text
    , _cdTimeCreated :: Maybe Text
    , _cdRecentCommunications :: Maybe RecentCaseCommunications
    , _cdCcEmailAddresses :: [Text]
    , _cdLanguage :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CaseDetails' data type.
--
-- 'CaseDetails' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CaseId ::@ @Maybe Text@
--
-- * @DisplayId ::@ @Maybe Text@
--
-- * @Subject ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
-- * @ServiceCode ::@ @Maybe Text@
--
-- * @CategoryCode ::@ @Maybe Text@
--
-- * @SeverityCode ::@ @Maybe Text@
--
-- * @SubmittedBy ::@ @Maybe Text@
--
-- * @TimeCreated ::@ @Maybe Text@
--
-- * @RecentCommunications ::@ @Maybe RecentCaseCommunications@
--
-- * @CcEmailAddresses ::@ @[Text]@
--
-- * @Language ::@ @Maybe Text@
--
caseDetails :: CaseDetails
caseDetails = CaseDetails
    { _cdCaseId = Nothing
    , _cdDisplayId = Nothing
    , _cdSubject = Nothing
    , _cdStatus = Nothing
    , _cdServiceCode = Nothing
    , _cdCategoryCode = Nothing
    , _cdSeverityCode = Nothing
    , _cdSubmittedBy = Nothing
    , _cdTimeCreated = Nothing
    , _cdRecentCommunications = Nothing
    , _cdCcEmailAddresses = mempty
    , _cdLanguage = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
cdCaseId :: Lens' CaseDetails (Maybe Text)
cdCaseId = lens _cdCaseId (\s a -> s { _cdCaseId = a })

-- | The ID displayed for the case in the AWS Support Center. This is a numeric
-- string.
cdDisplayId :: Lens' CaseDetails (Maybe Text)
cdDisplayId = lens _cdDisplayId (\s a -> s { _cdDisplayId = a })

-- | The subject line for the case in the AWS Support Center.
cdSubject :: Lens' CaseDetails (Maybe Text)
cdSubject = lens _cdSubject (\s a -> s { _cdSubject = a })

-- | The status of the case.
cdStatus :: Lens' CaseDetails (Maybe Text)
cdStatus = lens _cdStatus (\s a -> s { _cdStatus = a })

-- | The code for the AWS service returned by the call to DescribeServices.
cdServiceCode :: Lens' CaseDetails (Maybe Text)
cdServiceCode = lens _cdServiceCode (\s a -> s { _cdServiceCode = a })

-- | The category of problem for the AWS Support case.
cdCategoryCode :: Lens' CaseDetails (Maybe Text)
cdCategoryCode = lens _cdCategoryCode (\s a -> s { _cdCategoryCode = a })

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
cdSeverityCode :: Lens' CaseDetails (Maybe Text)
cdSeverityCode = lens _cdSeverityCode (\s a -> s { _cdSeverityCode = a })

-- | The email address of the account that submitted the case.
cdSubmittedBy :: Lens' CaseDetails (Maybe Text)
cdSubmittedBy = lens _cdSubmittedBy (\s a -> s { _cdSubmittedBy = a })

-- | The time that the case was case created in the AWS Support Center.
cdTimeCreated :: Lens' CaseDetails (Maybe Text)
cdTimeCreated = lens _cdTimeCreated (\s a -> s { _cdTimeCreated = a })

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also includes a
-- nextToken that you can use to retrieve earlier communications.
cdRecentCommunications :: Lens' CaseDetails (Maybe RecentCaseCommunications)
cdRecentCommunications =
    lens _cdRecentCommunications (\s a -> s { _cdRecentCommunications = a })

-- | The email addresses that receive copies of communication about the case.
cdCcEmailAddresses :: Lens' CaseDetails [Text]
cdCcEmailAddresses =
    lens _cdCcEmailAddresses (\s a -> s { _cdCcEmailAddresses = a })

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
cdLanguage :: Lens' CaseDetails (Maybe Text)
cdLanguage = lens _cdLanguage (\s a -> s { _cdLanguage = a })

instance FromJSON CaseDetails

-- | A JSON-formatted name/value pair that represents the category name and
-- category code of the problem, selected from the DescribeServices response
-- for each AWS service.
data Category = Category
    { _c1Code :: Maybe Text
    , _c1Name :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Category' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
category :: Category
category = Category
    { _c1Code = Nothing
    , _c1Name = Nothing
    }

-- | The category code for the support case.
c1Code :: Lens' Category (Maybe Text)
c1Code = lens _c1Code (\s a -> s { _c1Code = a })

-- | The category name for the support case.
c1Name :: Lens' Category (Maybe Text)
c1Name = lens _c1Name (\s a -> s { _c1Name = a })

instance FromJSON Category

instance ToJSON Category

-- | A communication associated with an AWS Support case. The communication
-- consists of the case ID, the message body, attachment information, the
-- account email address, and the date and time of the communication.
data Communication = Communication
    { _cCaseId :: Maybe Text
    , _cBody :: Maybe Text
    , _cSubmittedBy :: Maybe Text
    , _cTimeCreated :: Maybe Text
    , _cAttachmentSet :: [AttachmentDetails]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Communication' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CaseId ::@ @Maybe Text@
--
-- * @Body ::@ @Maybe Text@
--
-- * @SubmittedBy ::@ @Maybe Text@
--
-- * @TimeCreated ::@ @Maybe Text@
--
-- * @AttachmentSet ::@ @[AttachmentDetails]@
--
communication :: Communication
communication = Communication
    { _cCaseId = Nothing
    , _cBody = Nothing
    , _cSubmittedBy = Nothing
    , _cTimeCreated = Nothing
    , _cAttachmentSet = mempty
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
cCaseId :: Lens' Communication (Maybe Text)
cCaseId = lens _cCaseId (\s a -> s { _cCaseId = a })

-- | The text of the communication between the customer and AWS Support.
cBody :: Lens' Communication (Maybe Text)
cBody = lens _cBody (\s a -> s { _cBody = a })

-- | The email address of the account that submitted the AWS Support case.
cSubmittedBy :: Lens' Communication (Maybe Text)
cSubmittedBy = lens _cSubmittedBy (\s a -> s { _cSubmittedBy = a })

-- | The time the communication was created.
cTimeCreated :: Lens' Communication (Maybe Text)
cTimeCreated = lens _cTimeCreated (\s a -> s { _cTimeCreated = a })

-- | Information about the attachments to the case communication.
cAttachmentSet :: Lens' Communication [AttachmentDetails]
cAttachmentSet = lens _cAttachmentSet (\s a -> s { _cAttachmentSet = a })

instance FromJSON Communication

instance ToJSON Communication

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also includes a
-- nextToken that you can use to retrieve earlier communications.
data RecentCaseCommunications = RecentCaseCommunications
    { _rccCommunications :: [Communication]
    , _rccNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecentCaseCommunications' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Communications ::@ @[Communication]@
--
-- * @NextToken ::@ @Maybe Text@
--
recentCaseCommunications :: RecentCaseCommunications
recentCaseCommunications = RecentCaseCommunications
    { _rccCommunications = mempty
    , _rccNextToken = Nothing
    }

-- | The five most recent communications associated with the case.
rccCommunications :: Lens' RecentCaseCommunications [Communication]
rccCommunications =
    lens _rccCommunications (\s a -> s { _rccCommunications = a })

-- | A resumption point for pagination.
rccNextToken :: Lens' RecentCaseCommunications (Maybe Text)
rccNextToken = lens _rccNextToken (\s a -> s { _rccNextToken = a })

instance FromJSON RecentCaseCommunications

instance ToJSON RecentCaseCommunications

-- | Information about an AWS service returned by the DescribeServices
-- operation.
data Service' = Service'
    { _sCode :: Maybe Text
    , _sName :: Maybe Text
    , _sCategories :: [Category]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Service'' data type.
--
-- 'Service'' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Categories ::@ @[Category]@
--
service' :: Service'
service' = Service'
    { _sCode = Nothing
    , _sName = Nothing
    , _sCategories = mempty
    }

-- | The code for an AWS service returned by the DescribeServices response. The
-- Name element contains the corresponding friendly name.
sCode :: Lens' Service' (Maybe Text)
sCode = lens _sCode (\s a -> s { _sCode = a })

-- | The friendly name for an AWS service. The Code element contains the
-- corresponding code.
sName :: Lens' Service' (Maybe Text)
sName = lens _sName (\s a -> s { _sName = a })

-- | A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to AWS Support when you call
-- CreateCase.
sCategories :: Lens' Service' [Category]
sCategories = lens _sCategories (\s a -> s { _sCategories = a })

instance FromJSON Service'

-- | A code and name pair that represent a severity level that can be applied to
-- a support case.
data SeverityLevel = SeverityLevel
    { _slCode :: Maybe Text
    , _slName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SeverityLevel' data type.
--
-- 'SeverityLevel' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
severityLevel :: SeverityLevel
severityLevel = SeverityLevel
    { _slCode = Nothing
    , _slName = Nothing
    }

-- | One of four values: "low," "medium," "high," and "urgent". These values
-- correspond to response times returned to the caller in SeverityLevel.name.
slCode :: Lens' SeverityLevel (Maybe Text)
slCode = lens _slCode (\s a -> s { _slCode = a })

-- | The name of the severity level that corresponds to the severity level code.
slName :: Lens' SeverityLevel (Maybe Text)
slName = lens _slName (\s a -> s { _slName = a })

instance FromJSON SeverityLevel

-- | The description and metadata for a Trusted Advisor check.
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription
    { _tacdId :: Text
    , _tacdName :: Text
    , _tacdDescription :: Text
    , _tacdCategory :: Text
    , _tacdMetadata :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorCheckDescription' data type.
--
-- 'TrustedAdvisorCheckDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @Name ::@ @Text@
--
-- * @Description ::@ @Text@
--
-- * @Category ::@ @Text@
--
-- * @Metadata ::@ @[Text]@
--
trustedAdvisorCheckDescription :: Text -- ^ 'tacdId'
                               -> Text -- ^ 'tacdName'
                               -> Text -- ^ 'tacdDescription'
                               -> Text -- ^ 'tacdCategory'
                               -> [Text] -- ^ 'tacdMetadata'
                               -> TrustedAdvisorCheckDescription
trustedAdvisorCheckDescription p1 p2 p3 p4 p5 = TrustedAdvisorCheckDescription
    { _tacdId = p1
    , _tacdName = p2
    , _tacdDescription = p3
    , _tacdCategory = p4
    , _tacdMetadata = p5
    }

-- | The unique identifier for the Trusted Advisor check.
tacdId :: Lens' TrustedAdvisorCheckDescription Text
tacdId = lens _tacdId (\s a -> s { _tacdId = a })

-- | The display name for the Trusted Advisor check.
tacdName :: Lens' TrustedAdvisorCheckDescription Text
tacdName = lens _tacdName (\s a -> s { _tacdName = a })

-- | The description of the Trusted Advisor check, which includes the alert
-- criteria and recommended actions (contains HTML markup).
tacdDescription :: Lens' TrustedAdvisorCheckDescription Text
tacdDescription = lens _tacdDescription (\s a -> s { _tacdDescription = a })

-- | The category of the Trusted Advisor check.
tacdCategory :: Lens' TrustedAdvisorCheckDescription Text
tacdCategory = lens _tacdCategory (\s a -> s { _tacdCategory = a })

-- | The column headings for the data returned by the Trusted Advisor check. The
-- order of the headings corresponds to the order of the data in the Metadata
-- element of the TrustedAdvisorResourceDetail for the check. Metadata
-- contains all the data that is shown in the Excel download, even in those
-- cases where the UI shows just summary data.
tacdMetadata :: Lens' TrustedAdvisorCheckDescription [Text]
tacdMetadata = lens _tacdMetadata (\s a -> s { _tacdMetadata = a })

instance FromJSON TrustedAdvisorCheckDescription

-- | The refresh status of a Trusted Advisor check.
data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus
    { _tacrsCheckId :: Text
    , _tacrsStatus :: Text
    , _tacrsMillisUntilNextRefreshable :: !Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorCheckRefreshStatus' data type.
--
-- 'TrustedAdvisorCheckRefreshStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CheckId ::@ @Text@
--
-- * @Status ::@ @Text@
--
-- * @MillisUntilNextRefreshable ::@ @Integer@
--
trustedAdvisorCheckRefreshStatus :: Text -- ^ 'tacrsCheckId'
                                 -> Text -- ^ 'tacrsStatus'
                                 -> Integer -- ^ 'tacrsMillisUntilNextRefreshable'
                                 -> TrustedAdvisorCheckRefreshStatus
trustedAdvisorCheckRefreshStatus p1 p2 p3 = TrustedAdvisorCheckRefreshStatus
    { _tacrsCheckId = p1
    , _tacrsStatus = p2
    , _tacrsMillisUntilNextRefreshable = p3
    }

-- | The unique identifier for the Trusted Advisor check.
tacrsCheckId :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsCheckId = lens _tacrsCheckId (\s a -> s { _tacrsCheckId = a })

-- | The status of the Trusted Advisor check for which a refresh has been
-- requested: "none", "enqueued", "processing", "success", or "abandoned".
tacrsStatus :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsStatus = lens _tacrsStatus (\s a -> s { _tacrsStatus = a })

-- | The amount of time, in milliseconds, until the Trusted Advisor check is
-- eligible for refresh.
tacrsMillisUntilNextRefreshable :: Lens' TrustedAdvisorCheckRefreshStatus Integer
tacrsMillisUntilNextRefreshable =
    lens _tacrsMillisUntilNextRefreshable
         (\s a -> s { _tacrsMillisUntilNextRefreshable = a })

instance FromJSON TrustedAdvisorCheckRefreshStatus

-- | The detailed results of the Trusted Advisor check.
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult
    { _tacrCheckId :: Text
    , _tacrTimestamp :: Text
    , _tacrStatus :: Text
    , _tacrResourcesSummary :: TrustedAdvisorResourcesSummary
    , _tacrCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
    , _tacrFlaggedResources :: [TrustedAdvisorResourceDetail]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorCheckResult' data type.
--
-- 'TrustedAdvisorCheckResult' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CheckId ::@ @Text@
--
-- * @Timestamp ::@ @Text@
--
-- * @Status ::@ @Text@
--
-- * @ResourcesSummary ::@ @TrustedAdvisorResourcesSummary@
--
-- * @CategorySpecificSummary ::@ @TrustedAdvisorCategorySpecificSummary@
--
-- * @FlaggedResources ::@ @[TrustedAdvisorResourceDetail]@
--
trustedAdvisorCheckResult :: Text -- ^ 'tacrCheckId'
                          -> Text -- ^ 'tacrTimestamp'
                          -> Text -- ^ 'tacrStatus'
                          -> TrustedAdvisorResourcesSummary -- ^ 'tacrResourcesSummary'
                          -> TrustedAdvisorCategorySpecificSummary -- ^ 'tacrCategorySpecificSummary'
                          -> [TrustedAdvisorResourceDetail] -- ^ 'tacrFlaggedResources'
                          -> TrustedAdvisorCheckResult
trustedAdvisorCheckResult p1 p2 p3 p4 p5 p6 = TrustedAdvisorCheckResult
    { _tacrCheckId = p1
    , _tacrTimestamp = p2
    , _tacrStatus = p3
    , _tacrResourcesSummary = p4
    , _tacrCategorySpecificSummary = p5
    , _tacrFlaggedResources = p6
    }

-- | The unique identifier for the Trusted Advisor check.
tacrCheckId :: Lens' TrustedAdvisorCheckResult Text
tacrCheckId = lens _tacrCheckId (\s a -> s { _tacrCheckId = a })

-- | The time of the last refresh of the check.
tacrTimestamp :: Lens' TrustedAdvisorCheckResult Text
tacrTimestamp = lens _tacrTimestamp (\s a -> s { _tacrTimestamp = a })

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacrStatus :: Lens' TrustedAdvisorCheckResult Text
tacrStatus = lens _tacrStatus (\s a -> s { _tacrStatus = a })

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
tacrResourcesSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorResourcesSummary
tacrResourcesSummary =
    lens _tacrResourcesSummary (\s a -> s { _tacrResourcesSummary = a })

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacrCategorySpecificSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorCategorySpecificSummary
tacrCategorySpecificSummary =
    lens _tacrCategorySpecificSummary
         (\s a -> s { _tacrCategorySpecificSummary = a })

-- | The details about each resource listed in the check result.
tacrFlaggedResources :: Lens' TrustedAdvisorCheckResult [TrustedAdvisorResourceDetail]
tacrFlaggedResources =
    lens _tacrFlaggedResources (\s a -> s { _tacrFlaggedResources = a })

instance FromJSON TrustedAdvisorCheckResult

-- | A summary of a Trusted Advisor check result, including the alert status,
-- last refresh, and number of resources examined.
data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary
    { _tacsCheckId :: Text
    , _tacsTimestamp :: Text
    , _tacsStatus :: Text
    , _tacsHasFlaggedResources :: Maybe Bool
    , _tacsResourcesSummary :: TrustedAdvisorResourcesSummary
    , _tacsCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorCheckSummary' data type.
--
-- 'TrustedAdvisorCheckSummary' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CheckId ::@ @Text@
--
-- * @Timestamp ::@ @Text@
--
-- * @Status ::@ @Text@
--
-- * @HasFlaggedResources ::@ @Maybe Bool@
--
-- * @ResourcesSummary ::@ @TrustedAdvisorResourcesSummary@
--
-- * @CategorySpecificSummary ::@ @TrustedAdvisorCategorySpecificSummary@
--
trustedAdvisorCheckSummary :: Text -- ^ 'tacsCheckId'
                           -> Text -- ^ 'tacsTimestamp'
                           -> Text -- ^ 'tacsStatus'
                           -> TrustedAdvisorResourcesSummary -- ^ 'tacsResourcesSummary'
                           -> TrustedAdvisorCategorySpecificSummary -- ^ 'tacsCategorySpecificSummary'
                           -> TrustedAdvisorCheckSummary
trustedAdvisorCheckSummary p1 p2 p3 p5 p6 = TrustedAdvisorCheckSummary
    { _tacsCheckId = p1
    , _tacsTimestamp = p2
    , _tacsStatus = p3
    , _tacsHasFlaggedResources = Nothing
    , _tacsResourcesSummary = p5
    , _tacsCategorySpecificSummary = p6
    }

-- | The unique identifier for the Trusted Advisor check.
tacsCheckId :: Lens' TrustedAdvisorCheckSummary Text
tacsCheckId = lens _tacsCheckId (\s a -> s { _tacsCheckId = a })

-- | The time of the last refresh of the check.
tacsTimestamp :: Lens' TrustedAdvisorCheckSummary Text
tacsTimestamp = lens _tacsTimestamp (\s a -> s { _tacsTimestamp = a })

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacsStatus :: Lens' TrustedAdvisorCheckSummary Text
tacsStatus = lens _tacsStatus (\s a -> s { _tacsStatus = a })

-- | Specifies whether the Trusted Advisor check has flagged resources.
tacsHasFlaggedResources :: Lens' TrustedAdvisorCheckSummary (Maybe Bool)
tacsHasFlaggedResources =
    lens _tacsHasFlaggedResources
         (\s a -> s { _tacsHasFlaggedResources = a })

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
tacsResourcesSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorResourcesSummary
tacsResourcesSummary =
    lens _tacsResourcesSummary (\s a -> s { _tacsResourcesSummary = a })

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacsCategorySpecificSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorCategorySpecificSummary
tacsCategorySpecificSummary =
    lens _tacsCategorySpecificSummary
         (\s a -> s { _tacsCategorySpecificSummary = a })

instance FromJSON TrustedAdvisorCheckSummary

-- | The summary information about cost savings for a Trusted Advisor check that
-- is in the Cost Optimizing category.
data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary
    { _tacosEstimatedMonthlySavings :: !Double
    , _tacosEstimatedPercentMonthlySavings :: !Double
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorCostOptimizingSummary' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EstimatedMonthlySavings ::@ @Double@
--
-- * @EstimatedPercentMonthlySavings ::@ @Double@
--
trustedAdvisorCostOptimizingSummary :: Double -- ^ 'tacosEstimatedMonthlySavings'
                                    -> Double -- ^ 'tacosEstimatedPercentMonthlySavings'
                                    -> TrustedAdvisorCostOptimizingSummary
trustedAdvisorCostOptimizingSummary p1 p2 = TrustedAdvisorCostOptimizingSummary
    { _tacosEstimatedMonthlySavings = p1
    , _tacosEstimatedPercentMonthlySavings = p2
    }

-- | The estimated monthly savings that might be realized if the recommended
-- actions are taken.
tacosEstimatedMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary Double
tacosEstimatedMonthlySavings =
    lens _tacosEstimatedMonthlySavings
         (\s a -> s { _tacosEstimatedMonthlySavings = a })

-- | The estimated percentage of savings that might be realized if the
-- recommended actions are taken.
tacosEstimatedPercentMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary Double
tacosEstimatedPercentMonthlySavings =
    lens _tacosEstimatedPercentMonthlySavings
         (\s a -> s { _tacosEstimatedPercentMonthlySavings = a })

instance FromJSON TrustedAdvisorCostOptimizingSummary

instance ToJSON TrustedAdvisorCostOptimizingSummary

-- | Contains information about a resource identified by a Trusted Advisor
-- check.
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail
    { _tardStatus :: Text
    , _tardRegion :: Text
    , _tardResourceId :: Text
    , _tardIsSuppressed :: Maybe Bool
    , _tardMetadata :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorResourceDetail' data type.
--
-- 'TrustedAdvisorResourceDetail' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Text@
--
-- * @Region ::@ @Text@
--
-- * @ResourceId ::@ @Text@
--
-- * @IsSuppressed ::@ @Maybe Bool@
--
-- * @Metadata ::@ @[Text]@
--
trustedAdvisorResourceDetail :: Text -- ^ 'tardStatus'
                             -> Text -- ^ 'tardRegion'
                             -> Text -- ^ 'tardResourceId'
                             -> [Text] -- ^ 'tardMetadata'
                             -> TrustedAdvisorResourceDetail
trustedAdvisorResourceDetail p1 p2 p3 p5 = TrustedAdvisorResourceDetail
    { _tardStatus = p1
    , _tardRegion = p2
    , _tardResourceId = p3
    , _tardIsSuppressed = Nothing
    , _tardMetadata = p5
    }

-- | The status code for the resource identified in the Trusted Advisor check.
tardStatus :: Lens' TrustedAdvisorResourceDetail Text
tardStatus = lens _tardStatus (\s a -> s { _tardStatus = a })

-- | The AWS region in which the identified resource is located.
tardRegion :: Lens' TrustedAdvisorResourceDetail Text
tardRegion = lens _tardRegion (\s a -> s { _tardRegion = a })

-- | The unique identifier for the identified resource.
tardResourceId :: Lens' TrustedAdvisorResourceDetail Text
tardResourceId = lens _tardResourceId (\s a -> s { _tardResourceId = a })

-- | Specifies whether the AWS resource was ignored by Trusted Advisor because
-- it was marked as suppressed by the user.
tardIsSuppressed :: Lens' TrustedAdvisorResourceDetail (Maybe Bool)
tardIsSuppressed =
    lens _tardIsSuppressed (\s a -> s { _tardIsSuppressed = a })

-- | Additional information about the identified resource. The exact metadata
-- and its order can be obtained by inspecting the
-- TrustedAdvisorCheckDescription object returned by the call to
-- DescribeTrustedAdvisorChecks. Metadata contains all the data that is shown
-- in the Excel download, even in those cases where the UI shows just summary
-- data.
tardMetadata :: Lens' TrustedAdvisorResourceDetail [Text]
tardMetadata = lens _tardMetadata (\s a -> s { _tardMetadata = a })

instance FromJSON TrustedAdvisorResourceDetail

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary
    { _tarsResourcesProcessed :: !Integer
    , _tarsResourcesFlagged :: !Integer
    , _tarsResourcesIgnored :: !Integer
    , _tarsResourcesSuppressed :: !Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorResourcesSummary' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourcesProcessed ::@ @Integer@
--
-- * @ResourcesFlagged ::@ @Integer@
--
-- * @ResourcesIgnored ::@ @Integer@
--
-- * @ResourcesSuppressed ::@ @Integer@
--
trustedAdvisorResourcesSummary :: Integer -- ^ 'tarsResourcesProcessed'
                               -> Integer -- ^ 'tarsResourcesFlagged'
                               -> Integer -- ^ 'tarsResourcesIgnored'
                               -> Integer -- ^ 'tarsResourcesSuppressed'
                               -> TrustedAdvisorResourcesSummary
trustedAdvisorResourcesSummary p1 p2 p3 p4 = TrustedAdvisorResourcesSummary
    { _tarsResourcesProcessed = p1
    , _tarsResourcesFlagged = p2
    , _tarsResourcesIgnored = p3
    , _tarsResourcesSuppressed = p4
    }

-- | The number of AWS resources that were analyzed by the Trusted Advisor
-- check.
tarsResourcesProcessed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesProcessed =
    lens _tarsResourcesProcessed (\s a -> s { _tarsResourcesProcessed = a })

-- | The number of AWS resources that were flagged (listed) by the Trusted
-- Advisor check.
tarsResourcesFlagged :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesFlagged =
    lens _tarsResourcesFlagged (\s a -> s { _tarsResourcesFlagged = a })

-- | The number of AWS resources ignored by Trusted Advisor because information
-- was unavailable.
tarsResourcesIgnored :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesIgnored =
    lens _tarsResourcesIgnored (\s a -> s { _tarsResourcesIgnored = a })

-- | The number of AWS resources ignored by Trusted Advisor because they were
-- marked as suppressed by the user.
tarsResourcesSuppressed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesSuppressed =
    lens _tarsResourcesSuppressed
         (\s a -> s { _tarsResourcesSuppressed = a })

instance FromJSON TrustedAdvisorResourcesSummary

instance ToJSON TrustedAdvisorResourcesSummary
