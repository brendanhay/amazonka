{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.Types
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
module Network.AWS.Support.V2013_04_15.Types
    (
    -- * Service
      Support
    -- ** Errors
    , Er (..)
    -- * TrustedAdvisorCategorySpecificSummary
    , TrustedAdvisorCategorySpecificSummary
    , mkTrustedAdvisorCategorySpecificSummary
    , tacssCostOptimizing

    -- * Attachment
    , Attachment
    , mkAttachment
    , bFileName
    , bData

    -- * AttachmentDetails
    , AttachmentDetails
    , mkAttachmentDetails
    , adAttachmentId
    , adFileName

    -- * CaseDetails
    , CaseDetails
    , ceCaseId
    , ceDisplayId
    , ceSubject
    , ceStatus
    , ceServiceCode
    , ceCategoryCode
    , ceSeverityCode
    , ceSubmittedBy
    , ceTimeCreated
    , ceRecentCommunications
    , ceCcEmailAddresses
    , ceLanguage

    -- * Category
    , Category
    , mkCategory
    , hCode
    , hName

    -- * Communication
    , Communication
    , mkCommunication
    , fCaseId
    , fBody
    , fSubmittedBy
    , fTimeCreated
    , fAttachmentSet

    -- * RecentCaseCommunications
    , RecentCaseCommunications
    , mkRecentCaseCommunications
    , rccCommunications
    , rccNextToken

    -- * Service
    , Service
    , vCode
    , vName
    , vCategories

    -- * SeverityLevel
    , SeverityLevel
    , smCode
    , smName

    -- * TrustedAdvisorCheckDescription
    , TrustedAdvisorCheckDescription
    , tacdId
    , tacdName
    , tacdDescription
    , tacdCategory
    , tacdMetadata

    -- * TrustedAdvisorCheckRefreshStatus
    , TrustedAdvisorCheckRefreshStatus
    , tacrsCheckId
    , tacrsStatus
    , tacrsMillisUntilNextRefreshable

    -- * TrustedAdvisorCheckResult
    , TrustedAdvisorCheckResult
    , tacrCheckId
    , tacrTimestamp
    , tacrStatus
    , tacrResourcesSummary
    , tacrCategorySpecificSummary
    , tacrFlaggedResources

    -- * TrustedAdvisorCheckSummary
    , TrustedAdvisorCheckSummary
    , tacsCheckId
    , tacsTimestamp
    , tacsStatus
    , tacsHasFlaggedResources
    , tacsResourcesSummary
    , tacsCategorySpecificSummary

    -- * TrustedAdvisorCostOptimizingSummary
    , TrustedAdvisorCostOptimizingSummary
    , mkTrustedAdvisorCostOptimizingSummary
    , tacosEstimatedMonthlySavings
    , tacosEstimatedPercentMonthlySavings

    -- * TrustedAdvisorResourceDetail
    , TrustedAdvisorResourceDetail
    , tardStatus
    , tardRegion
    , tardResourceId
    , tardIsSuppressed
    , tardMetadata

    -- * TrustedAdvisorResourcesSummary
    , TrustedAdvisorResourcesSummary
    , mkTrustedAdvisorResourcesSummary
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
    data Er Support
        = AttachmentIdNotFound
            { _ainfMessage :: Maybe Text
            }
        | AttachmentLimitExceeded
            { _aleMessage :: Maybe Text
            }
        | AttachmentSetExpired
            { _aseMessage :: Maybe Text
            }
        | AttachmentSetIdNotFound
            { _asinfMessage :: Maybe Text
            }
        | AttachmentSetSizeLimitExceeded
            { _assleMessage :: Maybe Text
            }
        | CaseCreationLimitExceeded
            { _ccleMessage :: Maybe Text
            }
        | CaseIdNotFound
            { _cinfMessage :: Maybe Text
            }
        | DescribeAttachmentLimitExceeded
            { _daleMessage :: Maybe Text
            }
        | InternalServerError
            { _iseMessage :: Maybe Text
            }
        | SupportClient HttpException
        | SupportSerializer String
        | SupportService String

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "support"
        , _svcVersion  = "2013-04-15"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er Support)
deriving instance Generic (Er Support)

instance AWSError (Er Support) where
    awsError = const "SupportError"

instance AWSServiceError (Er Support) where
    serviceError    = SupportService
    clientError     = SupportClient
    serializerError = SupportSerializer

instance Exception (Er Support)

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary
    { _tacssCostOptimizing :: Maybe TrustedAdvisorCostOptimizingSummary
      -- ^ The summary information about cost savings for a Trusted Advisor
      -- check that is in the Cost Optimizing category.
    } deriving (Show, Generic)

-- | The summary information about cost savings for a Trusted Advisor check that
-- is in the Cost Optimizing category.
tacssCostOptimizing :: Lens' TrustedAdvisorCategorySpecificSummary (Maybe TrustedAdvisorCostOptimizingSummary)
tacssCostOptimizing = lens _tacssCostOptimizing (\s a -> s { _tacssCostOptimizing = a })
{-# INLINE tacssCostOptimizing #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorCategorySpecificSummary' data type to populate a request.
mkTrustedAdvisorCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
mkTrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary
    { _tacssCostOptimizing = Nothing
    }
{-# INLINE mkTrustedAdvisorCategorySpecificSummary #-}

instance FromJSON TrustedAdvisorCategorySpecificSummary

instance ToJSON TrustedAdvisorCategorySpecificSummary

-- | An attachment to a case communication. The attachment consists of the file
-- name and the content of the file.
data Attachment = Attachment
    { _bFileName :: Maybe Text
      -- ^ The name of the attachment file.
    , _bData :: Maybe Base64
      -- ^ The content of the attachment file.
    } deriving (Show, Generic)

-- | The name of the attachment file.
bFileName :: Lens' Attachment (Maybe Text)
bFileName = lens _bFileName (\s a -> s { _bFileName = a })
{-# INLINE bFileName #-}

-- | The content of the attachment file.
bData :: Lens' Attachment (Maybe Base64)
bData = lens _bData (\s a -> s { _bData = a })
{-# INLINE bData #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Attachment' data type to populate a request.
mkAttachment :: Attachment
mkAttachment = Attachment
    { _bFileName = Nothing
    , _bData = Nothing
    }
{-# INLINE mkAttachment #-}

instance FromJSON Attachment

instance ToJSON Attachment

-- | The file name and ID of an attachment to a case communication. You can use
-- the ID to retrieve the attachment with the DescribeAttachment operation.
data AttachmentDetails = AttachmentDetails
    { _adAttachmentId :: Maybe Text
      -- ^ The ID of the attachment.
    , _adFileName :: Maybe Text
      -- ^ The file name of the attachment.
    } deriving (Show, Generic)

-- | The ID of the attachment.
adAttachmentId :: Lens' AttachmentDetails (Maybe Text)
adAttachmentId = lens _adAttachmentId (\s a -> s { _adAttachmentId = a })
{-# INLINE adAttachmentId #-}

-- | The file name of the attachment.
adFileName :: Lens' AttachmentDetails (Maybe Text)
adFileName = lens _adFileName (\s a -> s { _adFileName = a })
{-# INLINE adFileName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AttachmentDetails' data type to populate a request.
mkAttachmentDetails :: AttachmentDetails
mkAttachmentDetails = AttachmentDetails
    { _adAttachmentId = Nothing
    , _adFileName = Nothing
    }
{-# INLINE mkAttachmentDetails #-}

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
    { _ceCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    , _ceDisplayId :: Maybe Text
      -- ^ The ID displayed for the case in the AWS Support Center. This is
      -- a numeric string.
    , _ceSubject :: Maybe Text
      -- ^ The subject line for the case in the AWS Support Center.
    , _ceStatus :: Maybe Text
      -- ^ The status of the case.
    , _ceServiceCode :: Maybe Text
      -- ^ The code for the AWS service returned by the call to
      -- DescribeServices.
    , _ceCategoryCode :: Maybe Text
      -- ^ The category of problem for the AWS Support case.
    , _ceSeverityCode :: Maybe Text
      -- ^ The code for the severity level returned by the call to
      -- DescribeSeverityLevels.
    , _ceSubmittedBy :: Maybe Text
      -- ^ The email address of the account that submitted the case.
    , _ceTimeCreated :: Maybe Text
      -- ^ The time that the case was case created in the AWS Support
      -- Center.
    , _ceRecentCommunications :: Maybe RecentCaseCommunications
      -- ^ The five most recent communications between you and AWS Support
      -- Center, including the IDs of any attachments to the
      -- communications. Also includes a nextToken that you can use to
      -- retrieve earlier communications.
    , _ceCcEmailAddresses :: [Text]
      -- ^ The email addresses that receive copies of communication about
      -- the case.
    , _ceLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides
      -- support. AWS Support currently supports English ("en") and
      -- Japanese ("ja"). Language parameters must be passed explicitly
      -- for operations that take them.
    } deriving (Show, Generic)

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
ceCaseId :: Lens' CaseDetails (Maybe Text)
ceCaseId = lens _ceCaseId (\s a -> s { _ceCaseId = a })
{-# INLINE ceCaseId #-}

-- | The ID displayed for the case in the AWS Support Center. This is a numeric
-- string.
ceDisplayId :: Lens' CaseDetails (Maybe Text)
ceDisplayId = lens _ceDisplayId (\s a -> s { _ceDisplayId = a })
{-# INLINE ceDisplayId #-}

-- | The subject line for the case in the AWS Support Center.
ceSubject :: Lens' CaseDetails (Maybe Text)
ceSubject = lens _ceSubject (\s a -> s { _ceSubject = a })
{-# INLINE ceSubject #-}

-- | The status of the case.
ceStatus :: Lens' CaseDetails (Maybe Text)
ceStatus = lens _ceStatus (\s a -> s { _ceStatus = a })
{-# INLINE ceStatus #-}

-- | The code for the AWS service returned by the call to DescribeServices.
ceServiceCode :: Lens' CaseDetails (Maybe Text)
ceServiceCode = lens _ceServiceCode (\s a -> s { _ceServiceCode = a })
{-# INLINE ceServiceCode #-}

-- | The category of problem for the AWS Support case.
ceCategoryCode :: Lens' CaseDetails (Maybe Text)
ceCategoryCode = lens _ceCategoryCode (\s a -> s { _ceCategoryCode = a })
{-# INLINE ceCategoryCode #-}

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
ceSeverityCode :: Lens' CaseDetails (Maybe Text)
ceSeverityCode = lens _ceSeverityCode (\s a -> s { _ceSeverityCode = a })
{-# INLINE ceSeverityCode #-}

-- | The email address of the account that submitted the case.
ceSubmittedBy :: Lens' CaseDetails (Maybe Text)
ceSubmittedBy = lens _ceSubmittedBy (\s a -> s { _ceSubmittedBy = a })
{-# INLINE ceSubmittedBy #-}

-- | The time that the case was case created in the AWS Support Center.
ceTimeCreated :: Lens' CaseDetails (Maybe Text)
ceTimeCreated = lens _ceTimeCreated (\s a -> s { _ceTimeCreated = a })
{-# INLINE ceTimeCreated #-}

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also includes a
-- nextToken that you can use to retrieve earlier communications.
ceRecentCommunications :: Lens' CaseDetails (Maybe RecentCaseCommunications)
ceRecentCommunications = lens _ceRecentCommunications (\s a -> s { _ceRecentCommunications = a })
{-# INLINE ceRecentCommunications #-}

-- | The email addresses that receive copies of communication about the case.
ceCcEmailAddresses :: Lens' CaseDetails ([Text])
ceCcEmailAddresses = lens _ceCcEmailAddresses (\s a -> s { _ceCcEmailAddresses = a })
{-# INLINE ceCcEmailAddresses #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
ceLanguage :: Lens' CaseDetails (Maybe Text)
ceLanguage = lens _ceLanguage (\s a -> s { _ceLanguage = a })
{-# INLINE ceLanguage #-}

instance FromJSON CaseDetails

-- | A JSON-formatted name/value pair that represents the category name and
-- category code of the problem, selected from the DescribeServices response
-- for each AWS service.
data Category = Category
    { _hCode :: Maybe Text
      -- ^ The category code for the support case.
    , _hName :: Maybe Text
      -- ^ The category name for the support case.
    } deriving (Show, Generic)

-- | The category code for the support case.
hCode :: Lens' Category (Maybe Text)
hCode = lens _hCode (\s a -> s { _hCode = a })
{-# INLINE hCode #-}

-- | The category name for the support case.
hName :: Lens' Category (Maybe Text)
hName = lens _hName (\s a -> s { _hName = a })
{-# INLINE hName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Category' data type to populate a request.
mkCategory :: Category
mkCategory = Category
    { _hCode = Nothing
    , _hName = Nothing
    }
{-# INLINE mkCategory #-}

instance FromJSON Category

instance ToJSON Category

-- | A communication associated with an AWS Support case. The communication
-- consists of the case ID, the message body, attachment information, the
-- account email address, and the date and time of the communication.
data Communication = Communication
    { _fCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    , _fBody :: Maybe Text
      -- ^ The text of the communication between the customer and AWS
      -- Support.
    , _fSubmittedBy :: Maybe Text
      -- ^ The email address of the account that submitted the AWS Support
      -- case.
    , _fTimeCreated :: Maybe Text
      -- ^ The time the communication was created.
    , _fAttachmentSet :: [AttachmentDetails]
      -- ^ Information about the attachments to the case communication.
    } deriving (Show, Generic)

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
fCaseId :: Lens' Communication (Maybe Text)
fCaseId = lens _fCaseId (\s a -> s { _fCaseId = a })
{-# INLINE fCaseId #-}

-- | The text of the communication between the customer and AWS Support.
fBody :: Lens' Communication (Maybe Text)
fBody = lens _fBody (\s a -> s { _fBody = a })
{-# INLINE fBody #-}

-- | The email address of the account that submitted the AWS Support case.
fSubmittedBy :: Lens' Communication (Maybe Text)
fSubmittedBy = lens _fSubmittedBy (\s a -> s { _fSubmittedBy = a })
{-# INLINE fSubmittedBy #-}

-- | The time the communication was created.
fTimeCreated :: Lens' Communication (Maybe Text)
fTimeCreated = lens _fTimeCreated (\s a -> s { _fTimeCreated = a })
{-# INLINE fTimeCreated #-}

-- | Information about the attachments to the case communication.
fAttachmentSet :: Lens' Communication ([AttachmentDetails])
fAttachmentSet = lens _fAttachmentSet (\s a -> s { _fAttachmentSet = a })
{-# INLINE fAttachmentSet #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Communication' data type to populate a request.
mkCommunication :: Communication
mkCommunication = Communication
    { _fCaseId = Nothing
    , _fBody = Nothing
    , _fSubmittedBy = Nothing
    , _fTimeCreated = Nothing
    , _fAttachmentSet = mempty
    }
{-# INLINE mkCommunication #-}

instance FromJSON Communication

instance ToJSON Communication

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also includes a
-- nextToken that you can use to retrieve earlier communications.
data RecentCaseCommunications = RecentCaseCommunications
    { _rccCommunications :: [Communication]
      -- ^ The five most recent communications associated with the case.
    , _rccNextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Show, Generic)

-- | The five most recent communications associated with the case.
rccCommunications :: Lens' RecentCaseCommunications ([Communication])
rccCommunications = lens _rccCommunications (\s a -> s { _rccCommunications = a })
{-# INLINE rccCommunications #-}

-- | A resumption point for pagination.
rccNextToken :: Lens' RecentCaseCommunications (Maybe Text)
rccNextToken = lens _rccNextToken (\s a -> s { _rccNextToken = a })
{-# INLINE rccNextToken #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecentCaseCommunications' data type to populate a request.
mkRecentCaseCommunications :: RecentCaseCommunications
mkRecentCaseCommunications = RecentCaseCommunications
    { _rccCommunications = mempty
    , _rccNextToken = Nothing
    }
{-# INLINE mkRecentCaseCommunications #-}

instance FromJSON RecentCaseCommunications

instance ToJSON RecentCaseCommunications

-- | Information about an AWS service returned by the DescribeServices
-- operation.
data Service = Service
    { _vCode :: Maybe Text
      -- ^ The code for an AWS service returned by the DescribeServices
      -- response. The Name element contains the corresponding friendly
      -- name.
    , _vName :: Maybe Text
      -- ^ The friendly name for an AWS service. The Code element contains
      -- the corresponding code.
    , _vCategories :: [Category]
      -- ^ A list of categories that describe the type of support issue a
      -- case describes. Categories consist of a category name and a
      -- category code. Category names and codes are passed to AWS Support
      -- when you call CreateCase.
    } deriving (Show, Generic)

-- | The code for an AWS service returned by the DescribeServices response. The
-- Name element contains the corresponding friendly name.
vCode :: Lens' Service (Maybe Text)
vCode = lens _vCode (\s a -> s { _vCode = a })
{-# INLINE vCode #-}

-- | The friendly name for an AWS service. The Code element contains the
-- corresponding code.
vName :: Lens' Service (Maybe Text)
vName = lens _vName (\s a -> s { _vName = a })
{-# INLINE vName #-}

-- | A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to AWS Support when you call
-- CreateCase.
vCategories :: Lens' Service ([Category])
vCategories = lens _vCategories (\s a -> s { _vCategories = a })
{-# INLINE vCategories #-}

instance FromJSON Service

-- | A code and name pair that represent a severity level that can be applied to
-- a support case.
data SeverityLevel = SeverityLevel
    { _smCode :: Maybe Text
      -- ^ One of four values: "low," "medium," "high," and "urgent". These
      -- values correspond to response times returned to the caller in
      -- SeverityLevel.name.
    , _smName :: Maybe Text
      -- ^ The name of the severity level that corresponds to the severity
      -- level code.
    } deriving (Show, Generic)

-- | One of four values: "low," "medium," "high," and "urgent". These values
-- correspond to response times returned to the caller in SeverityLevel.name.
smCode :: Lens' SeverityLevel (Maybe Text)
smCode = lens _smCode (\s a -> s { _smCode = a })
{-# INLINE smCode #-}

-- | The name of the severity level that corresponds to the severity level code.
smName :: Lens' SeverityLevel (Maybe Text)
smName = lens _smName (\s a -> s { _smName = a })
{-# INLINE smName #-}

instance FromJSON SeverityLevel

-- | The description and metadata for a Trusted Advisor check.
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription
    { _tacdId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , _tacdName :: Text
      -- ^ The display name for the Trusted Advisor check.
    , _tacdDescription :: Text
      -- ^ The description of the Trusted Advisor check, which includes the
      -- alert criteria and recommended actions (contains HTML markup).
    , _tacdCategory :: Text
      -- ^ The category of the Trusted Advisor check.
    , _tacdMetadata :: [Text]
      -- ^ The column headings for the data returned by the Trusted Advisor
      -- check. The order of the headings corresponds to the order of the
      -- data in the Metadata element of the TrustedAdvisorResourceDetail
      -- for the check. Metadata contains all the data that is shown in
      -- the Excel download, even in those cases where the UI shows just
      -- summary data.
    } deriving (Show, Generic)

-- | The unique identifier for the Trusted Advisor check.
tacdId :: Lens' TrustedAdvisorCheckDescription (Text)
tacdId = lens _tacdId (\s a -> s { _tacdId = a })
{-# INLINE tacdId #-}

-- | The display name for the Trusted Advisor check.
tacdName :: Lens' TrustedAdvisorCheckDescription (Text)
tacdName = lens _tacdName (\s a -> s { _tacdName = a })
{-# INLINE tacdName #-}

-- | The description of the Trusted Advisor check, which includes the alert
-- criteria and recommended actions (contains HTML markup).
tacdDescription :: Lens' TrustedAdvisorCheckDescription (Text)
tacdDescription = lens _tacdDescription (\s a -> s { _tacdDescription = a })
{-# INLINE tacdDescription #-}

-- | The category of the Trusted Advisor check.
tacdCategory :: Lens' TrustedAdvisorCheckDescription (Text)
tacdCategory = lens _tacdCategory (\s a -> s { _tacdCategory = a })
{-# INLINE tacdCategory #-}

-- | The column headings for the data returned by the Trusted Advisor check. The
-- order of the headings corresponds to the order of the data in the Metadata
-- element of the TrustedAdvisorResourceDetail for the check. Metadata
-- contains all the data that is shown in the Excel download, even in those
-- cases where the UI shows just summary data.
tacdMetadata :: Lens' TrustedAdvisorCheckDescription ([Text])
tacdMetadata = lens _tacdMetadata (\s a -> s { _tacdMetadata = a })
{-# INLINE tacdMetadata #-}

instance FromJSON TrustedAdvisorCheckDescription

-- | The refresh status of a Trusted Advisor check.
data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus
    { _tacrsCheckId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , _tacrsStatus :: Text
      -- ^ The status of the Trusted Advisor check for which a refresh has
      -- been requested: "none", "enqueued", "processing", "success", or
      -- "abandoned".
    , _tacrsMillisUntilNextRefreshable :: Integer
      -- ^ The amount of time, in milliseconds, until the Trusted Advisor
      -- check is eligible for refresh.
    } deriving (Show, Generic)

-- | The unique identifier for the Trusted Advisor check.
tacrsCheckId :: Lens' TrustedAdvisorCheckRefreshStatus (Text)
tacrsCheckId = lens _tacrsCheckId (\s a -> s { _tacrsCheckId = a })
{-# INLINE tacrsCheckId #-}

-- | The status of the Trusted Advisor check for which a refresh has been
-- requested: "none", "enqueued", "processing", "success", or "abandoned".
tacrsStatus :: Lens' TrustedAdvisorCheckRefreshStatus (Text)
tacrsStatus = lens _tacrsStatus (\s a -> s { _tacrsStatus = a })
{-# INLINE tacrsStatus #-}

-- | The amount of time, in milliseconds, until the Trusted Advisor check is
-- eligible for refresh.
tacrsMillisUntilNextRefreshable :: Lens' TrustedAdvisorCheckRefreshStatus (Integer)
tacrsMillisUntilNextRefreshable = lens _tacrsMillisUntilNextRefreshable (\s a -> s { _tacrsMillisUntilNextRefreshable = a })
{-# INLINE tacrsMillisUntilNextRefreshable #-}

instance FromJSON TrustedAdvisorCheckRefreshStatus

-- | The detailed results of the Trusted Advisor check.
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult
    { _tacrCheckId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , _tacrTimestamp :: Text
      -- ^ The time of the last refresh of the check.
    , _tacrStatus :: Text
      -- ^ The alert status of the check: "ok" (green), "warning" (yellow),
      -- "error" (red), or "not_available".
    , _tacrResourcesSummary :: TrustedAdvisorResourcesSummary
      -- ^ Details about AWS resources that were analyzed in a call to
      -- Trusted Advisor DescribeTrustedAdvisorCheckSummaries.
    , _tacrCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
      -- ^ Summary information that relates to the category of the check.
      -- Cost Optimizing is the only category that is currently supported.
    , _tacrFlaggedResources :: [TrustedAdvisorResourceDetail]
      -- ^ The details about each resource listed in the check result.
    } deriving (Show, Generic)

-- | The unique identifier for the Trusted Advisor check.
tacrCheckId :: Lens' TrustedAdvisorCheckResult (Text)
tacrCheckId = lens _tacrCheckId (\s a -> s { _tacrCheckId = a })
{-# INLINE tacrCheckId #-}

-- | The time of the last refresh of the check.
tacrTimestamp :: Lens' TrustedAdvisorCheckResult (Text)
tacrTimestamp = lens _tacrTimestamp (\s a -> s { _tacrTimestamp = a })
{-# INLINE tacrTimestamp #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacrStatus :: Lens' TrustedAdvisorCheckResult (Text)
tacrStatus = lens _tacrStatus (\s a -> s { _tacrStatus = a })
{-# INLINE tacrStatus #-}

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
tacrResourcesSummary :: Lens' TrustedAdvisorCheckResult (TrustedAdvisorResourcesSummary)
tacrResourcesSummary = lens _tacrResourcesSummary (\s a -> s { _tacrResourcesSummary = a })
{-# INLINE tacrResourcesSummary #-}

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacrCategorySpecificSummary :: Lens' TrustedAdvisorCheckResult (TrustedAdvisorCategorySpecificSummary)
tacrCategorySpecificSummary = lens _tacrCategorySpecificSummary (\s a -> s { _tacrCategorySpecificSummary = a })
{-# INLINE tacrCategorySpecificSummary #-}

-- | The details about each resource listed in the check result.
tacrFlaggedResources :: Lens' TrustedAdvisorCheckResult ([TrustedAdvisorResourceDetail])
tacrFlaggedResources = lens _tacrFlaggedResources (\s a -> s { _tacrFlaggedResources = a })
{-# INLINE tacrFlaggedResources #-}

instance FromJSON TrustedAdvisorCheckResult

-- | A summary of a Trusted Advisor check result, including the alert status,
-- last refresh, and number of resources examined.
data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary
    { _tacsCheckId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , _tacsTimestamp :: Text
      -- ^ The time of the last refresh of the check.
    , _tacsStatus :: Text
      -- ^ The alert status of the check: "ok" (green), "warning" (yellow),
      -- "error" (red), or "not_available".
    , _tacsHasFlaggedResources :: Maybe Bool
      -- ^ Specifies whether the Trusted Advisor check has flagged
      -- resources.
    , _tacsResourcesSummary :: TrustedAdvisorResourcesSummary
      -- ^ Details about AWS resources that were analyzed in a call to
      -- Trusted Advisor DescribeTrustedAdvisorCheckSummaries.
    , _tacsCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
      -- ^ Summary information that relates to the category of the check.
      -- Cost Optimizing is the only category that is currently supported.
    } deriving (Show, Generic)

-- | The unique identifier for the Trusted Advisor check.
tacsCheckId :: Lens' TrustedAdvisorCheckSummary (Text)
tacsCheckId = lens _tacsCheckId (\s a -> s { _tacsCheckId = a })
{-# INLINE tacsCheckId #-}

-- | The time of the last refresh of the check.
tacsTimestamp :: Lens' TrustedAdvisorCheckSummary (Text)
tacsTimestamp = lens _tacsTimestamp (\s a -> s { _tacsTimestamp = a })
{-# INLINE tacsTimestamp #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacsStatus :: Lens' TrustedAdvisorCheckSummary (Text)
tacsStatus = lens _tacsStatus (\s a -> s { _tacsStatus = a })
{-# INLINE tacsStatus #-}

-- | Specifies whether the Trusted Advisor check has flagged resources.
tacsHasFlaggedResources :: Lens' TrustedAdvisorCheckSummary (Maybe Bool)
tacsHasFlaggedResources = lens _tacsHasFlaggedResources (\s a -> s { _tacsHasFlaggedResources = a })
{-# INLINE tacsHasFlaggedResources #-}

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
tacsResourcesSummary :: Lens' TrustedAdvisorCheckSummary (TrustedAdvisorResourcesSummary)
tacsResourcesSummary = lens _tacsResourcesSummary (\s a -> s { _tacsResourcesSummary = a })
{-# INLINE tacsResourcesSummary #-}

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacsCategorySpecificSummary :: Lens' TrustedAdvisorCheckSummary (TrustedAdvisorCategorySpecificSummary)
tacsCategorySpecificSummary = lens _tacsCategorySpecificSummary (\s a -> s { _tacsCategorySpecificSummary = a })
{-# INLINE tacsCategorySpecificSummary #-}

instance FromJSON TrustedAdvisorCheckSummary

-- | The summary information about cost savings for a Trusted Advisor check that
-- is in the Cost Optimizing category.
data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary
    { _tacosEstimatedMonthlySavings :: Double
      -- ^ The estimated monthly savings that might be realized if the
      -- recommended actions are taken.
    , _tacosEstimatedPercentMonthlySavings :: Double
      -- ^ The estimated percentage of savings that might be realized if the
      -- recommended actions are taken.
    } deriving (Show, Generic)

-- | The estimated monthly savings that might be realized if the recommended
-- actions are taken.
tacosEstimatedMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary (Double)
tacosEstimatedMonthlySavings = lens _tacosEstimatedMonthlySavings (\s a -> s { _tacosEstimatedMonthlySavings = a })
{-# INLINE tacosEstimatedMonthlySavings #-}

-- | The estimated percentage of savings that might be realized if the
-- recommended actions are taken.
tacosEstimatedPercentMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary (Double)
tacosEstimatedPercentMonthlySavings = lens _tacosEstimatedPercentMonthlySavings (\s a -> s { _tacosEstimatedPercentMonthlySavings = a })
{-# INLINE tacosEstimatedPercentMonthlySavings #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorCostOptimizingSummary' data type to populate a request.
mkTrustedAdvisorCostOptimizingSummary :: Double -- ^ 'tacosEstimatedMonthlySavings'
                                      -> Double -- ^ 'tacosEstimatedPercentMonthlySavings'
                                      -> TrustedAdvisorCostOptimizingSummary
mkTrustedAdvisorCostOptimizingSummary p1 p2 = TrustedAdvisorCostOptimizingSummary
    { _tacosEstimatedMonthlySavings = p1
    , _tacosEstimatedPercentMonthlySavings = p2
    }
{-# INLINE mkTrustedAdvisorCostOptimizingSummary #-}

instance FromJSON TrustedAdvisorCostOptimizingSummary

instance ToJSON TrustedAdvisorCostOptimizingSummary

-- | Contains information about a resource identified by a Trusted Advisor
-- check.
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail
    { _tardStatus :: Text
      -- ^ The status code for the resource identified in the Trusted
      -- Advisor check.
    , _tardRegion :: Text
      -- ^ The AWS region in which the identified resource is located.
    , _tardResourceId :: Text
      -- ^ The unique identifier for the identified resource.
    , _tardIsSuppressed :: Maybe Bool
      -- ^ Specifies whether the AWS resource was ignored by Trusted Advisor
      -- because it was marked as suppressed by the user.
    , _tardMetadata :: [Text]
      -- ^ Additional information about the identified resource. The exact
      -- metadata and its order can be obtained by inspecting the
      -- TrustedAdvisorCheckDescription object returned by the call to
      -- DescribeTrustedAdvisorChecks. Metadata contains all the data that
      -- is shown in the Excel download, even in those cases where the UI
      -- shows just summary data.
    } deriving (Show, Generic)

-- | The status code for the resource identified in the Trusted Advisor check.
tardStatus :: Lens' TrustedAdvisorResourceDetail (Text)
tardStatus = lens _tardStatus (\s a -> s { _tardStatus = a })
{-# INLINE tardStatus #-}

-- | The AWS region in which the identified resource is located.
tardRegion :: Lens' TrustedAdvisorResourceDetail (Text)
tardRegion = lens _tardRegion (\s a -> s { _tardRegion = a })
{-# INLINE tardRegion #-}

-- | The unique identifier for the identified resource.
tardResourceId :: Lens' TrustedAdvisorResourceDetail (Text)
tardResourceId = lens _tardResourceId (\s a -> s { _tardResourceId = a })
{-# INLINE tardResourceId #-}

-- | Specifies whether the AWS resource was ignored by Trusted Advisor because
-- it was marked as suppressed by the user.
tardIsSuppressed :: Lens' TrustedAdvisorResourceDetail (Maybe Bool)
tardIsSuppressed = lens _tardIsSuppressed (\s a -> s { _tardIsSuppressed = a })
{-# INLINE tardIsSuppressed #-}

-- | Additional information about the identified resource. The exact metadata
-- and its order can be obtained by inspecting the
-- TrustedAdvisorCheckDescription object returned by the call to
-- DescribeTrustedAdvisorChecks. Metadata contains all the data that is shown
-- in the Excel download, even in those cases where the UI shows just summary
-- data.
tardMetadata :: Lens' TrustedAdvisorResourceDetail ([Text])
tardMetadata = lens _tardMetadata (\s a -> s { _tardMetadata = a })
{-# INLINE tardMetadata #-}

instance FromJSON TrustedAdvisorResourceDetail

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary
    { _tarsResourcesProcessed :: Integer
      -- ^ The number of AWS resources that were analyzed by the Trusted
      -- Advisor check.
    , _tarsResourcesFlagged :: Integer
      -- ^ The number of AWS resources that were flagged (listed) by the
      -- Trusted Advisor check.
    , _tarsResourcesIgnored :: Integer
      -- ^ The number of AWS resources ignored by Trusted Advisor because
      -- information was unavailable.
    , _tarsResourcesSuppressed :: Integer
      -- ^ The number of AWS resources ignored by Trusted Advisor because
      -- they were marked as suppressed by the user.
    } deriving (Show, Generic)

-- | The number of AWS resources that were analyzed by the Trusted Advisor
-- check.
tarsResourcesProcessed :: Lens' TrustedAdvisorResourcesSummary (Integer)
tarsResourcesProcessed = lens _tarsResourcesProcessed (\s a -> s { _tarsResourcesProcessed = a })
{-# INLINE tarsResourcesProcessed #-}

-- | The number of AWS resources that were flagged (listed) by the Trusted
-- Advisor check.
tarsResourcesFlagged :: Lens' TrustedAdvisorResourcesSummary (Integer)
tarsResourcesFlagged = lens _tarsResourcesFlagged (\s a -> s { _tarsResourcesFlagged = a })
{-# INLINE tarsResourcesFlagged #-}

-- | The number of AWS resources ignored by Trusted Advisor because information
-- was unavailable.
tarsResourcesIgnored :: Lens' TrustedAdvisorResourcesSummary (Integer)
tarsResourcesIgnored = lens _tarsResourcesIgnored (\s a -> s { _tarsResourcesIgnored = a })
{-# INLINE tarsResourcesIgnored #-}

-- | The number of AWS resources ignored by Trusted Advisor because they were
-- marked as suppressed by the user.
tarsResourcesSuppressed :: Lens' TrustedAdvisorResourcesSummary (Integer)
tarsResourcesSuppressed = lens _tarsResourcesSuppressed (\s a -> s { _tarsResourcesSuppressed = a })
{-# INLINE tarsResourcesSuppressed #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedAdvisorResourcesSummary' data type to populate a request.
mkTrustedAdvisorResourcesSummary :: Integer -- ^ 'tarsResourcesProcessed'
                                 -> Integer -- ^ 'tarsResourcesFlagged'
                                 -> Integer -- ^ 'tarsResourcesIgnored'
                                 -> Integer -- ^ 'tarsResourcesSuppressed'
                                 -> TrustedAdvisorResourcesSummary
mkTrustedAdvisorResourcesSummary p1 p2 p3 p4 = TrustedAdvisorResourcesSummary
    { _tarsResourcesProcessed = p1
    , _tarsResourcesFlagged = p2
    , _tarsResourcesIgnored = p3
    , _tarsResourcesSuppressed = p4
    }
{-# INLINE mkTrustedAdvisorResourcesSummary #-}

instance FromJSON TrustedAdvisorResourcesSummary

instance ToJSON TrustedAdvisorResourcesSummary
