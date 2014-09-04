{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , TrustedAdvisorCategorySpecificSummary (..)
    , tacssCostOptimizing

    -- * Attachment
    , Attachment (..)
    , bFileName
    , bData

    -- * AttachmentDetails
    , AttachmentDetails (..)
    , adAttachmentId
    , adFileName

    -- * CaseDetails
    , CaseDetails (..)
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
    , Category (..)
    , hCode
    , hName

    -- * Communication
    , Communication (..)
    , fCaseId
    , fBody
    , fSubmittedBy
    , fTimeCreated
    , fAttachmentSet

    -- * RecentCaseCommunications
    , RecentCaseCommunications (..)
    , rccCommunications
    , rccNextToken

    -- * Service
    , Service (..)
    , vCode
    , vName
    , vCategories

    -- * SeverityLevel
    , SeverityLevel (..)
    , smCode
    , smName

    -- * TrustedAdvisorCheckDescription
    , TrustedAdvisorCheckDescription (..)
    , tacdId
    , tacdName
    , tacdDescription
    , tacdCategory
    , tacdMetadata

    -- * TrustedAdvisorCheckRefreshStatus
    , TrustedAdvisorCheckRefreshStatus (..)
    , tacrsCheckId
    , tacrsStatus
    , tacrsMillisUntilNextRefreshable

    -- * TrustedAdvisorCheckResult
    , TrustedAdvisorCheckResult (..)
    , tacrCheckId
    , tacrTimestamp
    , tacrStatus
    , tacrResourcesSummary
    , tacrCategorySpecificSummary
    , tacrFlaggedResources

    -- * TrustedAdvisorCheckSummary
    , TrustedAdvisorCheckSummary (..)
    , tacsCheckId
    , tacsTimestamp
    , tacsStatus
    , tacsHasFlaggedResources
    , tacsResourcesSummary
    , tacsCategorySpecificSummary

    -- * TrustedAdvisorCostOptimizingSummary
    , TrustedAdvisorCostOptimizingSummary (..)
    , tacosEstimatedMonthlySavings
    , tacosEstimatedPercentMonthlySavings

    -- * TrustedAdvisorResourceDetail
    , TrustedAdvisorResourceDetail (..)
    , tardStatus
    , tardRegion
    , tardResourceId
    , tardIsSuppressed
    , tardMetadata

    -- * TrustedAdvisorResourcesSummary
    , TrustedAdvisorResourcesSummary (..)
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
tacssCostOptimizing f x =
    f (_tacssCostOptimizing x)
        <&> \y -> x { _tacssCostOptimizing = y }
{-# INLINE tacssCostOptimizing #-}

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
bFileName f x =
    f (_bFileName x)
        <&> \y -> x { _bFileName = y }
{-# INLINE bFileName #-}

-- | The content of the attachment file.
bData :: Lens' Attachment (Maybe Base64)
bData f x =
    f (_bData x)
        <&> \y -> x { _bData = y }
{-# INLINE bData #-}

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
adAttachmentId f x =
    f (_adAttachmentId x)
        <&> \y -> x { _adAttachmentId = y }
{-# INLINE adAttachmentId #-}

-- | The file name of the attachment.
adFileName :: Lens' AttachmentDetails (Maybe Text)
adFileName f x =
    f (_adFileName x)
        <&> \y -> x { _adFileName = y }
{-# INLINE adFileName #-}

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
ceCaseId f x =
    f (_ceCaseId x)
        <&> \y -> x { _ceCaseId = y }
{-# INLINE ceCaseId #-}

-- | The ID displayed for the case in the AWS Support Center. This is a numeric
-- string.
ceDisplayId :: Lens' CaseDetails (Maybe Text)
ceDisplayId f x =
    f (_ceDisplayId x)
        <&> \y -> x { _ceDisplayId = y }
{-# INLINE ceDisplayId #-}

-- | The subject line for the case in the AWS Support Center.
ceSubject :: Lens' CaseDetails (Maybe Text)
ceSubject f x =
    f (_ceSubject x)
        <&> \y -> x { _ceSubject = y }
{-# INLINE ceSubject #-}

-- | The status of the case.
ceStatus :: Lens' CaseDetails (Maybe Text)
ceStatus f x =
    f (_ceStatus x)
        <&> \y -> x { _ceStatus = y }
{-# INLINE ceStatus #-}

-- | The code for the AWS service returned by the call to DescribeServices.
ceServiceCode :: Lens' CaseDetails (Maybe Text)
ceServiceCode f x =
    f (_ceServiceCode x)
        <&> \y -> x { _ceServiceCode = y }
{-# INLINE ceServiceCode #-}

-- | The category of problem for the AWS Support case.
ceCategoryCode :: Lens' CaseDetails (Maybe Text)
ceCategoryCode f x =
    f (_ceCategoryCode x)
        <&> \y -> x { _ceCategoryCode = y }
{-# INLINE ceCategoryCode #-}

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
ceSeverityCode :: Lens' CaseDetails (Maybe Text)
ceSeverityCode f x =
    f (_ceSeverityCode x)
        <&> \y -> x { _ceSeverityCode = y }
{-# INLINE ceSeverityCode #-}

-- | The email address of the account that submitted the case.
ceSubmittedBy :: Lens' CaseDetails (Maybe Text)
ceSubmittedBy f x =
    f (_ceSubmittedBy x)
        <&> \y -> x { _ceSubmittedBy = y }
{-# INLINE ceSubmittedBy #-}

-- | The time that the case was case created in the AWS Support Center.
ceTimeCreated :: Lens' CaseDetails (Maybe Text)
ceTimeCreated f x =
    f (_ceTimeCreated x)
        <&> \y -> x { _ceTimeCreated = y }
{-# INLINE ceTimeCreated #-}

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also includes a
-- nextToken that you can use to retrieve earlier communications.
ceRecentCommunications :: Lens' CaseDetails (Maybe RecentCaseCommunications)
ceRecentCommunications f x =
    f (_ceRecentCommunications x)
        <&> \y -> x { _ceRecentCommunications = y }
{-# INLINE ceRecentCommunications #-}

-- | The email addresses that receive copies of communication about the case.
ceCcEmailAddresses :: Lens' CaseDetails ([Text])
ceCcEmailAddresses f x =
    f (_ceCcEmailAddresses x)
        <&> \y -> x { _ceCcEmailAddresses = y }
{-# INLINE ceCcEmailAddresses #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
ceLanguage :: Lens' CaseDetails (Maybe Text)
ceLanguage f x =
    f (_ceLanguage x)
        <&> \y -> x { _ceLanguage = y }
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
hCode f x =
    f (_hCode x)
        <&> \y -> x { _hCode = y }
{-# INLINE hCode #-}

-- | The category name for the support case.
hName :: Lens' Category (Maybe Text)
hName f x =
    f (_hName x)
        <&> \y -> x { _hName = y }
{-# INLINE hName #-}

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
fCaseId f x =
    f (_fCaseId x)
        <&> \y -> x { _fCaseId = y }
{-# INLINE fCaseId #-}

-- | The text of the communication between the customer and AWS Support.
fBody :: Lens' Communication (Maybe Text)
fBody f x =
    f (_fBody x)
        <&> \y -> x { _fBody = y }
{-# INLINE fBody #-}

-- | The email address of the account that submitted the AWS Support case.
fSubmittedBy :: Lens' Communication (Maybe Text)
fSubmittedBy f x =
    f (_fSubmittedBy x)
        <&> \y -> x { _fSubmittedBy = y }
{-# INLINE fSubmittedBy #-}

-- | The time the communication was created.
fTimeCreated :: Lens' Communication (Maybe Text)
fTimeCreated f x =
    f (_fTimeCreated x)
        <&> \y -> x { _fTimeCreated = y }
{-# INLINE fTimeCreated #-}

-- | Information about the attachments to the case communication.
fAttachmentSet :: Lens' Communication ([AttachmentDetails])
fAttachmentSet f x =
    f (_fAttachmentSet x)
        <&> \y -> x { _fAttachmentSet = y }
{-# INLINE fAttachmentSet #-}

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
rccCommunications f x =
    f (_rccCommunications x)
        <&> \y -> x { _rccCommunications = y }
{-# INLINE rccCommunications #-}

-- | A resumption point for pagination.
rccNextToken :: Lens' RecentCaseCommunications (Maybe Text)
rccNextToken f x =
    f (_rccNextToken x)
        <&> \y -> x { _rccNextToken = y }
{-# INLINE rccNextToken #-}

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
vCode f x =
    f (_vCode x)
        <&> \y -> x { _vCode = y }
{-# INLINE vCode #-}

-- | The friendly name for an AWS service. The Code element contains the
-- corresponding code.
vName :: Lens' Service (Maybe Text)
vName f x =
    f (_vName x)
        <&> \y -> x { _vName = y }
{-# INLINE vName #-}

-- | A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to AWS Support when you call
-- CreateCase.
vCategories :: Lens' Service ([Category])
vCategories f x =
    f (_vCategories x)
        <&> \y -> x { _vCategories = y }
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
smCode f x =
    f (_smCode x)
        <&> \y -> x { _smCode = y }
{-# INLINE smCode #-}

-- | The name of the severity level that corresponds to the severity level code.
smName :: Lens' SeverityLevel (Maybe Text)
smName f x =
    f (_smName x)
        <&> \y -> x { _smName = y }
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
tacdId f x =
    f (_tacdId x)
        <&> \y -> x { _tacdId = y }
{-# INLINE tacdId #-}

-- | The display name for the Trusted Advisor check.
tacdName :: Lens' TrustedAdvisorCheckDescription (Text)
tacdName f x =
    f (_tacdName x)
        <&> \y -> x { _tacdName = y }
{-# INLINE tacdName #-}

-- | The description of the Trusted Advisor check, which includes the alert
-- criteria and recommended actions (contains HTML markup).
tacdDescription :: Lens' TrustedAdvisorCheckDescription (Text)
tacdDescription f x =
    f (_tacdDescription x)
        <&> \y -> x { _tacdDescription = y }
{-# INLINE tacdDescription #-}

-- | The category of the Trusted Advisor check.
tacdCategory :: Lens' TrustedAdvisorCheckDescription (Text)
tacdCategory f x =
    f (_tacdCategory x)
        <&> \y -> x { _tacdCategory = y }
{-# INLINE tacdCategory #-}

-- | The column headings for the data returned by the Trusted Advisor check. The
-- order of the headings corresponds to the order of the data in the Metadata
-- element of the TrustedAdvisorResourceDetail for the check. Metadata
-- contains all the data that is shown in the Excel download, even in those
-- cases where the UI shows just summary data.
tacdMetadata :: Lens' TrustedAdvisorCheckDescription ([Text])
tacdMetadata f x =
    f (_tacdMetadata x)
        <&> \y -> x { _tacdMetadata = y }
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
tacrsCheckId f x =
    f (_tacrsCheckId x)
        <&> \y -> x { _tacrsCheckId = y }
{-# INLINE tacrsCheckId #-}

-- | The status of the Trusted Advisor check for which a refresh has been
-- requested: "none", "enqueued", "processing", "success", or "abandoned".
tacrsStatus :: Lens' TrustedAdvisorCheckRefreshStatus (Text)
tacrsStatus f x =
    f (_tacrsStatus x)
        <&> \y -> x { _tacrsStatus = y }
{-# INLINE tacrsStatus #-}

-- | The amount of time, in milliseconds, until the Trusted Advisor check is
-- eligible for refresh.
tacrsMillisUntilNextRefreshable :: Lens' TrustedAdvisorCheckRefreshStatus (Integer)
tacrsMillisUntilNextRefreshable f x =
    f (_tacrsMillisUntilNextRefreshable x)
        <&> \y -> x { _tacrsMillisUntilNextRefreshable = y }
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
tacrCheckId f x =
    f (_tacrCheckId x)
        <&> \y -> x { _tacrCheckId = y }
{-# INLINE tacrCheckId #-}

-- | The time of the last refresh of the check.
tacrTimestamp :: Lens' TrustedAdvisorCheckResult (Text)
tacrTimestamp f x =
    f (_tacrTimestamp x)
        <&> \y -> x { _tacrTimestamp = y }
{-# INLINE tacrTimestamp #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacrStatus :: Lens' TrustedAdvisorCheckResult (Text)
tacrStatus f x =
    f (_tacrStatus x)
        <&> \y -> x { _tacrStatus = y }
{-# INLINE tacrStatus #-}

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
tacrResourcesSummary :: Lens' TrustedAdvisorCheckResult (TrustedAdvisorResourcesSummary)
tacrResourcesSummary f x =
    f (_tacrResourcesSummary x)
        <&> \y -> x { _tacrResourcesSummary = y }
{-# INLINE tacrResourcesSummary #-}

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacrCategorySpecificSummary :: Lens' TrustedAdvisorCheckResult (TrustedAdvisorCategorySpecificSummary)
tacrCategorySpecificSummary f x =
    f (_tacrCategorySpecificSummary x)
        <&> \y -> x { _tacrCategorySpecificSummary = y }
{-# INLINE tacrCategorySpecificSummary #-}

-- | The details about each resource listed in the check result.
tacrFlaggedResources :: Lens' TrustedAdvisorCheckResult ([TrustedAdvisorResourceDetail])
tacrFlaggedResources f x =
    f (_tacrFlaggedResources x)
        <&> \y -> x { _tacrFlaggedResources = y }
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
tacsCheckId f x =
    f (_tacsCheckId x)
        <&> \y -> x { _tacsCheckId = y }
{-# INLINE tacsCheckId #-}

-- | The time of the last refresh of the check.
tacsTimestamp :: Lens' TrustedAdvisorCheckSummary (Text)
tacsTimestamp f x =
    f (_tacsTimestamp x)
        <&> \y -> x { _tacsTimestamp = y }
{-# INLINE tacsTimestamp #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacsStatus :: Lens' TrustedAdvisorCheckSummary (Text)
tacsStatus f x =
    f (_tacsStatus x)
        <&> \y -> x { _tacsStatus = y }
{-# INLINE tacsStatus #-}

-- | Specifies whether the Trusted Advisor check has flagged resources.
tacsHasFlaggedResources :: Lens' TrustedAdvisorCheckSummary (Maybe Bool)
tacsHasFlaggedResources f x =
    f (_tacsHasFlaggedResources x)
        <&> \y -> x { _tacsHasFlaggedResources = y }
{-# INLINE tacsHasFlaggedResources #-}

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
tacsResourcesSummary :: Lens' TrustedAdvisorCheckSummary (TrustedAdvisorResourcesSummary)
tacsResourcesSummary f x =
    f (_tacsResourcesSummary x)
        <&> \y -> x { _tacsResourcesSummary = y }
{-# INLINE tacsResourcesSummary #-}

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacsCategorySpecificSummary :: Lens' TrustedAdvisorCheckSummary (TrustedAdvisorCategorySpecificSummary)
tacsCategorySpecificSummary f x =
    f (_tacsCategorySpecificSummary x)
        <&> \y -> x { _tacsCategorySpecificSummary = y }
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
tacosEstimatedMonthlySavings f x =
    f (_tacosEstimatedMonthlySavings x)
        <&> \y -> x { _tacosEstimatedMonthlySavings = y }
{-# INLINE tacosEstimatedMonthlySavings #-}

-- | The estimated percentage of savings that might be realized if the
-- recommended actions are taken.
tacosEstimatedPercentMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary (Double)
tacosEstimatedPercentMonthlySavings f x =
    f (_tacosEstimatedPercentMonthlySavings x)
        <&> \y -> x { _tacosEstimatedPercentMonthlySavings = y }
{-# INLINE tacosEstimatedPercentMonthlySavings #-}

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
tardStatus f x =
    f (_tardStatus x)
        <&> \y -> x { _tardStatus = y }
{-# INLINE tardStatus #-}

-- | The AWS region in which the identified resource is located.
tardRegion :: Lens' TrustedAdvisorResourceDetail (Text)
tardRegion f x =
    f (_tardRegion x)
        <&> \y -> x { _tardRegion = y }
{-# INLINE tardRegion #-}

-- | The unique identifier for the identified resource.
tardResourceId :: Lens' TrustedAdvisorResourceDetail (Text)
tardResourceId f x =
    f (_tardResourceId x)
        <&> \y -> x { _tardResourceId = y }
{-# INLINE tardResourceId #-}

-- | Specifies whether the AWS resource was ignored by Trusted Advisor because
-- it was marked as suppressed by the user.
tardIsSuppressed :: Lens' TrustedAdvisorResourceDetail (Maybe Bool)
tardIsSuppressed f x =
    f (_tardIsSuppressed x)
        <&> \y -> x { _tardIsSuppressed = y }
{-# INLINE tardIsSuppressed #-}

-- | Additional information about the identified resource. The exact metadata
-- and its order can be obtained by inspecting the
-- TrustedAdvisorCheckDescription object returned by the call to
-- DescribeTrustedAdvisorChecks. Metadata contains all the data that is shown
-- in the Excel download, even in those cases where the UI shows just summary
-- data.
tardMetadata :: Lens' TrustedAdvisorResourceDetail ([Text])
tardMetadata f x =
    f (_tardMetadata x)
        <&> \y -> x { _tardMetadata = y }
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
tarsResourcesProcessed f x =
    f (_tarsResourcesProcessed x)
        <&> \y -> x { _tarsResourcesProcessed = y }
{-# INLINE tarsResourcesProcessed #-}

-- | The number of AWS resources that were flagged (listed) by the Trusted
-- Advisor check.
tarsResourcesFlagged :: Lens' TrustedAdvisorResourcesSummary (Integer)
tarsResourcesFlagged f x =
    f (_tarsResourcesFlagged x)
        <&> \y -> x { _tarsResourcesFlagged = y }
{-# INLINE tarsResourcesFlagged #-}

-- | The number of AWS resources ignored by Trusted Advisor because information
-- was unavailable.
tarsResourcesIgnored :: Lens' TrustedAdvisorResourcesSummary (Integer)
tarsResourcesIgnored f x =
    f (_tarsResourcesIgnored x)
        <&> \y -> x { _tarsResourcesIgnored = y }
{-# INLINE tarsResourcesIgnored #-}

-- | The number of AWS resources ignored by Trusted Advisor because they were
-- marked as suppressed by the user.
tarsResourcesSuppressed :: Lens' TrustedAdvisorResourcesSummary (Integer)
tarsResourcesSuppressed f x =
    f (_tarsResourcesSuppressed x)
        <&> \y -> x { _tarsResourcesSuppressed = y }
{-# INLINE tarsResourcesSuppressed #-}

instance FromJSON TrustedAdvisorResourcesSummary

instance ToJSON TrustedAdvisorResourcesSummary
