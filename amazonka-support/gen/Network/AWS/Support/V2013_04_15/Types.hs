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
tacssCostOptimizing
    :: Functor f
    => (Maybe TrustedAdvisorCostOptimizingSummary
    -> f (Maybe TrustedAdvisorCostOptimizingSummary))
    -> TrustedAdvisorCategorySpecificSummary
    -> f TrustedAdvisorCategorySpecificSummary
tacssCostOptimizing f x =
    (\y -> x { _tacssCostOptimizing = y })
       <$> f (_tacssCostOptimizing x)
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
bFileName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Attachment
    -> f Attachment
bFileName f x =
    (\y -> x { _bFileName = y })
       <$> f (_bFileName x)
{-# INLINE bFileName #-}

-- | The content of the attachment file.
bData
    :: Functor f
    => (Maybe Base64
    -> f (Maybe Base64))
    -> Attachment
    -> f Attachment
bData f x =
    (\y -> x { _bData = y })
       <$> f (_bData x)
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
adAttachmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AttachmentDetails
    -> f AttachmentDetails
adAttachmentId f x =
    (\y -> x { _adAttachmentId = y })
       <$> f (_adAttachmentId x)
{-# INLINE adAttachmentId #-}

-- | The file name of the attachment.
adFileName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AttachmentDetails
    -> f AttachmentDetails
adFileName f x =
    (\y -> x { _adFileName = y })
       <$> f (_adFileName x)
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
ceCaseId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceCaseId f x =
    (\y -> x { _ceCaseId = y })
       <$> f (_ceCaseId x)
{-# INLINE ceCaseId #-}

-- | The ID displayed for the case in the AWS Support Center. This is a numeric
-- string.
ceDisplayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceDisplayId f x =
    (\y -> x { _ceDisplayId = y })
       <$> f (_ceDisplayId x)
{-# INLINE ceDisplayId #-}

-- | The subject line for the case in the AWS Support Center.
ceSubject
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceSubject f x =
    (\y -> x { _ceSubject = y })
       <$> f (_ceSubject x)
{-# INLINE ceSubject #-}

-- | The status of the case.
ceStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceStatus f x =
    (\y -> x { _ceStatus = y })
       <$> f (_ceStatus x)
{-# INLINE ceStatus #-}

-- | The code for the AWS service returned by the call to DescribeServices.
ceServiceCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceServiceCode f x =
    (\y -> x { _ceServiceCode = y })
       <$> f (_ceServiceCode x)
{-# INLINE ceServiceCode #-}

-- | The category of problem for the AWS Support case.
ceCategoryCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceCategoryCode f x =
    (\y -> x { _ceCategoryCode = y })
       <$> f (_ceCategoryCode x)
{-# INLINE ceCategoryCode #-}

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
ceSeverityCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceSeverityCode f x =
    (\y -> x { _ceSeverityCode = y })
       <$> f (_ceSeverityCode x)
{-# INLINE ceSeverityCode #-}

-- | The email address of the account that submitted the case.
ceSubmittedBy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceSubmittedBy f x =
    (\y -> x { _ceSubmittedBy = y })
       <$> f (_ceSubmittedBy x)
{-# INLINE ceSubmittedBy #-}

-- | The time that the case was case created in the AWS Support Center.
ceTimeCreated
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceTimeCreated f x =
    (\y -> x { _ceTimeCreated = y })
       <$> f (_ceTimeCreated x)
{-# INLINE ceTimeCreated #-}

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also includes a
-- nextToken that you can use to retrieve earlier communications.
ceRecentCommunications
    :: Functor f
    => (Maybe RecentCaseCommunications
    -> f (Maybe RecentCaseCommunications))
    -> CaseDetails
    -> f CaseDetails
ceRecentCommunications f x =
    (\y -> x { _ceRecentCommunications = y })
       <$> f (_ceRecentCommunications x)
{-# INLINE ceRecentCommunications #-}

-- | The email addresses that receive copies of communication about the case.
ceCcEmailAddresses
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CaseDetails
    -> f CaseDetails
ceCcEmailAddresses f x =
    (\y -> x { _ceCcEmailAddresses = y })
       <$> f (_ceCcEmailAddresses x)
{-# INLINE ceCcEmailAddresses #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
ceLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaseDetails
    -> f CaseDetails
ceLanguage f x =
    (\y -> x { _ceLanguage = y })
       <$> f (_ceLanguage x)
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
hCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Category
    -> f Category
hCode f x =
    (\y -> x { _hCode = y })
       <$> f (_hCode x)
{-# INLINE hCode #-}

-- | The category name for the support case.
hName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Category
    -> f Category
hName f x =
    (\y -> x { _hName = y })
       <$> f (_hName x)
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
fCaseId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Communication
    -> f Communication
fCaseId f x =
    (\y -> x { _fCaseId = y })
       <$> f (_fCaseId x)
{-# INLINE fCaseId #-}

-- | The text of the communication between the customer and AWS Support.
fBody
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Communication
    -> f Communication
fBody f x =
    (\y -> x { _fBody = y })
       <$> f (_fBody x)
{-# INLINE fBody #-}

-- | The email address of the account that submitted the AWS Support case.
fSubmittedBy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Communication
    -> f Communication
fSubmittedBy f x =
    (\y -> x { _fSubmittedBy = y })
       <$> f (_fSubmittedBy x)
{-# INLINE fSubmittedBy #-}

-- | The time the communication was created.
fTimeCreated
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Communication
    -> f Communication
fTimeCreated f x =
    (\y -> x { _fTimeCreated = y })
       <$> f (_fTimeCreated x)
{-# INLINE fTimeCreated #-}

-- | Information about the attachments to the case communication.
fAttachmentSet
    :: Functor f
    => ([AttachmentDetails]
    -> f ([AttachmentDetails]))
    -> Communication
    -> f Communication
fAttachmentSet f x =
    (\y -> x { _fAttachmentSet = y })
       <$> f (_fAttachmentSet x)
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
rccCommunications
    :: Functor f
    => ([Communication]
    -> f ([Communication]))
    -> RecentCaseCommunications
    -> f RecentCaseCommunications
rccCommunications f x =
    (\y -> x { _rccCommunications = y })
       <$> f (_rccCommunications x)
{-# INLINE rccCommunications #-}

-- | A resumption point for pagination.
rccNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RecentCaseCommunications
    -> f RecentCaseCommunications
rccNextToken f x =
    (\y -> x { _rccNextToken = y })
       <$> f (_rccNextToken x)
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
vCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Service
    -> f Service
vCode f x =
    (\y -> x { _vCode = y })
       <$> f (_vCode x)
{-# INLINE vCode #-}

-- | The friendly name for an AWS service. The Code element contains the
-- corresponding code.
vName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Service
    -> f Service
vName f x =
    (\y -> x { _vName = y })
       <$> f (_vName x)
{-# INLINE vName #-}

-- | A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to AWS Support when you call
-- CreateCase.
vCategories
    :: Functor f
    => ([Category]
    -> f ([Category]))
    -> Service
    -> f Service
vCategories f x =
    (\y -> x { _vCategories = y })
       <$> f (_vCategories x)
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
smCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SeverityLevel
    -> f SeverityLevel
smCode f x =
    (\y -> x { _smCode = y })
       <$> f (_smCode x)
{-# INLINE smCode #-}

-- | The name of the severity level that corresponds to the severity level code.
smName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SeverityLevel
    -> f SeverityLevel
smName f x =
    (\y -> x { _smName = y })
       <$> f (_smName x)
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
tacdId
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckDescription
    -> f TrustedAdvisorCheckDescription
tacdId f x =
    (\y -> x { _tacdId = y })
       <$> f (_tacdId x)
{-# INLINE tacdId #-}

-- | The display name for the Trusted Advisor check.
tacdName
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckDescription
    -> f TrustedAdvisorCheckDescription
tacdName f x =
    (\y -> x { _tacdName = y })
       <$> f (_tacdName x)
{-# INLINE tacdName #-}

-- | The description of the Trusted Advisor check, which includes the alert
-- criteria and recommended actions (contains HTML markup).
tacdDescription
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckDescription
    -> f TrustedAdvisorCheckDescription
tacdDescription f x =
    (\y -> x { _tacdDescription = y })
       <$> f (_tacdDescription x)
{-# INLINE tacdDescription #-}

-- | The category of the Trusted Advisor check.
tacdCategory
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckDescription
    -> f TrustedAdvisorCheckDescription
tacdCategory f x =
    (\y -> x { _tacdCategory = y })
       <$> f (_tacdCategory x)
{-# INLINE tacdCategory #-}

-- | The column headings for the data returned by the Trusted Advisor check. The
-- order of the headings corresponds to the order of the data in the Metadata
-- element of the TrustedAdvisorResourceDetail for the check. Metadata
-- contains all the data that is shown in the Excel download, even in those
-- cases where the UI shows just summary data.
tacdMetadata
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> TrustedAdvisorCheckDescription
    -> f TrustedAdvisorCheckDescription
tacdMetadata f x =
    (\y -> x { _tacdMetadata = y })
       <$> f (_tacdMetadata x)
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
tacrsCheckId
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckRefreshStatus
    -> f TrustedAdvisorCheckRefreshStatus
tacrsCheckId f x =
    (\y -> x { _tacrsCheckId = y })
       <$> f (_tacrsCheckId x)
{-# INLINE tacrsCheckId #-}

-- | The status of the Trusted Advisor check for which a refresh has been
-- requested: "none", "enqueued", "processing", "success", or "abandoned".
tacrsStatus
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckRefreshStatus
    -> f TrustedAdvisorCheckRefreshStatus
tacrsStatus f x =
    (\y -> x { _tacrsStatus = y })
       <$> f (_tacrsStatus x)
{-# INLINE tacrsStatus #-}

-- | The amount of time, in milliseconds, until the Trusted Advisor check is
-- eligible for refresh.
tacrsMillisUntilNextRefreshable
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TrustedAdvisorCheckRefreshStatus
    -> f TrustedAdvisorCheckRefreshStatus
tacrsMillisUntilNextRefreshable f x =
    (\y -> x { _tacrsMillisUntilNextRefreshable = y })
       <$> f (_tacrsMillisUntilNextRefreshable x)
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
tacrCheckId
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckResult
    -> f TrustedAdvisorCheckResult
tacrCheckId f x =
    (\y -> x { _tacrCheckId = y })
       <$> f (_tacrCheckId x)
{-# INLINE tacrCheckId #-}

-- | The time of the last refresh of the check.
tacrTimestamp
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckResult
    -> f TrustedAdvisorCheckResult
tacrTimestamp f x =
    (\y -> x { _tacrTimestamp = y })
       <$> f (_tacrTimestamp x)
{-# INLINE tacrTimestamp #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacrStatus
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckResult
    -> f TrustedAdvisorCheckResult
tacrStatus f x =
    (\y -> x { _tacrStatus = y })
       <$> f (_tacrStatus x)
{-# INLINE tacrStatus #-}

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
tacrResourcesSummary
    :: Functor f
    => (TrustedAdvisorResourcesSummary
    -> f (TrustedAdvisorResourcesSummary))
    -> TrustedAdvisorCheckResult
    -> f TrustedAdvisorCheckResult
tacrResourcesSummary f x =
    (\y -> x { _tacrResourcesSummary = y })
       <$> f (_tacrResourcesSummary x)
{-# INLINE tacrResourcesSummary #-}

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacrCategorySpecificSummary
    :: Functor f
    => (TrustedAdvisorCategorySpecificSummary
    -> f (TrustedAdvisorCategorySpecificSummary))
    -> TrustedAdvisorCheckResult
    -> f TrustedAdvisorCheckResult
tacrCategorySpecificSummary f x =
    (\y -> x { _tacrCategorySpecificSummary = y })
       <$> f (_tacrCategorySpecificSummary x)
{-# INLINE tacrCategorySpecificSummary #-}

-- | The details about each resource listed in the check result.
tacrFlaggedResources
    :: Functor f
    => ([TrustedAdvisorResourceDetail]
    -> f ([TrustedAdvisorResourceDetail]))
    -> TrustedAdvisorCheckResult
    -> f TrustedAdvisorCheckResult
tacrFlaggedResources f x =
    (\y -> x { _tacrFlaggedResources = y })
       <$> f (_tacrFlaggedResources x)
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
tacsCheckId
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckSummary
    -> f TrustedAdvisorCheckSummary
tacsCheckId f x =
    (\y -> x { _tacsCheckId = y })
       <$> f (_tacsCheckId x)
{-# INLINE tacsCheckId #-}

-- | The time of the last refresh of the check.
tacsTimestamp
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckSummary
    -> f TrustedAdvisorCheckSummary
tacsTimestamp f x =
    (\y -> x { _tacsTimestamp = y })
       <$> f (_tacsTimestamp x)
{-# INLINE tacsTimestamp #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacsStatus
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorCheckSummary
    -> f TrustedAdvisorCheckSummary
tacsStatus f x =
    (\y -> x { _tacsStatus = y })
       <$> f (_tacsStatus x)
{-# INLINE tacsStatus #-}

-- | Specifies whether the Trusted Advisor check has flagged resources.
tacsHasFlaggedResources
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TrustedAdvisorCheckSummary
    -> f TrustedAdvisorCheckSummary
tacsHasFlaggedResources f x =
    (\y -> x { _tacsHasFlaggedResources = y })
       <$> f (_tacsHasFlaggedResources x)
{-# INLINE tacsHasFlaggedResources #-}

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
tacsResourcesSummary
    :: Functor f
    => (TrustedAdvisorResourcesSummary
    -> f (TrustedAdvisorResourcesSummary))
    -> TrustedAdvisorCheckSummary
    -> f TrustedAdvisorCheckSummary
tacsResourcesSummary f x =
    (\y -> x { _tacsResourcesSummary = y })
       <$> f (_tacsResourcesSummary x)
{-# INLINE tacsResourcesSummary #-}

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacsCategorySpecificSummary
    :: Functor f
    => (TrustedAdvisorCategorySpecificSummary
    -> f (TrustedAdvisorCategorySpecificSummary))
    -> TrustedAdvisorCheckSummary
    -> f TrustedAdvisorCheckSummary
tacsCategorySpecificSummary f x =
    (\y -> x { _tacsCategorySpecificSummary = y })
       <$> f (_tacsCategorySpecificSummary x)
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
tacosEstimatedMonthlySavings
    :: Functor f
    => (Double
    -> f (Double))
    -> TrustedAdvisorCostOptimizingSummary
    -> f TrustedAdvisorCostOptimizingSummary
tacosEstimatedMonthlySavings f x =
    (\y -> x { _tacosEstimatedMonthlySavings = y })
       <$> f (_tacosEstimatedMonthlySavings x)
{-# INLINE tacosEstimatedMonthlySavings #-}

-- | The estimated percentage of savings that might be realized if the
-- recommended actions are taken.
tacosEstimatedPercentMonthlySavings
    :: Functor f
    => (Double
    -> f (Double))
    -> TrustedAdvisorCostOptimizingSummary
    -> f TrustedAdvisorCostOptimizingSummary
tacosEstimatedPercentMonthlySavings f x =
    (\y -> x { _tacosEstimatedPercentMonthlySavings = y })
       <$> f (_tacosEstimatedPercentMonthlySavings x)
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
tardStatus
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorResourceDetail
    -> f TrustedAdvisorResourceDetail
tardStatus f x =
    (\y -> x { _tardStatus = y })
       <$> f (_tardStatus x)
{-# INLINE tardStatus #-}

-- | The AWS region in which the identified resource is located.
tardRegion
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorResourceDetail
    -> f TrustedAdvisorResourceDetail
tardRegion f x =
    (\y -> x { _tardRegion = y })
       <$> f (_tardRegion x)
{-# INLINE tardRegion #-}

-- | The unique identifier for the identified resource.
tardResourceId
    :: Functor f
    => (Text
    -> f (Text))
    -> TrustedAdvisorResourceDetail
    -> f TrustedAdvisorResourceDetail
tardResourceId f x =
    (\y -> x { _tardResourceId = y })
       <$> f (_tardResourceId x)
{-# INLINE tardResourceId #-}

-- | Specifies whether the AWS resource was ignored by Trusted Advisor because
-- it was marked as suppressed by the user.
tardIsSuppressed
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TrustedAdvisorResourceDetail
    -> f TrustedAdvisorResourceDetail
tardIsSuppressed f x =
    (\y -> x { _tardIsSuppressed = y })
       <$> f (_tardIsSuppressed x)
{-# INLINE tardIsSuppressed #-}

-- | Additional information about the identified resource. The exact metadata
-- and its order can be obtained by inspecting the
-- TrustedAdvisorCheckDescription object returned by the call to
-- DescribeTrustedAdvisorChecks. Metadata contains all the data that is shown
-- in the Excel download, even in those cases where the UI shows just summary
-- data.
tardMetadata
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> TrustedAdvisorResourceDetail
    -> f TrustedAdvisorResourceDetail
tardMetadata f x =
    (\y -> x { _tardMetadata = y })
       <$> f (_tardMetadata x)
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
tarsResourcesProcessed
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TrustedAdvisorResourcesSummary
    -> f TrustedAdvisorResourcesSummary
tarsResourcesProcessed f x =
    (\y -> x { _tarsResourcesProcessed = y })
       <$> f (_tarsResourcesProcessed x)
{-# INLINE tarsResourcesProcessed #-}

-- | The number of AWS resources that were flagged (listed) by the Trusted
-- Advisor check.
tarsResourcesFlagged
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TrustedAdvisorResourcesSummary
    -> f TrustedAdvisorResourcesSummary
tarsResourcesFlagged f x =
    (\y -> x { _tarsResourcesFlagged = y })
       <$> f (_tarsResourcesFlagged x)
{-# INLINE tarsResourcesFlagged #-}

-- | The number of AWS resources ignored by Trusted Advisor because information
-- was unavailable.
tarsResourcesIgnored
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TrustedAdvisorResourcesSummary
    -> f TrustedAdvisorResourcesSummary
tarsResourcesIgnored f x =
    (\y -> x { _tarsResourcesIgnored = y })
       <$> f (_tarsResourcesIgnored x)
{-# INLINE tarsResourcesIgnored #-}

-- | The number of AWS resources ignored by Trusted Advisor because they were
-- marked as suppressed by the user.
tarsResourcesSuppressed
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TrustedAdvisorResourcesSummary
    -> f TrustedAdvisorResourcesSummary
tarsResourcesSuppressed f x =
    (\y -> x { _tarsResourcesSuppressed = y })
       <$> f (_tarsResourcesSuppressed x)
{-# INLINE tarsResourcesSuppressed #-}

instance FromJSON TrustedAdvisorResourcesSummary

instance ToJSON TrustedAdvisorResourcesSummary
