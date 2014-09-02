{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    ( module Network.AWS.Support.V2013_04_15.Types
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

instance FromJSON TrustedAdvisorCategorySpecificSummary

instance ToJSON TrustedAdvisorCategorySpecificSummary

-- | The attachment content and file name.
data Attachment = Attachment
    { _aData :: Maybe Base64
      -- ^ The content of the attachment file.
    , _aFileName :: Maybe Text
      -- ^ The name of the attachment file.
    } deriving (Show, Generic)

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
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    , _cdCategoryCode :: Maybe Text
      -- ^ The category of problem for the AWS Support case.
    , _cdCcEmailAddresses :: [Text]
      -- ^ The email addresses that receive copies of communication about
      -- the case.
    , _cdDisplayId :: Maybe Text
      -- ^ The ID displayed for the case in the AWS Support Center. This is
      -- a numeric string.
    , _cdLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides
      -- support. AWS Support currently supports English ("en") and
      -- Japanese ("ja"). Language parameters must be passed explicitly
      -- for operations that take them.
    , _cdRecentCommunications :: Maybe RecentCaseCommunications
      -- ^ The five most recent communications between you and AWS Support
      -- Center, including the IDs of any attachments to the
      -- communications. Also includes a nextToken that you can use to
      -- retrieve earlier communications.
    , _cdServiceCode :: Maybe Text
      -- ^ The code for the AWS service returned by the call to
      -- DescribeServices.
    , _cdSeverityCode :: Maybe Text
      -- ^ The code for the severity level returned by the call to
      -- DescribeSeverityLevels.
    , _cdStatus :: Maybe Text
      -- ^ The status of the case.
    , _cdSubject :: Maybe Text
      -- ^ The subject line for the case in the AWS Support Center.
    , _cdSubmittedBy :: Maybe Text
      -- ^ The email address of the account that submitted the case.
    , _cdTimeCreated :: Maybe Text
      -- ^ The time that the case was case created in the AWS Support
      -- Center.
    } deriving (Show, Generic)

instance FromJSON CaseDetails

-- | A JSON-formatted name/value pair that represents the category name and
-- category code of the problem, selected from the DescribeServices response
-- for each AWS service.
data Category = Category
    { _cyCode :: Maybe Text
      -- ^ The category code for the support case.
    , _cyName :: Maybe Text
      -- ^ The category name for the support case.
    } deriving (Show, Generic)

instance FromJSON Category

instance ToJSON Category

-- | A communication associated with an AWS Support case. The communication
-- consists of the case ID, the message body, attachment information, the
-- account email address, and the date and time of the communication.
data Communication = Communication
    { _cAttachmentSet :: [AttachmentDetails]
      -- ^ Information about the attachments to the case communication.
    , _cBody :: Maybe Text
      -- ^ The text of the communication between the customer and AWS
      -- Support.
    , _cCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    , _cSubmittedBy :: Maybe Text
      -- ^ The email address of the account that submitted the AWS Support
      -- case.
    , _cTimeCreated :: Maybe Text
      -- ^ The time the communication was created.
    } deriving (Show, Generic)

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

instance FromJSON RecentCaseCommunications

instance ToJSON RecentCaseCommunications

-- | Information about an AWS service returned by the DescribeServices
-- operation.
data Service = Service
    { _ssfCategories :: [Category]
      -- ^ A list of categories that describe the type of support issue a
      -- case describes. Categories consist of a category name and a
      -- category code. Category names and codes are passed to AWS Support
      -- when you call CreateCase.
    , _ssfCode :: Maybe Text
      -- ^ The code for an AWS service returned by the DescribeServices
      -- response. The Name element contains the corresponding friendly
      -- name.
    , _ssfName :: Maybe Text
      -- ^ The friendly name for an AWS service. The Code element contains
      -- the corresponding code.
    } deriving (Show, Generic)

instance FromJSON Service

-- | A code and name pair that represent a severity level that can be applied to
-- a support case.
data SeverityLevel = SeverityLevel
    { _snCode :: Maybe Text
      -- ^ One of four values: "low," "medium," "high," and "urgent". These
      -- values correspond to response times returned to the caller in
      -- SeverityLevel.name.
    , _snName :: Maybe Text
      -- ^ The name of the severity level that corresponds to the severity
      -- level code.
    } deriving (Show, Generic)

instance FromJSON SeverityLevel

-- | The description and metadata for a Trusted Advisor check.
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription
    { _tacdCategory :: Text
      -- ^ The category of the Trusted Advisor check.
    , _tacdDescription :: Text
      -- ^ The description of the Trusted Advisor check, which includes the
      -- alert criteria and recommended actions (contains HTML markup).
    , _tacdId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , _tacdMetadata :: [Text]
      -- ^ The column headings for the data returned by the Trusted Advisor
      -- check. The order of the headings corresponds to the order of the
      -- data in the Metadata element of the TrustedAdvisorResourceDetail
      -- for the check. Metadata contains all the data that is shown in
      -- the Excel download, even in those cases where the UI shows just
      -- summary data.
    , _tacdName :: Text
      -- ^ The display name for the Trusted Advisor check.
    } deriving (Show, Generic)

instance FromJSON TrustedAdvisorCheckDescription

-- | The current refresh status for a check, including the amount of time until
-- the check is eligible for refresh.
data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus
    { _tacrsCheckId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , _tacrsMillisUntilNextRefreshable :: Integer
      -- ^ The amount of time, in milliseconds, until the Trusted Advisor
      -- check is eligible for refresh.
    , _tacrsStatus :: Text
      -- ^ The status of the Trusted Advisor check for which a refresh has
      -- been requested: "none", "enqueued", "processing", "success", or
      -- "abandoned".
    } deriving (Show, Generic)

instance FromJSON TrustedAdvisorCheckRefreshStatus

-- | The detailed results of the Trusted Advisor check.
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult
    { _tacrCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
      -- ^ Summary information that relates to the category of the check.
      -- Cost Optimizing is the only category that is currently supported.
    , _tacrCheckId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , _tacrFlaggedResources :: [TrustedAdvisorResourceDetail]
      -- ^ The details about each resource listed in the check result.
    , _tacrResourcesSummary :: TrustedAdvisorResourcesSummary
      -- ^ Details about AWS resources that were analyzed in a call to
      -- Trusted Advisor DescribeTrustedAdvisorCheckSummaries.
    , _tacrStatus :: Text
      -- ^ The alert status of the check: "ok" (green), "warning" (yellow),
      -- "error" (red), or "not_available".
    , _tacrTimestamp :: Text
      -- ^ The time of the last refresh of the check.
    } deriving (Show, Generic)

instance FromJSON TrustedAdvisorCheckResult

-- | A summary of a Trusted Advisor check result, including the alert status,
-- last refresh, and number of resources examined.
data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary
    { _tacsCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
      -- ^ Summary information that relates to the category of the check.
      -- Cost Optimizing is the only category that is currently supported.
    , _tacsCheckId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , _tacsHasFlaggedResources :: Maybe Bool
      -- ^ Specifies whether the Trusted Advisor check has flagged
      -- resources.
    , _tacsResourcesSummary :: TrustedAdvisorResourcesSummary
      -- ^ Details about AWS resources that were analyzed in a call to
      -- Trusted Advisor DescribeTrustedAdvisorCheckSummaries.
    , _tacsStatus :: Text
      -- ^ The alert status of the check: "ok" (green), "warning" (yellow),
      -- "error" (red), or "not_available".
    , _tacsTimestamp :: Text
      -- ^ The time of the last refresh of the check.
    } deriving (Show, Generic)

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

instance FromJSON TrustedAdvisorCostOptimizingSummary

instance ToJSON TrustedAdvisorCostOptimizingSummary

-- | Contains information about a resource identified by a Trusted Advisor
-- check.
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail
    { _tardIsSuppressed :: Maybe Bool
      -- ^ Specifies whether the AWS resource was ignored by Trusted Advisor
      -- because it was marked as suppressed by the user.
    , _tardMetadata :: [Text]
      -- ^ Additional information about the identified resource. The exact
      -- metadata and its order can be obtained by inspecting the
      -- TrustedAdvisorCheckDescription object returned by the call to
      -- DescribeTrustedAdvisorChecks. Metadata contains all the data that
      -- is shown in the Excel download, even in those cases where the UI
      -- shows just summary data.
    , _tardRegion :: Text
      -- ^ The AWS region in which the identified resource is located.
    , _tardResourceId :: Text
      -- ^ The unique identifier for the identified resource.
    , _tardStatus :: Text
      -- ^ The status code for the resource identified in the Trusted
      -- Advisor check.
    } deriving (Show, Generic)

instance FromJSON TrustedAdvisorResourceDetail

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary
    { _tarsResourcesFlagged :: Integer
      -- ^ The number of AWS resources that were flagged (listed) by the
      -- Trusted Advisor check.
    , _tarsResourcesIgnored :: Integer
      -- ^ The number of AWS resources ignored by Trusted Advisor because
      -- information was unavailable.
    , _tarsResourcesProcessed :: Integer
      -- ^ The number of AWS resources that were analyzed by the Trusted
      -- Advisor check.
    , _tarsResourcesSuppressed :: Integer
      -- ^ The number of AWS resources ignored by Trusted Advisor because
      -- they were marked as suppressed by the user.
    } deriving (Show, Generic)

instance FromJSON TrustedAdvisorResourcesSummary

instance ToJSON TrustedAdvisorResourcesSummary

makeLenses ''TrustedAdvisorCategorySpecificSummary
makeLenses ''Attachment
makeLenses ''AttachmentDetails
makeLenses ''CaseDetails
makeLenses ''Category
makeLenses ''Communication
makeLenses ''RecentCaseCommunications
makeLenses ''Service
makeLenses ''SeverityLevel
makeLenses ''TrustedAdvisorCheckDescription
makeLenses ''TrustedAdvisorCheckRefreshStatus
makeLenses ''TrustedAdvisorCheckResult
makeLenses ''TrustedAdvisorCheckSummary
makeLenses ''TrustedAdvisorCostOptimizingSummary
makeLenses ''TrustedAdvisorResourceDetail
makeLenses ''TrustedAdvisorResourcesSummary
