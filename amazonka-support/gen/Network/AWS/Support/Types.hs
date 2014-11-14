{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.Support.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Support.Types
    (
    -- * Service
      Support
    -- ** Error
    , JSONError

    -- * TrustedAdvisorResourcesSummary
    , TrustedAdvisorResourcesSummary
    , trustedAdvisorResourcesSummary
    , tarsResourcesFlagged
    , tarsResourcesIgnored
    , tarsResourcesProcessed
    , tarsResourcesSuppressed

    -- * Service
    , Service
    , service
    , sCategories
    , sCode
    , sName

    -- * TrustedAdvisorCategorySpecificSummary
    , TrustedAdvisorCategorySpecificSummary
    , trustedAdvisorCategorySpecificSummary
    , tacssCostOptimizing

    -- * Communication
    , Communication
    , communication
    , cAttachmentSet
    , cBody
    , cCaseId
    , cSubmittedBy
    , cTimeCreated

    -- * Category
    , Category
    , category
    , cCode
    , cName

    -- * TrustedAdvisorCheckSummary
    , TrustedAdvisorCheckSummary
    , trustedAdvisorCheckSummary
    , tacsCategorySpecificSummary
    , tacsCheckId
    , tacsHasFlaggedResources
    , tacsResourcesSummary
    , tacsStatus
    , tacsTimestamp

    -- * AttachmentDetails
    , AttachmentDetails
    , attachmentDetails
    , adAttachmentId
    , adFileName

    -- * TrustedAdvisorCheckResult
    , TrustedAdvisorCheckResult
    , trustedAdvisorCheckResult
    , tacrCategorySpecificSummary
    , tacrCheckId
    , tacrFlaggedResources
    , tacrResourcesSummary
    , tacrStatus
    , tacrTimestamp

    -- * TrustedAdvisorCheckDescription
    , TrustedAdvisorCheckDescription
    , trustedAdvisorCheckDescription
    , tacdCategory
    , tacdDescription
    , tacdId
    , tacdMetadata
    , tacdName

    -- * Attachment
    , Attachment
    , attachment
    , aData
    , aFileName

    -- * RecentCaseCommunications
    , RecentCaseCommunications
    , recentCaseCommunications
    , rccCommunications
    , rccNextToken

    -- * TrustedAdvisorResourceDetail
    , TrustedAdvisorResourceDetail
    , trustedAdvisorResourceDetail
    , tardIsSuppressed
    , tardMetadata
    , tardRegion
    , tardResourceId
    , tardStatus

    -- * TrustedAdvisorCostOptimizingSummary
    , TrustedAdvisorCostOptimizingSummary
    , trustedAdvisorCostOptimizingSummary
    , tacosEstimatedMonthlySavings
    , tacosEstimatedPercentMonthlySavings

    -- * SeverityLevel
    , SeverityLevel
    , severityLevel
    , slCode
    , slName

    -- * CaseDetails
    , CaseDetails
    , caseDetails
    , cdCaseId
    , cdCategoryCode
    , cdCcEmailAddresses
    , cdDisplayId
    , cdLanguage
    , cdRecentCommunications
    , cdServiceCode
    , cdSeverityCode
    , cdStatus
    , cdSubject
    , cdSubmittedBy
    , cdTimeCreated

    -- * TrustedAdvisorCheckRefreshStatus
    , TrustedAdvisorCheckRefreshStatus
    , trustedAdvisorCheckRefreshStatus
    , tacrsCheckId
    , tacrsMillisUntilNextRefreshable
    , tacrsStatus
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-04-15@) of the Amazon Support.
data Support deriving (Typeable)

instance AWSService Support where
    type Sg Support = V4
    type Er Support = JSONError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "Support"
        , _svcPrefix   = "support"
        , _svcVersion  = "2013-04-15"
        , _svcTarget   = Nothing
        }

    handle = jsonError alwaysFail

data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary
    { _tarsResourcesFlagged    :: Integer
    , _tarsResourcesIgnored    :: Integer
    , _tarsResourcesProcessed  :: Integer
    , _tarsResourcesSuppressed :: Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'TrustedAdvisorResourcesSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tarsResourcesFlagged' @::@ 'Integer'
--
-- * 'tarsResourcesIgnored' @::@ 'Integer'
--
-- * 'tarsResourcesProcessed' @::@ 'Integer'
--
-- * 'tarsResourcesSuppressed' @::@ 'Integer'
--
trustedAdvisorResourcesSummary :: Integer -- ^ 'tarsResourcesProcessed'
                               -> Integer -- ^ 'tarsResourcesFlagged'
                               -> Integer -- ^ 'tarsResourcesIgnored'
                               -> Integer -- ^ 'tarsResourcesSuppressed'
                               -> TrustedAdvisorResourcesSummary
trustedAdvisorResourcesSummary p1 p2 p3 p4 = TrustedAdvisorResourcesSummary
    { _tarsResourcesProcessed  = p1
    , _tarsResourcesFlagged    = p2
    , _tarsResourcesIgnored    = p3
    , _tarsResourcesSuppressed = p4
    }

-- | The number of AWS resources that were flagged (listed) by the Trusted
-- Advisor check.
tarsResourcesFlagged :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesFlagged =
    lens _tarsResourcesFlagged (\s a -> s { _tarsResourcesFlagged = a })

-- | The number of AWS resources ignored by Trusted Advisor because
-- information was unavailable.
tarsResourcesIgnored :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesIgnored =
    lens _tarsResourcesIgnored (\s a -> s { _tarsResourcesIgnored = a })

-- | The number of AWS resources that were analyzed by the Trusted Advisor
-- check.
tarsResourcesProcessed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesProcessed =
    lens _tarsResourcesProcessed (\s a -> s { _tarsResourcesProcessed = a })

-- | The number of AWS resources ignored by Trusted Advisor because they were
-- marked as suppressed by the user.
tarsResourcesSuppressed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesSuppressed =
    lens _tarsResourcesSuppressed (\s a -> s { _tarsResourcesSuppressed = a })

data Service = Service
    { _sCategories :: [Category]
    , _sCode       :: Maybe Text
    , _sName       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Service' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sCategories' @::@ ['Category']
--
-- * 'sCode' @::@ 'Maybe' 'Text'
--
-- * 'sName' @::@ 'Maybe' 'Text'
--
service :: Service
service = Service
    { _sCode       = Nothing
    , _sName       = Nothing
    , _sCategories = mempty
    }

-- | A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to AWS Support when you call
-- CreateCase.
sCategories :: Lens' Service [Category]
sCategories = lens _sCategories (\s a -> s { _sCategories = a })

-- | The code for an AWS service returned by the DescribeServices response.
-- The Name element contains the corresponding friendly name.
sCode :: Lens' Service (Maybe Text)
sCode = lens _sCode (\s a -> s { _sCode = a })

-- | The friendly name for an AWS service. The Code element contains the
-- corresponding code.
sName :: Lens' Service (Maybe Text)
sName = lens _sName (\s a -> s { _sName = a })

newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary
    { _tacssCostOptimizing :: Maybe TrustedAdvisorCostOptimizingSummary
    } deriving (Eq, Show, Generic)

-- | 'TrustedAdvisorCategorySpecificSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tacssCostOptimizing' @::@ 'Maybe' 'TrustedAdvisorCostOptimizingSummary'
--
trustedAdvisorCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
trustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary
    { _tacssCostOptimizing = Nothing
    }

-- | The summary information about cost savings for a Trusted Advisor check
-- that is in the Cost Optimizing category.
tacssCostOptimizing :: Lens' TrustedAdvisorCategorySpecificSummary (Maybe TrustedAdvisorCostOptimizingSummary)
tacssCostOptimizing =
    lens _tacssCostOptimizing (\s a -> s { _tacssCostOptimizing = a })

data Communication = Communication
    { _cAttachmentSet :: [AttachmentDetails]
    , _cBody          :: Maybe Text
    , _cCaseId        :: Maybe Text
    , _cSubmittedBy   :: Maybe Text
    , _cTimeCreated   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Communication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cAttachmentSet' @::@ ['AttachmentDetails']
--
-- * 'cBody' @::@ 'Maybe' 'Text'
--
-- * 'cCaseId' @::@ 'Maybe' 'Text'
--
-- * 'cSubmittedBy' @::@ 'Maybe' 'Text'
--
-- * 'cTimeCreated' @::@ 'Maybe' 'Text'
--
communication :: Communication
communication = Communication
    { _cCaseId        = Nothing
    , _cBody          = Nothing
    , _cSubmittedBy   = Nothing
    , _cTimeCreated   = Nothing
    , _cAttachmentSet = mempty
    }

-- | Information about the attachments to the case communication.
cAttachmentSet :: Lens' Communication [AttachmentDetails]
cAttachmentSet = lens _cAttachmentSet (\s a -> s { _cAttachmentSet = a })

-- | The text of the communication between the customer and AWS Support.
cBody :: Lens' Communication (Maybe Text)
cBody = lens _cBody (\s a -> s { _cBody = a })

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
cCaseId :: Lens' Communication (Maybe Text)
cCaseId = lens _cCaseId (\s a -> s { _cCaseId = a })

-- | The email address of the account that submitted the AWS Support case.
cSubmittedBy :: Lens' Communication (Maybe Text)
cSubmittedBy = lens _cSubmittedBy (\s a -> s { _cSubmittedBy = a })

-- | The time the communication was created.
cTimeCreated :: Lens' Communication (Maybe Text)
cTimeCreated = lens _cTimeCreated (\s a -> s { _cTimeCreated = a })

data Category = Category
    { _cCode :: Maybe Text
    , _cName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Category' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cCode' @::@ 'Maybe' 'Text'
--
-- * 'cName' @::@ 'Maybe' 'Text'
--
category :: Category
category = Category
    { _cCode = Nothing
    , _cName = Nothing
    }

-- | The category code for the support case.
cCode :: Lens' Category (Maybe Text)
cCode = lens _cCode (\s a -> s { _cCode = a })

-- | The category name for the support case.
cName :: Lens' Category (Maybe Text)
cName = lens _cName (\s a -> s { _cName = a })

data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary
    { _tacsCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
    , _tacsCheckId                 :: Text
    , _tacsHasFlaggedResources     :: Maybe Bool
    , _tacsResourcesSummary        :: TrustedAdvisorResourcesSummary
    , _tacsStatus                  :: Text
    , _tacsTimestamp               :: Text
    } deriving (Eq, Show, Generic)

-- | 'TrustedAdvisorCheckSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tacsCategorySpecificSummary' @::@ 'TrustedAdvisorCategorySpecificSummary'
--
-- * 'tacsCheckId' @::@ 'Text'
--
-- * 'tacsHasFlaggedResources' @::@ 'Maybe' 'Bool'
--
-- * 'tacsResourcesSummary' @::@ 'TrustedAdvisorResourcesSummary'
--
-- * 'tacsStatus' @::@ 'Text'
--
-- * 'tacsTimestamp' @::@ 'Text'
--
trustedAdvisorCheckSummary :: Text -- ^ 'tacsCheckId'
                           -> Text -- ^ 'tacsTimestamp'
                           -> Text -- ^ 'tacsStatus'
                           -> TrustedAdvisorResourcesSummary -- ^ 'tacsResourcesSummary'
                           -> TrustedAdvisorCategorySpecificSummary -- ^ 'tacsCategorySpecificSummary'
                           -> TrustedAdvisorCheckSummary
trustedAdvisorCheckSummary p1 p2 p3 p4 p5 = TrustedAdvisorCheckSummary
    { _tacsCheckId                 = p1
    , _tacsTimestamp               = p2
    , _tacsStatus                  = p3
    , _tacsResourcesSummary        = p4
    , _tacsCategorySpecificSummary = p5
    , _tacsHasFlaggedResources     = Nothing
    }

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacsCategorySpecificSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorCategorySpecificSummary
tacsCategorySpecificSummary =
    lens _tacsCategorySpecificSummary
        (\s a -> s { _tacsCategorySpecificSummary = a })

-- | The unique identifier for the Trusted Advisor check.
tacsCheckId :: Lens' TrustedAdvisorCheckSummary Text
tacsCheckId = lens _tacsCheckId (\s a -> s { _tacsCheckId = a })

-- | Specifies whether the Trusted Advisor check has flagged resources.
tacsHasFlaggedResources :: Lens' TrustedAdvisorCheckSummary (Maybe Bool)
tacsHasFlaggedResources =
    lens _tacsHasFlaggedResources (\s a -> s { _tacsHasFlaggedResources = a })

tacsResourcesSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorResourcesSummary
tacsResourcesSummary =
    lens _tacsResourcesSummary (\s a -> s { _tacsResourcesSummary = a })

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacsStatus :: Lens' TrustedAdvisorCheckSummary Text
tacsStatus = lens _tacsStatus (\s a -> s { _tacsStatus = a })

-- | The time of the last refresh of the check.
tacsTimestamp :: Lens' TrustedAdvisorCheckSummary Text
tacsTimestamp = lens _tacsTimestamp (\s a -> s { _tacsTimestamp = a })

data AttachmentDetails = AttachmentDetails
    { _adAttachmentId :: Maybe Text
    , _adFileName     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AttachmentDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adAttachmentId' @::@ 'Maybe' 'Text'
--
-- * 'adFileName' @::@ 'Maybe' 'Text'
--
attachmentDetails :: AttachmentDetails
attachmentDetails = AttachmentDetails
    { _adAttachmentId = Nothing
    , _adFileName     = Nothing
    }

-- | The ID of the attachment.
adAttachmentId :: Lens' AttachmentDetails (Maybe Text)
adAttachmentId = lens _adAttachmentId (\s a -> s { _adAttachmentId = a })

-- | The file name of the attachment.
adFileName :: Lens' AttachmentDetails (Maybe Text)
adFileName = lens _adFileName (\s a -> s { _adFileName = a })

data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult
    { _tacrCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
    , _tacrCheckId                 :: Text
    , _tacrFlaggedResources        :: [TrustedAdvisorResourceDetail]
    , _tacrResourcesSummary        :: TrustedAdvisorResourcesSummary
    , _tacrStatus                  :: Text
    , _tacrTimestamp               :: Text
    } deriving (Eq, Show, Generic)

-- | 'TrustedAdvisorCheckResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tacrCategorySpecificSummary' @::@ 'TrustedAdvisorCategorySpecificSummary'
--
-- * 'tacrCheckId' @::@ 'Text'
--
-- * 'tacrFlaggedResources' @::@ ['TrustedAdvisorResourceDetail']
--
-- * 'tacrResourcesSummary' @::@ 'TrustedAdvisorResourcesSummary'
--
-- * 'tacrStatus' @::@ 'Text'
--
-- * 'tacrTimestamp' @::@ 'Text'
--
trustedAdvisorCheckResult :: Text -- ^ 'tacrCheckId'
                          -> Text -- ^ 'tacrTimestamp'
                          -> Text -- ^ 'tacrStatus'
                          -> TrustedAdvisorResourcesSummary -- ^ 'tacrResourcesSummary'
                          -> TrustedAdvisorCategorySpecificSummary -- ^ 'tacrCategorySpecificSummary'
                          -> TrustedAdvisorCheckResult
trustedAdvisorCheckResult p1 p2 p3 p4 p5 = TrustedAdvisorCheckResult
    { _tacrCheckId                 = p1
    , _tacrTimestamp               = p2
    , _tacrStatus                  = p3
    , _tacrResourcesSummary        = p4
    , _tacrCategorySpecificSummary = p5
    , _tacrFlaggedResources        = mempty
    }

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
tacrCategorySpecificSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorCategorySpecificSummary
tacrCategorySpecificSummary =
    lens _tacrCategorySpecificSummary
        (\s a -> s { _tacrCategorySpecificSummary = a })

-- | The unique identifier for the Trusted Advisor check.
tacrCheckId :: Lens' TrustedAdvisorCheckResult Text
tacrCheckId = lens _tacrCheckId (\s a -> s { _tacrCheckId = a })

-- | The details about each resource listed in the check result.
tacrFlaggedResources :: Lens' TrustedAdvisorCheckResult [TrustedAdvisorResourceDetail]
tacrFlaggedResources =
    lens _tacrFlaggedResources (\s a -> s { _tacrFlaggedResources = a })

tacrResourcesSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorResourcesSummary
tacrResourcesSummary =
    lens _tacrResourcesSummary (\s a -> s { _tacrResourcesSummary = a })

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
tacrStatus :: Lens' TrustedAdvisorCheckResult Text
tacrStatus = lens _tacrStatus (\s a -> s { _tacrStatus = a })

-- | The time of the last refresh of the check.
tacrTimestamp :: Lens' TrustedAdvisorCheckResult Text
tacrTimestamp = lens _tacrTimestamp (\s a -> s { _tacrTimestamp = a })

data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription
    { _tacdCategory    :: Text
    , _tacdDescription :: Text
    , _tacdId          :: Text
    , _tacdMetadata    :: [Text]
    , _tacdName        :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'TrustedAdvisorCheckDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tacdCategory' @::@ 'Text'
--
-- * 'tacdDescription' @::@ 'Text'
--
-- * 'tacdId' @::@ 'Text'
--
-- * 'tacdMetadata' @::@ ['Text']
--
-- * 'tacdName' @::@ 'Text'
--
trustedAdvisorCheckDescription :: Text -- ^ 'tacdId'
                               -> Text -- ^ 'tacdName'
                               -> Text -- ^ 'tacdDescription'
                               -> Text -- ^ 'tacdCategory'
                               -> TrustedAdvisorCheckDescription
trustedAdvisorCheckDescription p1 p2 p3 p4 = TrustedAdvisorCheckDescription
    { _tacdId          = p1
    , _tacdName        = p2
    , _tacdDescription = p3
    , _tacdCategory    = p4
    , _tacdMetadata    = mempty
    }

-- | The category of the Trusted Advisor check.
tacdCategory :: Lens' TrustedAdvisorCheckDescription Text
tacdCategory = lens _tacdCategory (\s a -> s { _tacdCategory = a })

-- | The description of the Trusted Advisor check, which includes the alert
-- criteria and recommended actions (contains HTML markup).
tacdDescription :: Lens' TrustedAdvisorCheckDescription Text
tacdDescription = lens _tacdDescription (\s a -> s { _tacdDescription = a })

-- | The unique identifier for the Trusted Advisor check.
tacdId :: Lens' TrustedAdvisorCheckDescription Text
tacdId = lens _tacdId (\s a -> s { _tacdId = a })

-- | The column headings for the data returned by the Trusted Advisor check.
-- The order of the headings corresponds to the order of the data in the
-- Metadata element of the TrustedAdvisorResourceDetail for the check.
-- Metadata contains all the data that is shown in the Excel download, even
-- in those cases where the UI shows just summary data.
tacdMetadata :: Lens' TrustedAdvisorCheckDescription [Text]
tacdMetadata = lens _tacdMetadata (\s a -> s { _tacdMetadata = a })

-- | The display name for the Trusted Advisor check.
tacdName :: Lens' TrustedAdvisorCheckDescription Text
tacdName = lens _tacdName (\s a -> s { _tacdName = a })

data Attachment = Attachment
    { _aData     :: Maybe Base64
    , _aFileName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Attachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aData' @::@ 'Maybe' 'Base64'
--
-- * 'aFileName' @::@ 'Maybe' 'Text'
--
attachment :: Attachment
attachment = Attachment
    { _aFileName = Nothing
    , _aData     = Nothing
    }

-- | The content of the attachment file.
aData :: Lens' Attachment (Maybe Base64)
aData = lens _aData (\s a -> s { _aData = a })

-- | The name of the attachment file.
aFileName :: Lens' Attachment (Maybe Text)
aFileName = lens _aFileName (\s a -> s { _aFileName = a })

data RecentCaseCommunications = RecentCaseCommunications
    { _rccCommunications :: [Communication]
    , _rccNextToken      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'RecentCaseCommunications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rccCommunications' @::@ ['Communication']
--
-- * 'rccNextToken' @::@ 'Maybe' 'Text'
--
recentCaseCommunications :: RecentCaseCommunications
recentCaseCommunications = RecentCaseCommunications
    { _rccCommunications = mempty
    , _rccNextToken      = Nothing
    }

-- | The five most recent communications associated with the case.
rccCommunications :: Lens' RecentCaseCommunications [Communication]
rccCommunications =
    lens _rccCommunications (\s a -> s { _rccCommunications = a })

-- | A resumption point for pagination.
rccNextToken :: Lens' RecentCaseCommunications (Maybe Text)
rccNextToken = lens _rccNextToken (\s a -> s { _rccNextToken = a })

data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail
    { _tardIsSuppressed :: Maybe Bool
    , _tardMetadata     :: [Text]
    , _tardRegion       :: Text
    , _tardResourceId   :: Text
    , _tardStatus       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'TrustedAdvisorResourceDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tardIsSuppressed' @::@ 'Maybe' 'Bool'
--
-- * 'tardMetadata' @::@ ['Text']
--
-- * 'tardRegion' @::@ 'Text'
--
-- * 'tardResourceId' @::@ 'Text'
--
-- * 'tardStatus' @::@ 'Text'
--
trustedAdvisorResourceDetail :: Text -- ^ 'tardStatus'
                             -> Text -- ^ 'tardRegion'
                             -> Text -- ^ 'tardResourceId'
                             -> TrustedAdvisorResourceDetail
trustedAdvisorResourceDetail p1 p2 p3 = TrustedAdvisorResourceDetail
    { _tardStatus       = p1
    , _tardRegion       = p2
    , _tardResourceId   = p3
    , _tardIsSuppressed = Nothing
    , _tardMetadata     = mempty
    }

-- | Specifies whether the AWS resource was ignored by Trusted Advisor because
-- it was marked as suppressed by the user.
tardIsSuppressed :: Lens' TrustedAdvisorResourceDetail (Maybe Bool)
tardIsSuppressed = lens _tardIsSuppressed (\s a -> s { _tardIsSuppressed = a })

-- | Additional information about the identified resource. The exact metadata
-- and its order can be obtained by inspecting the
-- TrustedAdvisorCheckDescription object returned by the call to
-- DescribeTrustedAdvisorChecks. Metadata contains all the data that is
-- shown in the Excel download, even in those cases where the UI shows just
-- summary data.
tardMetadata :: Lens' TrustedAdvisorResourceDetail [Text]
tardMetadata = lens _tardMetadata (\s a -> s { _tardMetadata = a })

-- | The AWS region in which the identified resource is located.
tardRegion :: Lens' TrustedAdvisorResourceDetail Text
tardRegion = lens _tardRegion (\s a -> s { _tardRegion = a })

-- | The unique identifier for the identified resource.
tardResourceId :: Lens' TrustedAdvisorResourceDetail Text
tardResourceId = lens _tardResourceId (\s a -> s { _tardResourceId = a })

-- | The status code for the resource identified in the Trusted Advisor check.
tardStatus :: Lens' TrustedAdvisorResourceDetail Text
tardStatus = lens _tardStatus (\s a -> s { _tardStatus = a })

data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary
    { _tacosEstimatedMonthlySavings        :: Double
    , _tacosEstimatedPercentMonthlySavings :: Double
    } deriving (Eq, Ord, Show, Generic)

-- | 'TrustedAdvisorCostOptimizingSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tacosEstimatedMonthlySavings' @::@ 'Double'
--
-- * 'tacosEstimatedPercentMonthlySavings' @::@ 'Double'
--
trustedAdvisorCostOptimizingSummary :: Double -- ^ 'tacosEstimatedMonthlySavings'
                                    -> Double -- ^ 'tacosEstimatedPercentMonthlySavings'
                                    -> TrustedAdvisorCostOptimizingSummary
trustedAdvisorCostOptimizingSummary p1 p2 = TrustedAdvisorCostOptimizingSummary
    { _tacosEstimatedMonthlySavings        = p1
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

data SeverityLevel = SeverityLevel
    { _slCode :: Maybe Text
    , _slName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SeverityLevel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slCode' @::@ 'Maybe' 'Text'
--
-- * 'slName' @::@ 'Maybe' 'Text'
--
severityLevel :: SeverityLevel
severityLevel = SeverityLevel
    { _slCode = Nothing
    , _slName = Nothing
    }

-- | One of four values: "low," "medium," "high," and "urgent". These values
-- correspond to response times returned to the caller in
-- SeverityLevel.name.
slCode :: Lens' SeverityLevel (Maybe Text)
slCode = lens _slCode (\s a -> s { _slCode = a })

-- | The name of the severity level that corresponds to the severity level
-- code.
slName :: Lens' SeverityLevel (Maybe Text)
slName = lens _slName (\s a -> s { _slName = a })

data CaseDetails = CaseDetails
    { _cdCaseId               :: Maybe Text
    , _cdCategoryCode         :: Maybe Text
    , _cdCcEmailAddresses     :: [Text]
    , _cdDisplayId            :: Maybe Text
    , _cdLanguage             :: Maybe Text
    , _cdRecentCommunications :: Maybe RecentCaseCommunications
    , _cdServiceCode          :: Maybe Text
    , _cdSeverityCode         :: Maybe Text
    , _cdStatus               :: Maybe Text
    , _cdSubject              :: Maybe Text
    , _cdSubmittedBy          :: Maybe Text
    , _cdTimeCreated          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CaseDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdCaseId' @::@ 'Maybe' 'Text'
--
-- * 'cdCategoryCode' @::@ 'Maybe' 'Text'
--
-- * 'cdCcEmailAddresses' @::@ ['Text']
--
-- * 'cdDisplayId' @::@ 'Maybe' 'Text'
--
-- * 'cdLanguage' @::@ 'Maybe' 'Text'
--
-- * 'cdRecentCommunications' @::@ 'Maybe' 'RecentCaseCommunications'
--
-- * 'cdServiceCode' @::@ 'Maybe' 'Text'
--
-- * 'cdSeverityCode' @::@ 'Maybe' 'Text'
--
-- * 'cdStatus' @::@ 'Maybe' 'Text'
--
-- * 'cdSubject' @::@ 'Maybe' 'Text'
--
-- * 'cdSubmittedBy' @::@ 'Maybe' 'Text'
--
-- * 'cdTimeCreated' @::@ 'Maybe' 'Text'
--
caseDetails :: CaseDetails
caseDetails = CaseDetails
    { _cdCaseId               = Nothing
    , _cdDisplayId            = Nothing
    , _cdSubject              = Nothing
    , _cdStatus               = Nothing
    , _cdServiceCode          = Nothing
    , _cdCategoryCode         = Nothing
    , _cdSeverityCode         = Nothing
    , _cdSubmittedBy          = Nothing
    , _cdTimeCreated          = Nothing
    , _cdRecentCommunications = Nothing
    , _cdCcEmailAddresses     = mempty
    , _cdLanguage             = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
cdCaseId :: Lens' CaseDetails (Maybe Text)
cdCaseId = lens _cdCaseId (\s a -> s { _cdCaseId = a })

-- | The category of problem for the AWS Support case.
cdCategoryCode :: Lens' CaseDetails (Maybe Text)
cdCategoryCode = lens _cdCategoryCode (\s a -> s { _cdCategoryCode = a })

-- | The email addresses that receive copies of communication about the case.
cdCcEmailAddresses :: Lens' CaseDetails [Text]
cdCcEmailAddresses =
    lens _cdCcEmailAddresses (\s a -> s { _cdCcEmailAddresses = a })

-- | The ID displayed for the case in the AWS Support Center. This is a
-- numeric string.
cdDisplayId :: Lens' CaseDetails (Maybe Text)
cdDisplayId = lens _cdDisplayId (\s a -> s { _cdDisplayId = a })

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
cdLanguage :: Lens' CaseDetails (Maybe Text)
cdLanguage = lens _cdLanguage (\s a -> s { _cdLanguage = a })

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also includes
-- a nextToken that you can use to retrieve earlier communications.
cdRecentCommunications :: Lens' CaseDetails (Maybe RecentCaseCommunications)
cdRecentCommunications =
    lens _cdRecentCommunications (\s a -> s { _cdRecentCommunications = a })

-- | The code for the AWS service returned by the call to DescribeServices.
cdServiceCode :: Lens' CaseDetails (Maybe Text)
cdServiceCode = lens _cdServiceCode (\s a -> s { _cdServiceCode = a })

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
cdSeverityCode :: Lens' CaseDetails (Maybe Text)
cdSeverityCode = lens _cdSeverityCode (\s a -> s { _cdSeverityCode = a })

-- | The status of the case.
cdStatus :: Lens' CaseDetails (Maybe Text)
cdStatus = lens _cdStatus (\s a -> s { _cdStatus = a })

-- | The subject line for the case in the AWS Support Center.
cdSubject :: Lens' CaseDetails (Maybe Text)
cdSubject = lens _cdSubject (\s a -> s { _cdSubject = a })

-- | The email address of the account that submitted the case.
cdSubmittedBy :: Lens' CaseDetails (Maybe Text)
cdSubmittedBy = lens _cdSubmittedBy (\s a -> s { _cdSubmittedBy = a })

-- | The time that the case was case created in the AWS Support Center.
cdTimeCreated :: Lens' CaseDetails (Maybe Text)
cdTimeCreated = lens _cdTimeCreated (\s a -> s { _cdTimeCreated = a })

data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus
    { _tacrsCheckId                    :: Text
    , _tacrsMillisUntilNextRefreshable :: Integer
    , _tacrsStatus                     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'TrustedAdvisorCheckRefreshStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tacrsCheckId' @::@ 'Text'
--
-- * 'tacrsMillisUntilNextRefreshable' @::@ 'Integer'
--
-- * 'tacrsStatus' @::@ 'Text'
--
trustedAdvisorCheckRefreshStatus :: Text -- ^ 'tacrsCheckId'
                                 -> Text -- ^ 'tacrsStatus'
                                 -> Integer -- ^ 'tacrsMillisUntilNextRefreshable'
                                 -> TrustedAdvisorCheckRefreshStatus
trustedAdvisorCheckRefreshStatus p1 p2 p3 = TrustedAdvisorCheckRefreshStatus
    { _tacrsCheckId                    = p1
    , _tacrsStatus                     = p2
    , _tacrsMillisUntilNextRefreshable = p3
    }

-- | The unique identifier for the Trusted Advisor check.
tacrsCheckId :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsCheckId = lens _tacrsCheckId (\s a -> s { _tacrsCheckId = a })

-- | The amount of time, in milliseconds, until the Trusted Advisor check is
-- eligible for refresh.
tacrsMillisUntilNextRefreshable :: Lens' TrustedAdvisorCheckRefreshStatus Integer
tacrsMillisUntilNextRefreshable =
    lens _tacrsMillisUntilNextRefreshable
        (\s a -> s { _tacrsMillisUntilNextRefreshable = a })

-- | The status of the Trusted Advisor check for which a refresh has been
-- requested: "none", "enqueued", "processing", "success", or "abandoned".
tacrsStatus :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsStatus = lens _tacrsStatus (\s a -> s { _tacrsStatus = a })
