{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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

    -- * SupportService
    , SupportService
    , supportService
    , ssCategories
    , ssCode
    , ssName

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

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Version @2013-04-15@ of the Amazon Support service.
data Support

instance AWSService Support where
    type Sg Support = V4
    type Er Support = JSONError

    service = Service
        { _svcEndpoint     = regional
        , _svcAbbrev       = "Support"
        , _svcPrefix       = "support"
        , _svcVersion      = "2013-04-15"
        , _svcTargetPrefix = Just "AWSSupport_20130415"
        , _svcJSONVersion  = Just "1.1"
        }

    handle = jsonError statusSuccess

data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary
    { _tarsResourcesFlagged    :: Integer
    , _tarsResourcesIgnored    :: Integer
    , _tarsResourcesProcessed  :: Integer
    , _tarsResourcesSuppressed :: Integer
    } deriving (Eq, Ord, Show)

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

-- | The number of AWS resources that were flagged (listed) by the Trusted Advisor
-- check.
--
tarsResourcesFlagged :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesFlagged =
    lens _tarsResourcesFlagged (\s a -> s { _tarsResourcesFlagged = a })

-- | The number of AWS resources ignored by Trusted Advisor because information
-- was unavailable.
--
tarsResourcesIgnored :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesIgnored =
    lens _tarsResourcesIgnored (\s a -> s { _tarsResourcesIgnored = a })

-- | The number of AWS resources that were analyzed by the Trusted Advisor check.
--
tarsResourcesProcessed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesProcessed =
    lens _tarsResourcesProcessed (\s a -> s { _tarsResourcesProcessed = a })

-- | The number of AWS resources ignored by Trusted Advisor because they were
-- marked as suppressed by the user.
--
tarsResourcesSuppressed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesSuppressed =
    lens _tarsResourcesSuppressed (\s a -> s { _tarsResourcesSuppressed = a })

instance FromJSON TrustedAdvisorResourcesSummary where
    parseJSON = withObject "TrustedAdvisorResourcesSummary" $ \o -> TrustedAdvisorResourcesSummary
        <$> o .:  "resourcesFlagged"
        <*> o .:  "resourcesIgnored"
        <*> o .:  "resourcesProcessed"
        <*> o .:  "resourcesSuppressed"

instance ToJSON TrustedAdvisorResourcesSummary where
    toJSON TrustedAdvisorResourcesSummary{..} = object
        [ "resourcesProcessed"  .= _tarsResourcesProcessed
        , "resourcesFlagged"    .= _tarsResourcesFlagged
        , "resourcesIgnored"    .= _tarsResourcesIgnored
        , "resourcesSuppressed" .= _tarsResourcesSuppressed
        ]

newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary
    { _tacssCostOptimizing :: Maybe TrustedAdvisorCostOptimizingSummary
    } deriving (Eq, Show)

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

-- | The summary information about cost savings for a Trusted Advisor check that
-- is in the Cost Optimizing category.
--
tacssCostOptimizing :: Lens' TrustedAdvisorCategorySpecificSummary (Maybe TrustedAdvisorCostOptimizingSummary)
tacssCostOptimizing =
    lens _tacssCostOptimizing (\s a -> s { _tacssCostOptimizing = a })

instance FromJSON TrustedAdvisorCategorySpecificSummary where
    parseJSON = withObject "TrustedAdvisorCategorySpecificSummary" $ \o -> TrustedAdvisorCategorySpecificSummary
        <$> o .:? "costOptimizing"

instance ToJSON TrustedAdvisorCategorySpecificSummary where
    toJSON TrustedAdvisorCategorySpecificSummary{..} = object
        [ "costOptimizing" .= _tacssCostOptimizing
        ]

data Communication = Communication
    { _cAttachmentSet :: List "attachmentSet" AttachmentDetails
    , _cBody          :: Maybe Text
    , _cCaseId        :: Maybe Text
    , _cSubmittedBy   :: Maybe Text
    , _cTimeCreated   :: Maybe Text
    } deriving (Eq, Show)

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
--
cAttachmentSet :: Lens' Communication [AttachmentDetails]
cAttachmentSet = lens _cAttachmentSet (\s a -> s { _cAttachmentSet = a }) . _List

-- | The text of the communication between the customer and AWS Support.
--
cBody :: Lens' Communication (Maybe Text)
cBody = lens _cBody (\s a -> s { _cBody = a })

-- | The AWS Support case ID requested or returned in the call. The case ID is an
-- alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
cCaseId :: Lens' Communication (Maybe Text)
cCaseId = lens _cCaseId (\s a -> s { _cCaseId = a })

-- | The email address of the account that submitted the AWS Support case.
--
cSubmittedBy :: Lens' Communication (Maybe Text)
cSubmittedBy = lens _cSubmittedBy (\s a -> s { _cSubmittedBy = a })

-- | The time the communication was created.
--
cTimeCreated :: Lens' Communication (Maybe Text)
cTimeCreated = lens _cTimeCreated (\s a -> s { _cTimeCreated = a })

instance FromJSON Communication where
    parseJSON = withObject "Communication" $ \o -> Communication
        <$> o .:  "attachmentSet"
        <*> o .:? "body"
        <*> o .:? "caseId"
        <*> o .:? "submittedBy"
        <*> o .:? "timeCreated"

instance ToJSON Communication where
    toJSON Communication{..} = object
        [ "caseId"        .= _cCaseId
        , "body"          .= _cBody
        , "submittedBy"   .= _cSubmittedBy
        , "timeCreated"   .= _cTimeCreated
        , "attachmentSet" .= _cAttachmentSet
        ]

data Category = Category
    { _cCode :: Maybe Text
    , _cName :: Maybe Text
    } deriving (Eq, Ord, Show)

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
--
cCode :: Lens' Category (Maybe Text)
cCode = lens _cCode (\s a -> s { _cCode = a })

-- | The category name for the support case.
--
cName :: Lens' Category (Maybe Text)
cName = lens _cName (\s a -> s { _cName = a })

instance FromJSON Category where
    parseJSON = withObject "Category" $ \o -> Category
        <$> o .:? "code"
        <*> o .:? "name"

instance ToJSON Category where
    toJSON Category{..} = object
        [ "code" .= _cCode
        , "name" .= _cName
        ]

data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary
    { _tacsCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
    , _tacsCheckId                 :: Text
    , _tacsHasFlaggedResources     :: Maybe Bool
    , _tacsResourcesSummary        :: TrustedAdvisorResourcesSummary
    , _tacsStatus                  :: Text
    , _tacsTimestamp               :: Text
    } deriving (Eq, Show)

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
--
tacsCategorySpecificSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorCategorySpecificSummary
tacsCategorySpecificSummary =
    lens _tacsCategorySpecificSummary
        (\s a -> s { _tacsCategorySpecificSummary = a })

-- | The unique identifier for the Trusted Advisor check.
--
tacsCheckId :: Lens' TrustedAdvisorCheckSummary Text
tacsCheckId = lens _tacsCheckId (\s a -> s { _tacsCheckId = a })

-- | Specifies whether the Trusted Advisor check has flagged resources.
--
tacsHasFlaggedResources :: Lens' TrustedAdvisorCheckSummary (Maybe Bool)
tacsHasFlaggedResources =
    lens _tacsHasFlaggedResources (\s a -> s { _tacsHasFlaggedResources = a })

tacsResourcesSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorResourcesSummary
tacsResourcesSummary =
    lens _tacsResourcesSummary (\s a -> s { _tacsResourcesSummary = a })

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
--
tacsStatus :: Lens' TrustedAdvisorCheckSummary Text
tacsStatus = lens _tacsStatus (\s a -> s { _tacsStatus = a })

-- | The time of the last refresh of the check.
--
tacsTimestamp :: Lens' TrustedAdvisorCheckSummary Text
tacsTimestamp = lens _tacsTimestamp (\s a -> s { _tacsTimestamp = a })

instance FromJSON TrustedAdvisorCheckSummary where
    parseJSON = withObject "TrustedAdvisorCheckSummary" $ \o -> TrustedAdvisorCheckSummary
        <$> o .:  "categorySpecificSummary"
        <*> o .:  "checkId"
        <*> o .:? "hasFlaggedResources"
        <*> o .:  "resourcesSummary"
        <*> o .:  "status"
        <*> o .:  "timestamp"

instance ToJSON TrustedAdvisorCheckSummary where
    toJSON TrustedAdvisorCheckSummary{..} = object
        [ "checkId"                 .= _tacsCheckId
        , "timestamp"               .= _tacsTimestamp
        , "status"                  .= _tacsStatus
        , "hasFlaggedResources"     .= _tacsHasFlaggedResources
        , "resourcesSummary"        .= _tacsResourcesSummary
        , "categorySpecificSummary" .= _tacsCategorySpecificSummary
        ]

data AttachmentDetails = AttachmentDetails
    { _adAttachmentId :: Maybe Text
    , _adFileName     :: Maybe Text
    } deriving (Eq, Ord, Show)

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
--
adAttachmentId :: Lens' AttachmentDetails (Maybe Text)
adAttachmentId = lens _adAttachmentId (\s a -> s { _adAttachmentId = a })

-- | The file name of the attachment.
--
adFileName :: Lens' AttachmentDetails (Maybe Text)
adFileName = lens _adFileName (\s a -> s { _adFileName = a })

instance FromJSON AttachmentDetails where
    parseJSON = withObject "AttachmentDetails" $ \o -> AttachmentDetails
        <$> o .:? "attachmentId"
        <*> o .:? "fileName"

instance ToJSON AttachmentDetails where
    toJSON AttachmentDetails{..} = object
        [ "attachmentId" .= _adAttachmentId
        , "fileName"     .= _adFileName
        ]

data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult
    { _tacrCategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
    , _tacrCheckId                 :: Text
    , _tacrFlaggedResources        :: List "flaggedResources" TrustedAdvisorResourceDetail
    , _tacrResourcesSummary        :: TrustedAdvisorResourcesSummary
    , _tacrStatus                  :: Text
    , _tacrTimestamp               :: Text
    } deriving (Eq, Show)

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
--
tacrCategorySpecificSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorCategorySpecificSummary
tacrCategorySpecificSummary =
    lens _tacrCategorySpecificSummary
        (\s a -> s { _tacrCategorySpecificSummary = a })

-- | The unique identifier for the Trusted Advisor check.
--
tacrCheckId :: Lens' TrustedAdvisorCheckResult Text
tacrCheckId = lens _tacrCheckId (\s a -> s { _tacrCheckId = a })

-- | The details about each resource listed in the check result.
--
tacrFlaggedResources :: Lens' TrustedAdvisorCheckResult [TrustedAdvisorResourceDetail]
tacrFlaggedResources =
    lens _tacrFlaggedResources (\s a -> s { _tacrFlaggedResources = a })
        . _List

tacrResourcesSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorResourcesSummary
tacrResourcesSummary =
    lens _tacrResourcesSummary (\s a -> s { _tacrResourcesSummary = a })

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error"
-- (red), or "not_available".
--
tacrStatus :: Lens' TrustedAdvisorCheckResult Text
tacrStatus = lens _tacrStatus (\s a -> s { _tacrStatus = a })

-- | The time of the last refresh of the check.
--
tacrTimestamp :: Lens' TrustedAdvisorCheckResult Text
tacrTimestamp = lens _tacrTimestamp (\s a -> s { _tacrTimestamp = a })

instance FromJSON TrustedAdvisorCheckResult where
    parseJSON = withObject "TrustedAdvisorCheckResult" $ \o -> TrustedAdvisorCheckResult
        <$> o .:  "categorySpecificSummary"
        <*> o .:  "checkId"
        <*> o .:  "flaggedResources"
        <*> o .:  "resourcesSummary"
        <*> o .:  "status"
        <*> o .:  "timestamp"

instance ToJSON TrustedAdvisorCheckResult where
    toJSON TrustedAdvisorCheckResult{..} = object
        [ "checkId"                 .= _tacrCheckId
        , "timestamp"               .= _tacrTimestamp
        , "status"                  .= _tacrStatus
        , "resourcesSummary"        .= _tacrResourcesSummary
        , "categorySpecificSummary" .= _tacrCategorySpecificSummary
        , "flaggedResources"        .= _tacrFlaggedResources
        ]

data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription
    { _tacdCategory    :: Text
    , _tacdDescription :: Text
    , _tacdId          :: Text
    , _tacdMetadata    :: List "checkIds" Text
    , _tacdName        :: Text
    } deriving (Eq, Ord, Show)

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
--
tacdCategory :: Lens' TrustedAdvisorCheckDescription Text
tacdCategory = lens _tacdCategory (\s a -> s { _tacdCategory = a })

-- | The description of the Trusted Advisor check, which includes the alert
-- criteria and recommended actions (contains HTML markup).
--
tacdDescription :: Lens' TrustedAdvisorCheckDescription Text
tacdDescription = lens _tacdDescription (\s a -> s { _tacdDescription = a })

-- | The unique identifier for the Trusted Advisor check.
--
tacdId :: Lens' TrustedAdvisorCheckDescription Text
tacdId = lens _tacdId (\s a -> s { _tacdId = a })

-- | The column headings for the data returned by the Trusted Advisor check. The
-- order of the headings corresponds to the order of the data in the Metadata
-- element of the 'TrustedAdvisorResourceDetail' for the check. Metadata contains
-- all the data that is shown in the Excel download, even in those cases where
-- the UI shows just summary data.
--
tacdMetadata :: Lens' TrustedAdvisorCheckDescription [Text]
tacdMetadata = lens _tacdMetadata (\s a -> s { _tacdMetadata = a }) . _List

-- | The display name for the Trusted Advisor check.
--
tacdName :: Lens' TrustedAdvisorCheckDescription Text
tacdName = lens _tacdName (\s a -> s { _tacdName = a })

instance FromJSON TrustedAdvisorCheckDescription where
    parseJSON = withObject "TrustedAdvisorCheckDescription" $ \o -> TrustedAdvisorCheckDescription
        <$> o .:  "category"
        <*> o .:  "description"
        <*> o .:  "id"
        <*> o .:  "metadata"
        <*> o .:  "name"

instance ToJSON TrustedAdvisorCheckDescription where
    toJSON TrustedAdvisorCheckDescription{..} = object
        [ "id"          .= _tacdId
        , "name"        .= _tacdName
        , "description" .= _tacdDescription
        , "category"    .= _tacdCategory
        , "metadata"    .= _tacdMetadata
        ]

data Attachment = Attachment
    { _aData     :: Maybe Base64
    , _aFileName :: Maybe Text
    } deriving (Eq, Show)

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
--
aData :: Lens' Attachment (Maybe Base64)
aData = lens _aData (\s a -> s { _aData = a })

-- | The name of the attachment file.
--
aFileName :: Lens' Attachment (Maybe Text)
aFileName = lens _aFileName (\s a -> s { _aFileName = a })

instance FromJSON Attachment where
    parseJSON = withObject "Attachment" $ \o -> Attachment
        <$> o .:? "data"
        <*> o .:? "fileName"

instance ToJSON Attachment where
    toJSON Attachment{..} = object
        [ "fileName" .= _aFileName
        , "data"     .= _aData
        ]

data RecentCaseCommunications = RecentCaseCommunications
    { _rccCommunications :: List "communications" Communication
    , _rccNextToken      :: Maybe Text
    } deriving (Eq, Show)

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
--
rccCommunications :: Lens' RecentCaseCommunications [Communication]
rccCommunications =
    lens _rccCommunications (\s a -> s { _rccCommunications = a })
        . _List

-- | A resumption point for pagination.
--
rccNextToken :: Lens' RecentCaseCommunications (Maybe Text)
rccNextToken = lens _rccNextToken (\s a -> s { _rccNextToken = a })

instance FromJSON RecentCaseCommunications where
    parseJSON = withObject "RecentCaseCommunications" $ \o -> RecentCaseCommunications
        <$> o .:  "communications"
        <*> o .:? "nextToken"

instance ToJSON RecentCaseCommunications where
    toJSON RecentCaseCommunications{..} = object
        [ "communications" .= _rccCommunications
        , "nextToken"      .= _rccNextToken
        ]

data SupportService = SupportService
    { _ssCategories :: List "categories" Category
    , _ssCode       :: Maybe Text
    , _ssName       :: Maybe Text
    } deriving (Eq, Show)

-- | 'SupportService' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssCategories' @::@ ['Category']
--
-- * 'ssCode' @::@ 'Maybe' 'Text'
--
-- * 'ssName' @::@ 'Maybe' 'Text'
--
supportService :: SupportService
supportService = SupportService
    { _ssCode       = Nothing
    , _ssName       = Nothing
    , _ssCategories = mempty
    }

-- | A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to AWS Support when you call 'CreateCase'.
--
ssCategories :: Lens' SupportService [Category]
ssCategories = lens _ssCategories (\s a -> s { _ssCategories = a }) . _List

-- | The code for an AWS service returned by the 'DescribeServices' response. The 'Name' element contains the corresponding friendly name.
--
ssCode :: Lens' SupportService (Maybe Text)
ssCode = lens _ssCode (\s a -> s { _ssCode = a })

-- | The friendly name for an AWS service. The 'Code' element contains the
-- corresponding code.
--
ssName :: Lens' SupportService (Maybe Text)
ssName = lens _ssName (\s a -> s { _ssName = a })

instance FromJSON SupportService where
    parseJSON = withObject "SupportService" $ \o -> SupportService
        <$> o .:  "categories"
        <*> o .:? "code"
        <*> o .:? "name"

instance ToJSON SupportService where
    toJSON SupportService{..} = object
        [ "code"       .= _ssCode
        , "name"       .= _ssName
        , "categories" .= _ssCategories
        ]

data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail
    { _tardIsSuppressed :: Maybe Bool
    , _tardMetadata     :: List "checkIds" Text
    , _tardRegion       :: Text
    , _tardResourceId   :: Text
    , _tardStatus       :: Text
    } deriving (Eq, Ord, Show)

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

-- | Specifies whether the AWS resource was ignored by Trusted Advisor because it
-- was marked as suppressed by the user.
--
tardIsSuppressed :: Lens' TrustedAdvisorResourceDetail (Maybe Bool)
tardIsSuppressed = lens _tardIsSuppressed (\s a -> s { _tardIsSuppressed = a })

-- | Additional information about the identified resource. The exact metadata and
-- its order can be obtained by inspecting the 'TrustedAdvisorCheckDescription'
-- object returned by the call to 'DescribeTrustedAdvisorChecks'. Metadata
-- contains all the data that is shown in the Excel download, even in those
-- cases where the UI shows just summary data.
--
tardMetadata :: Lens' TrustedAdvisorResourceDetail [Text]
tardMetadata = lens _tardMetadata (\s a -> s { _tardMetadata = a }) . _List

-- | The AWS region in which the identified resource is located.
--
tardRegion :: Lens' TrustedAdvisorResourceDetail Text
tardRegion = lens _tardRegion (\s a -> s { _tardRegion = a })

-- | The unique identifier for the identified resource.
--
tardResourceId :: Lens' TrustedAdvisorResourceDetail Text
tardResourceId = lens _tardResourceId (\s a -> s { _tardResourceId = a })

-- | The status code for the resource identified in the Trusted Advisor check.
--
tardStatus :: Lens' TrustedAdvisorResourceDetail Text
tardStatus = lens _tardStatus (\s a -> s { _tardStatus = a })

instance FromJSON TrustedAdvisorResourceDetail where
    parseJSON = withObject "TrustedAdvisorResourceDetail" $ \o -> TrustedAdvisorResourceDetail
        <$> o .:? "isSuppressed"
        <*> o .:  "metadata"
        <*> o .:  "region"
        <*> o .:  "resourceId"
        <*> o .:  "status"

instance ToJSON TrustedAdvisorResourceDetail where
    toJSON TrustedAdvisorResourceDetail{..} = object
        [ "status"       .= _tardStatus
        , "region"       .= _tardRegion
        , "resourceId"   .= _tardResourceId
        , "isSuppressed" .= _tardIsSuppressed
        , "metadata"     .= _tardMetadata
        ]

data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary
    { _tacosEstimatedMonthlySavings        :: Double
    , _tacosEstimatedPercentMonthlySavings :: Double
    } deriving (Eq, Ord, Show)

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
--
tacosEstimatedMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary Double
tacosEstimatedMonthlySavings =
    lens _tacosEstimatedMonthlySavings
        (\s a -> s { _tacosEstimatedMonthlySavings = a })

-- | The estimated percentage of savings that might be realized if the recommended
-- actions are taken.
--
tacosEstimatedPercentMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary Double
tacosEstimatedPercentMonthlySavings =
    lens _tacosEstimatedPercentMonthlySavings
        (\s a -> s { _tacosEstimatedPercentMonthlySavings = a })

instance FromJSON TrustedAdvisorCostOptimizingSummary where
    parseJSON = withObject "TrustedAdvisorCostOptimizingSummary" $ \o -> TrustedAdvisorCostOptimizingSummary
        <$> o .:  "estimatedMonthlySavings"
        <*> o .:  "estimatedPercentMonthlySavings"

instance ToJSON TrustedAdvisorCostOptimizingSummary where
    toJSON TrustedAdvisorCostOptimizingSummary{..} = object
        [ "estimatedMonthlySavings"        .= _tacosEstimatedMonthlySavings
        , "estimatedPercentMonthlySavings" .= _tacosEstimatedPercentMonthlySavings
        ]

data SeverityLevel = SeverityLevel
    { _slCode :: Maybe Text
    , _slName :: Maybe Text
    } deriving (Eq, Ord, Show)

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
-- correspond to response times returned to the caller in 'SeverityLevel.name'.
--
slCode :: Lens' SeverityLevel (Maybe Text)
slCode = lens _slCode (\s a -> s { _slCode = a })

-- | The name of the severity level that corresponds to the severity level code.
--
slName :: Lens' SeverityLevel (Maybe Text)
slName = lens _slName (\s a -> s { _slName = a })

instance FromJSON SeverityLevel where
    parseJSON = withObject "SeverityLevel" $ \o -> SeverityLevel
        <$> o .:? "code"
        <*> o .:? "name"

instance ToJSON SeverityLevel where
    toJSON SeverityLevel{..} = object
        [ "code" .= _slCode
        , "name" .= _slName
        ]

data CaseDetails = CaseDetails
    { _cdCaseId               :: Maybe Text
    , _cdCategoryCode         :: Maybe Text
    , _cdCcEmailAddresses     :: List "ccEmailAddresses" Text
    , _cdDisplayId            :: Maybe Text
    , _cdLanguage             :: Maybe Text
    , _cdRecentCommunications :: Maybe RecentCaseCommunications
    , _cdServiceCode          :: Maybe Text
    , _cdSeverityCode         :: Maybe Text
    , _cdStatus               :: Maybe Text
    , _cdSubject              :: Maybe Text
    , _cdSubmittedBy          :: Maybe Text
    , _cdTimeCreated          :: Maybe Text
    } deriving (Eq, Show)

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

-- | The AWS Support case ID requested or returned in the call. The case ID is an
-- alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
cdCaseId :: Lens' CaseDetails (Maybe Text)
cdCaseId = lens _cdCaseId (\s a -> s { _cdCaseId = a })

-- | The category of problem for the AWS Support case.
--
cdCategoryCode :: Lens' CaseDetails (Maybe Text)
cdCategoryCode = lens _cdCategoryCode (\s a -> s { _cdCategoryCode = a })

-- | The email addresses that receive copies of communication about the case.
--
cdCcEmailAddresses :: Lens' CaseDetails [Text]
cdCcEmailAddresses =
    lens _cdCcEmailAddresses (\s a -> s { _cdCcEmailAddresses = a })
        . _List

-- | The ID displayed for the case in the AWS Support Center. This is a numeric
-- string.
--
cdDisplayId :: Lens' CaseDetails (Maybe Text)
cdDisplayId = lens _cdDisplayId (\s a -> s { _cdDisplayId = a })

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
--
cdLanguage :: Lens' CaseDetails (Maybe Text)
cdLanguage = lens _cdLanguage (\s a -> s { _cdLanguage = a })

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also includes a 'nextToken' that you can use to retrieve earlier communications.
--
cdRecentCommunications :: Lens' CaseDetails (Maybe RecentCaseCommunications)
cdRecentCommunications =
    lens _cdRecentCommunications (\s a -> s { _cdRecentCommunications = a })

-- | The code for the AWS service returned by the call to 'DescribeServices'.
--
cdServiceCode :: Lens' CaseDetails (Maybe Text)
cdServiceCode = lens _cdServiceCode (\s a -> s { _cdServiceCode = a })

-- | The code for the severity level returned by the call to 'DescribeSeverityLevels'
-- .
--
cdSeverityCode :: Lens' CaseDetails (Maybe Text)
cdSeverityCode = lens _cdSeverityCode (\s a -> s { _cdSeverityCode = a })

-- | The status of the case.
--
cdStatus :: Lens' CaseDetails (Maybe Text)
cdStatus = lens _cdStatus (\s a -> s { _cdStatus = a })

-- | The subject line for the case in the AWS Support Center.
--
cdSubject :: Lens' CaseDetails (Maybe Text)
cdSubject = lens _cdSubject (\s a -> s { _cdSubject = a })

-- | The email address of the account that submitted the case.
--
cdSubmittedBy :: Lens' CaseDetails (Maybe Text)
cdSubmittedBy = lens _cdSubmittedBy (\s a -> s { _cdSubmittedBy = a })

-- | The time that the case was case created in the AWS Support Center.
--
cdTimeCreated :: Lens' CaseDetails (Maybe Text)
cdTimeCreated = lens _cdTimeCreated (\s a -> s { _cdTimeCreated = a })

instance FromJSON CaseDetails where
    parseJSON = withObject "CaseDetails" $ \o -> CaseDetails
        <$> o .:? "caseId"
        <*> o .:? "categoryCode"
        <*> o .:  "ccEmailAddresses"
        <*> o .:? "displayId"
        <*> o .:? "language"
        <*> o .:? "recentCommunications"
        <*> o .:? "serviceCode"
        <*> o .:? "severityCode"
        <*> o .:? "status"
        <*> o .:? "subject"
        <*> o .:? "submittedBy"
        <*> o .:? "timeCreated"

instance ToJSON CaseDetails where
    toJSON CaseDetails{..} = object
        [ "caseId"               .= _cdCaseId
        , "displayId"            .= _cdDisplayId
        , "subject"              .= _cdSubject
        , "status"               .= _cdStatus
        , "serviceCode"          .= _cdServiceCode
        , "categoryCode"         .= _cdCategoryCode
        , "severityCode"         .= _cdSeverityCode
        , "submittedBy"          .= _cdSubmittedBy
        , "timeCreated"          .= _cdTimeCreated
        , "recentCommunications" .= _cdRecentCommunications
        , "ccEmailAddresses"     .= _cdCcEmailAddresses
        , "language"             .= _cdLanguage
        ]

data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus
    { _tacrsCheckId                    :: Text
    , _tacrsMillisUntilNextRefreshable :: Integer
    , _tacrsStatus                     :: Text
    } deriving (Eq, Ord, Show)

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
--
tacrsCheckId :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsCheckId = lens _tacrsCheckId (\s a -> s { _tacrsCheckId = a })

-- | The amount of time, in milliseconds, until the Trusted Advisor check is
-- eligible for refresh.
--
tacrsMillisUntilNextRefreshable :: Lens' TrustedAdvisorCheckRefreshStatus Integer
tacrsMillisUntilNextRefreshable =
    lens _tacrsMillisUntilNextRefreshable
        (\s a -> s { _tacrsMillisUntilNextRefreshable = a })

-- | The status of the Trusted Advisor check for which a refresh has been
-- requested: "none", "enqueued", "processing", "success", or "abandoned".
--
tacrsStatus :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsStatus = lens _tacrsStatus (\s a -> s { _tacrsStatus = a })

instance FromJSON TrustedAdvisorCheckRefreshStatus where
    parseJSON = withObject "TrustedAdvisorCheckRefreshStatus" $ \o -> TrustedAdvisorCheckRefreshStatus
        <$> o .:  "checkId"
        <*> o .:  "millisUntilNextRefreshable"
        <*> o .:  "status"

instance ToJSON TrustedAdvisorCheckRefreshStatus where
    toJSON TrustedAdvisorCheckRefreshStatus{..} = object
        [ "checkId"                    .= _tacrsCheckId
        , "status"                     .= _tacrsStatus
        , "millisUntilNextRefreshable" .= _tacrsMillisUntilNextRefreshable
        ]
