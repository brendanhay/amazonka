{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.CreateCase
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new case in the AWS Support Center. This operation is modeled on
-- the behavior of the AWS Support Center Open a new case page. Its parameters
-- require you to specify the following information: IssueType. The type of
-- issue for the case. You can specify either "customer-service" or
-- "technical." If you do not indicate a value, the default is "technical."
-- ServiceCode. The code for an AWS service. You obtain the ServiceCode by
-- calling DescribeServices. CategoryCode. The category for the service
-- defined for the ServiceCode value. You also obtain the category code for a
-- service by calling DescribeServices. Each AWS service defines its own set
-- of category codes. SeverityCode. A value that indicates the urgency of the
-- case, which in turn determines the response time according to your service
-- level agreement with AWS Support. You obtain the SeverityCode by calling
-- DescribeSeverityLevels. Subject. The Subject field on the AWS Support
-- Center Open a new case page. CommunicationBody. The Description field on
-- the AWS Support Center Open a new case page. AttachmentSetId. The ID of a
-- set of attachments that has been created by using AddAttachmentsToSet.
-- Language. The human language in which AWS Support handles the case. English
-- and Japanese are currently supported. CcEmailAddresses. The AWS Support
-- Center CC field on the Open a new case page. You can list email addresses
-- to be copied on any correspondence about the case. The account that opens
-- the case is already identified by passing the AWS Credentials in the HTTP
-- POST method or in a method or function call from one of the programming
-- languages supported by an AWS SDK. To add additional communication or
-- attachments to an existing case, use AddCommunicationToCase. A successful
-- CreateCase request returns an AWS Support case number. Case numbers are
-- used by the DescribeCases operation to retrieve existing AWS Support cases.
module Network.AWS.Support.V2013_04_15.CreateCase
    (
    -- * Request
      CreateCase
    -- ** Request constructor
    , createCase
    -- ** Request lenses
    , ccrCommunicationBody
    , ccrSubject
    , ccrAttachmentSetId
    , ccrCategoryCode
    , ccrCcEmailAddresses
    , ccrIssueType
    , ccrLanguage
    , ccrServiceCode
    , ccrSeverityCode

    -- * Response
    , CreateCaseResponse
    -- ** Response lenses
    , ccsCaseId
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateCase' request.
createCase :: Text -- ^ 'ccrCommunicationBody'
           -> Text -- ^ 'ccrSubject'
           -> CreateCase
createCase p1 p2 = CreateCase
    { _ccrCommunicationBody = p1
    , _ccrSubject = p2
    , _ccrAttachmentSetId = Nothing
    , _ccrCategoryCode = Nothing
    , _ccrCcEmailAddresses = mempty
    , _ccrIssueType = Nothing
    , _ccrLanguage = Nothing
    , _ccrServiceCode = Nothing
    , _ccrSeverityCode = Nothing
    }

data CreateCase = CreateCase
    { _ccrCommunicationBody :: Text
      -- ^ The communication body text when you create an AWS Support case
      -- by calling CreateCase.
    , _ccrSubject :: Text
      -- ^ The title of the AWS Support case.
    , _ccrAttachmentSetId :: Maybe Text
      -- ^ The ID of a set of one or more attachments for the case. Create
      -- the set by using AddAttachmentsToSet.
    , _ccrCategoryCode :: Maybe Text
      -- ^ The category of problem for the AWS Support case.
    , _ccrCcEmailAddresses :: [Text]
      -- ^ A list of email addresses that AWS Support copies on case
      -- correspondence.
    , _ccrIssueType :: Maybe Text
      -- ^ The type of issue for the case. You can specify either
      -- "customer-service" or "technical." If you do not indicate a
      -- value, the default is "technical.".
    , _ccrLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides
      -- support. AWS Support currently supports English ("en") and
      -- Japanese ("ja"). Language parameters must be passed explicitly
      -- for operations that take them.
    , _ccrServiceCode :: Maybe Text
      -- ^ The code for the AWS service returned by the call to
      -- DescribeServices.
    , _ccrSeverityCode :: Maybe Text
      -- ^ The code for the severity level returned by the call to
      -- DescribeSeverityLevels. The availability of severity levels
      -- depends on each customer's support subscription. In other words,
      -- your subscription may not necessarily require the urgent level of
      -- response time.
    } deriving (Show, Generic)

-- | The communication body text when you create an AWS Support case by calling
-- CreateCase.
ccrCommunicationBody
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateCase
    -> f CreateCase
ccrCommunicationBody f x =
    (\y -> x { _ccrCommunicationBody = y })
       <$> f (_ccrCommunicationBody x)
{-# INLINE ccrCommunicationBody #-}

-- | The title of the AWS Support case.
ccrSubject
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateCase
    -> f CreateCase
ccrSubject f x =
    (\y -> x { _ccrSubject = y })
       <$> f (_ccrSubject x)
{-# INLINE ccrSubject #-}

-- | The ID of a set of one or more attachments for the case. Create the set by
-- using AddAttachmentsToSet.
ccrAttachmentSetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateCase
    -> f CreateCase
ccrAttachmentSetId f x =
    (\y -> x { _ccrAttachmentSetId = y })
       <$> f (_ccrAttachmentSetId x)
{-# INLINE ccrAttachmentSetId #-}

-- | The category of problem for the AWS Support case.
ccrCategoryCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateCase
    -> f CreateCase
ccrCategoryCode f x =
    (\y -> x { _ccrCategoryCode = y })
       <$> f (_ccrCategoryCode x)
{-# INLINE ccrCategoryCode #-}

-- | A list of email addresses that AWS Support copies on case correspondence.
ccrCcEmailAddresses
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateCase
    -> f CreateCase
ccrCcEmailAddresses f x =
    (\y -> x { _ccrCcEmailAddresses = y })
       <$> f (_ccrCcEmailAddresses x)
{-# INLINE ccrCcEmailAddresses #-}

-- | The type of issue for the case. You can specify either "customer-service"
-- or "technical." If you do not indicate a value, the default is
-- "technical.".
ccrIssueType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateCase
    -> f CreateCase
ccrIssueType f x =
    (\y -> x { _ccrIssueType = y })
       <$> f (_ccrIssueType x)
{-# INLINE ccrIssueType #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
ccrLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateCase
    -> f CreateCase
ccrLanguage f x =
    (\y -> x { _ccrLanguage = y })
       <$> f (_ccrLanguage x)
{-# INLINE ccrLanguage #-}

-- | The code for the AWS service returned by the call to DescribeServices.
ccrServiceCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateCase
    -> f CreateCase
ccrServiceCode f x =
    (\y -> x { _ccrServiceCode = y })
       <$> f (_ccrServiceCode x)
{-# INLINE ccrServiceCode #-}

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels. The availability of severity levels depends on each
-- customer's support subscription. In other words, your subscription may not
-- necessarily require the urgent level of response time.
ccrSeverityCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateCase
    -> f CreateCase
ccrSeverityCode f x =
    (\y -> x { _ccrSeverityCode = y })
       <$> f (_ccrSeverityCode x)
{-# INLINE ccrSeverityCode #-}

instance ToPath CreateCase

instance ToQuery CreateCase

instance ToHeaders CreateCase

instance ToJSON CreateCase

data CreateCaseResponse = CreateCaseResponse
    { _ccsCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    } deriving (Show, Generic)

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
ccsCaseId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateCaseResponse
    -> f CreateCaseResponse
ccsCaseId f x =
    (\y -> x { _ccsCaseId = y })
       <$> f (_ccsCaseId x)
{-# INLINE ccsCaseId #-}

instance FromJSON CreateCaseResponse

instance AWSRequest CreateCase where
    type Sv CreateCase = Support
    type Rs CreateCase = CreateCaseResponse

    request = get
    response _ = jsonResponse
