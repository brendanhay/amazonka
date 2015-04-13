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

-- Module      : Network.AWS.Support.CreateCase
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new case in the AWS Support Center. This operation is modeled on
-- the behavior of the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page. Its parameters
-- require you to specify the following information:
--
-- IssueType. The type of issue for the case. You can specify either
-- "customer-service" or "technical." If you do not indicate a value, the
-- default is "technical."   ServiceCode. The code for an AWS service. You
-- obtain the 'ServiceCode' by calling 'DescribeServices'.   CategoryCode. The
-- category for the service defined for the 'ServiceCode' value. You also obtain
-- the category code for a service by calling 'DescribeServices'. Each AWS service
-- defines its own set of category codes.   SeverityCode. A value that indicates
-- the urgency of the case, which in turn determines the response time according
-- to your service level agreement with AWS Support. You obtain the SeverityCode
-- by calling 'DescribeSeverityLevels'.  Subject. The Subject field on the AWS
-- Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.  CommunicationBody. The Description field on
-- the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.  AttachmentSetId. The ID of a set of
-- attachments that has been created by using 'AddAttachmentsToSet'.  Language.
-- The human language in which AWS Support handles the case. English and
-- Japanese are currently supported.  CcEmailAddresses. The AWS Support Center CC
-- field on the <https://console.aws.amazon.com/support/home#/case/create Create Case> page. You can list email addresses to be copied on
-- any correspondence about the case. The account that opens the case is already
-- identified by passing the AWS Credentials in the HTTP POST method or in a
-- method or function call from one of the programming languages supported by an <http://aws.amazon.com/tools/ AWS SDK>.   To add additional communication or attachments to an existing
-- case, use 'AddCommunicationToCase'.
--
-- A successful 'CreateCase' request returns an AWS Support case number. Case
-- numbers are used by the 'DescribeCases' operation to retrieve existing AWS
-- Support cases.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_CreateCase.html>
module Network.AWS.Support.CreateCase
    (
    -- * Request
      CreateCase
    -- ** Request constructor
    , createCase
    -- ** Request lenses
    , ccAttachmentSetId
    , ccCategoryCode
    , ccCcEmailAddresses
    , ccCommunicationBody
    , ccIssueType
    , ccLanguage
    , ccServiceCode
    , ccSeverityCode
    , ccSubject

    -- * Response
    , CreateCaseResponse
    -- ** Response constructor
    , createCaseResponse
    -- ** Response lenses
    , ccrCaseId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Support.Types
import qualified GHC.Exts

data CreateCase = CreateCase
    { _ccAttachmentSetId   :: Maybe Text
    , _ccCategoryCode      :: Maybe Text
    , _ccCcEmailAddresses  :: List "ccEmailAddresses" Text
    , _ccCommunicationBody :: Text
    , _ccIssueType         :: Maybe Text
    , _ccLanguage          :: Maybe Text
    , _ccServiceCode       :: Maybe Text
    , _ccSeverityCode      :: Maybe Text
    , _ccSubject           :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateCase' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccAttachmentSetId' @::@ 'Maybe' 'Text'
--
-- * 'ccCategoryCode' @::@ 'Maybe' 'Text'
--
-- * 'ccCcEmailAddresses' @::@ ['Text']
--
-- * 'ccCommunicationBody' @::@ 'Text'
--
-- * 'ccIssueType' @::@ 'Maybe' 'Text'
--
-- * 'ccLanguage' @::@ 'Maybe' 'Text'
--
-- * 'ccServiceCode' @::@ 'Maybe' 'Text'
--
-- * 'ccSeverityCode' @::@ 'Maybe' 'Text'
--
-- * 'ccSubject' @::@ 'Text'
--
createCase :: Text -- ^ 'ccSubject'
           -> Text -- ^ 'ccCommunicationBody'
           -> CreateCase
createCase p1 p2 = CreateCase
    { _ccSubject           = p1
    , _ccCommunicationBody = p2
    , _ccServiceCode       = Nothing
    , _ccSeverityCode      = Nothing
    , _ccCategoryCode      = Nothing
    , _ccCcEmailAddresses  = mempty
    , _ccLanguage          = Nothing
    , _ccIssueType         = Nothing
    , _ccAttachmentSetId   = Nothing
    }

-- | The ID of a set of one or more attachments for the case. Create the set by
-- using 'AddAttachmentsToSet'.
ccAttachmentSetId :: Lens' CreateCase (Maybe Text)
ccAttachmentSetId =
    lens _ccAttachmentSetId (\s a -> s { _ccAttachmentSetId = a })

-- | The category of problem for the AWS Support case.
ccCategoryCode :: Lens' CreateCase (Maybe Text)
ccCategoryCode = lens _ccCategoryCode (\s a -> s { _ccCategoryCode = a })

-- | A list of email addresses that AWS Support copies on case correspondence.
ccCcEmailAddresses :: Lens' CreateCase [Text]
ccCcEmailAddresses =
    lens _ccCcEmailAddresses (\s a -> s { _ccCcEmailAddresses = a })
        . _List

-- | The communication body text when you create an AWS Support case by calling 'CreateCase'.
ccCommunicationBody :: Lens' CreateCase Text
ccCommunicationBody =
    lens _ccCommunicationBody (\s a -> s { _ccCommunicationBody = a })

-- | The type of issue for the case. You can specify either "customer-service" or
-- "technical." If you do not indicate a value, the default is "technical."
ccIssueType :: Lens' CreateCase (Maybe Text)
ccIssueType = lens _ccIssueType (\s a -> s { _ccIssueType = a })

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
ccLanguage :: Lens' CreateCase (Maybe Text)
ccLanguage = lens _ccLanguage (\s a -> s { _ccLanguage = a })

-- | The code for the AWS service returned by the call to 'DescribeServices'.
ccServiceCode :: Lens' CreateCase (Maybe Text)
ccServiceCode = lens _ccServiceCode (\s a -> s { _ccServiceCode = a })

-- | The code for the severity level returned by the call to 'DescribeSeverityLevels'
-- .
--
-- The availability of severity levels depends on each customer's support
-- subscription. In other words, your subscription may not necessarily require
-- the urgent level of response time.
--
ccSeverityCode :: Lens' CreateCase (Maybe Text)
ccSeverityCode = lens _ccSeverityCode (\s a -> s { _ccSeverityCode = a })

-- | The title of the AWS Support case.
ccSubject :: Lens' CreateCase Text
ccSubject = lens _ccSubject (\s a -> s { _ccSubject = a })

newtype CreateCaseResponse = CreateCaseResponse
    { _ccrCaseId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateCaseResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrCaseId' @::@ 'Maybe' 'Text'
--
createCaseResponse :: CreateCaseResponse
createCaseResponse = CreateCaseResponse
    { _ccrCaseId = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is an
-- alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
ccrCaseId :: Lens' CreateCaseResponse (Maybe Text)
ccrCaseId = lens _ccrCaseId (\s a -> s { _ccrCaseId = a })

instance ToPath CreateCase where
    toPath = const "/"

instance ToQuery CreateCase where
    toQuery = const mempty

instance ToHeaders CreateCase

instance ToJSON CreateCase where
    toJSON CreateCase{..} = object
        [ "subject"           .= _ccSubject
        , "serviceCode"       .= _ccServiceCode
        , "severityCode"      .= _ccSeverityCode
        , "categoryCode"      .= _ccCategoryCode
        , "communicationBody" .= _ccCommunicationBody
        , "ccEmailAddresses"  .= _ccCcEmailAddresses
        , "language"          .= _ccLanguage
        , "issueType"         .= _ccIssueType
        , "attachmentSetId"   .= _ccAttachmentSetId
        ]

instance AWSRequest CreateCase where
    type Sv CreateCase = Support
    type Rs CreateCase = CreateCaseResponse

    request  = post "CreateCase"
    response = jsonResponse

instance FromJSON CreateCaseResponse where
    parseJSON = withObject "CreateCaseResponse" $ \o -> CreateCaseResponse
        <$> o .:? "caseId"
