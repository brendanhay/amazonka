{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.Support.CreateCase
    (
    -- * Request
      CreateCase
    -- ** Request constructor
    , createCase
    -- ** Request lenses
    , ccSubject
    , ccServiceCode
    , ccSeverityCode
    , ccCategoryCode
    , ccCommunicationBody
    , ccCcEmailAddresses
    , ccLanguage
    , ccIssueType
    , ccAttachmentSetId

    -- * Response
    , CreateCaseResponse
    -- ** Response constructor
    , createCaseResponse
    -- ** Response lenses
    , ccrCaseId
    ) where

import Network.AWS.Support.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CreateCase = CreateCase
    { _ccSubject :: Text
    , _ccServiceCode :: Maybe Text
    , _ccSeverityCode :: Maybe Text
    , _ccCategoryCode :: Maybe Text
    , _ccCommunicationBody :: Text
    , _ccCcEmailAddresses :: [Text]
    , _ccLanguage :: Maybe Text
    , _ccIssueType :: Maybe Text
    , _ccAttachmentSetId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCase' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Subject ::@ @Text@
--
-- * @ServiceCode ::@ @Maybe Text@
--
-- * @SeverityCode ::@ @Maybe Text@
--
-- * @CategoryCode ::@ @Maybe Text@
--
-- * @CommunicationBody ::@ @Text@
--
-- * @CcEmailAddresses ::@ @[Text]@
--
-- * @Language ::@ @Maybe Text@
--
-- * @IssueType ::@ @Maybe Text@
--
-- * @AttachmentSetId ::@ @Maybe Text@
--
createCase :: Text -- ^ 'ccSubject'
             -> Text -- ^ 'ccCommunicationBody'
             -> CreateCase
createCase p1 p5 = CreateCase
    { _ccSubject = p1
    , _ccServiceCode = Nothing
    , _ccSeverityCode = Nothing
    , _ccCategoryCode = Nothing
    , _ccCommunicationBody = p5
    , _ccCcEmailAddresses = mempty
    , _ccLanguage = Nothing
    , _ccIssueType = Nothing
    , _ccAttachmentSetId = Nothing
    }

-- | The title of the AWS Support case.
ccSubject :: Lens' CreateCase Text
ccSubject = lens _ccSubject (\s a -> s { _ccSubject = a })

-- | The code for the AWS service returned by the call to DescribeServices.
ccServiceCode :: Lens' CreateCase (Maybe Text)
ccServiceCode = lens _ccServiceCode (\s a -> s { _ccServiceCode = a })

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels. The availability of severity levels depends on each
-- customer's support subscription. In other words, your subscription may not
-- necessarily require the urgent level of response time.
ccSeverityCode :: Lens' CreateCase (Maybe Text)
ccSeverityCode = lens _ccSeverityCode (\s a -> s { _ccSeverityCode = a })

-- | The category of problem for the AWS Support case.
ccCategoryCode :: Lens' CreateCase (Maybe Text)
ccCategoryCode = lens _ccCategoryCode (\s a -> s { _ccCategoryCode = a })

-- | The communication body text when you create an AWS Support case by calling
-- CreateCase.
ccCommunicationBody :: Lens' CreateCase Text
ccCommunicationBody =
    lens _ccCommunicationBody (\s a -> s { _ccCommunicationBody = a })

-- | A list of email addresses that AWS Support copies on case correspondence.
ccCcEmailAddresses :: Lens' CreateCase [Text]
ccCcEmailAddresses =
    lens _ccCcEmailAddresses (\s a -> s { _ccCcEmailAddresses = a })

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
ccLanguage :: Lens' CreateCase (Maybe Text)
ccLanguage = lens _ccLanguage (\s a -> s { _ccLanguage = a })

-- | The type of issue for the case. You can specify either "customer-service"
-- or "technical." If you do not indicate a value, the default is
-- "technical.".
ccIssueType :: Lens' CreateCase (Maybe Text)
ccIssueType = lens _ccIssueType (\s a -> s { _ccIssueType = a })

-- | The ID of a set of one or more attachments for the case. Create the set by
-- using AddAttachmentsToSet.
ccAttachmentSetId :: Lens' CreateCase (Maybe Text)
ccAttachmentSetId =
    lens _ccAttachmentSetId (\s a -> s { _ccAttachmentSetId = a })

instance ToPath CreateCase

instance ToQuery CreateCase

instance ToHeaders CreateCase

instance ToJSON CreateCase

-- | The AWS Support case ID returned by a successful completion of the
-- CreateCase operation.
newtype CreateCaseResponse = CreateCaseResponse
    { _ccrCaseId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCaseResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CaseId ::@ @Maybe Text@
--
createCaseResponse :: CreateCaseResponse
createCaseResponse = CreateCaseResponse
    { _ccrCaseId = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
ccrCaseId :: Lens' CreateCaseResponse (Maybe Text)
ccrCaseId = lens _ccrCaseId (\s a -> s { _ccrCaseId = a })

instance FromJSON CreateCaseResponse

instance AWSRequest CreateCase where
    type Sv CreateCase = Support
    type Rs CreateCase = CreateCaseResponse

    request = get
    response _ = jsonResponse
