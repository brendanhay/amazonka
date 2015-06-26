{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Support.CreateCase
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new case in the AWS Support Center. This operation is modeled
-- on the behavior of the AWS Support Center
-- <https://console.aws.amazon.com/support/home#/case/create Create Case>
-- page. Its parameters require you to specify the following information:
--
-- 1.  __IssueType.__ The type of issue for the case. You can specify
--     either \"customer-service\" or \"technical.\" If you do not indicate
--     a value, the default is \"technical.\"
-- 2.  __ServiceCode.__ The code for an AWS service. You obtain the
--     @ServiceCode@ by calling DescribeServices.
-- 3.  __CategoryCode.__ The category for the service defined for the
--     @ServiceCode@ value. You also obtain the category code for a service
--     by calling DescribeServices. Each AWS service defines its own set of
--     category codes.
-- 4.  __SeverityCode.__ A value that indicates the urgency of the case,
--     which in turn determines the response time according to your service
--     level agreement with AWS Support. You obtain the SeverityCode by
--     calling DescribeSeverityLevels.
-- 5.  __Subject.__ The __Subject__ field on the AWS Support Center
--     <https://console.aws.amazon.com/support/home#/case/create Create Case>
--     page.
-- 6.  __CommunicationBody.__ The __Description__ field on the AWS Support
--     Center
--     <https://console.aws.amazon.com/support/home#/case/create Create Case>
--     page.
-- 7.  __AttachmentSetId.__ The ID of a set of attachments that has been
--     created by using AddAttachmentsToSet.
-- 8.  __Language.__ The human language in which AWS Support handles the
--     case. English and Japanese are currently supported.
-- 9.  __CcEmailAddresses.__ The AWS Support Center __CC__ field on the
--     <https://console.aws.amazon.com/support/home#/case/create Create Case>
--     page. You can list email addresses to be copied on any
--     correspondence about the case. The account that opens the case is
--     already identified by passing the AWS Credentials in the HTTP POST
--     method or in a method or function call from one of the programming
--     languages supported by an <http://aws.amazon.com/tools/ AWS SDK>.
--
-- To add additional communication or attachments to an existing case, use
-- AddCommunicationToCase.
--
-- A successful CreateCase request returns an AWS Support case number. Case
-- numbers are used by the DescribeCases operation to retrieve existing AWS
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
    , ccSeverityCode
    , ccIssueType
    , ccCcEmailAddresses
    , ccLanguage
    , ccCategoryCode
    , ccServiceCode
    , ccAttachmentSetId
    , ccSubject
    , ccCommunicationBody

    -- * Response
    , CreateCaseResponse
    -- ** Response constructor
    , createCaseResponse
    -- ** Response lenses
    , ccrCaseId
    , ccrStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types

-- | /See:/ 'createCase' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccSeverityCode'
--
-- * 'ccIssueType'
--
-- * 'ccCcEmailAddresses'
--
-- * 'ccLanguage'
--
-- * 'ccCategoryCode'
--
-- * 'ccServiceCode'
--
-- * 'ccAttachmentSetId'
--
-- * 'ccSubject'
--
-- * 'ccCommunicationBody'
data CreateCase = CreateCase'{_ccSeverityCode :: Maybe Text, _ccIssueType :: Maybe Text, _ccCcEmailAddresses :: Maybe [Text], _ccLanguage :: Maybe Text, _ccCategoryCode :: Maybe Text, _ccServiceCode :: Maybe Text, _ccAttachmentSetId :: Maybe Text, _ccSubject :: Text, _ccCommunicationBody :: Text} deriving (Eq, Read, Show)

-- | 'CreateCase' smart constructor.
createCase :: Text -> Text -> CreateCase
createCase pSubject pCommunicationBody = CreateCase'{_ccSeverityCode = Nothing, _ccIssueType = Nothing, _ccCcEmailAddresses = Nothing, _ccLanguage = Nothing, _ccCategoryCode = Nothing, _ccServiceCode = Nothing, _ccAttachmentSetId = Nothing, _ccSubject = pSubject, _ccCommunicationBody = pCommunicationBody};

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
--
-- The availability of severity levels depends on each customer\'s support
-- subscription. In other words, your subscription may not necessarily
-- require the urgent level of response time.
ccSeverityCode :: Lens' CreateCase (Maybe Text)
ccSeverityCode = lens _ccSeverityCode (\ s a -> s{_ccSeverityCode = a});

-- | The type of issue for the case. You can specify either
-- \"customer-service\" or \"technical.\" If you do not indicate a value,
-- the default is \"technical.\"
ccIssueType :: Lens' CreateCase (Maybe Text)
ccIssueType = lens _ccIssueType (\ s a -> s{_ccIssueType = a});

-- | A list of email addresses that AWS Support copies on case
-- correspondence.
ccCcEmailAddresses :: Lens' CreateCase [Text]
ccCcEmailAddresses = lens _ccCcEmailAddresses (\ s a -> s{_ccCcEmailAddresses = a}) . _Default;

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
ccLanguage :: Lens' CreateCase (Maybe Text)
ccLanguage = lens _ccLanguage (\ s a -> s{_ccLanguage = a});

-- | The category of problem for the AWS Support case.
ccCategoryCode :: Lens' CreateCase (Maybe Text)
ccCategoryCode = lens _ccCategoryCode (\ s a -> s{_ccCategoryCode = a});

-- | The code for the AWS service returned by the call to DescribeServices.
ccServiceCode :: Lens' CreateCase (Maybe Text)
ccServiceCode = lens _ccServiceCode (\ s a -> s{_ccServiceCode = a});

-- | The ID of a set of one or more attachments for the case. Create the set
-- by using AddAttachmentsToSet.
ccAttachmentSetId :: Lens' CreateCase (Maybe Text)
ccAttachmentSetId = lens _ccAttachmentSetId (\ s a -> s{_ccAttachmentSetId = a});

-- | The title of the AWS Support case.
ccSubject :: Lens' CreateCase Text
ccSubject = lens _ccSubject (\ s a -> s{_ccSubject = a});

-- | The communication body text when you create an AWS Support case by
-- calling CreateCase.
ccCommunicationBody :: Lens' CreateCase Text
ccCommunicationBody = lens _ccCommunicationBody (\ s a -> s{_ccCommunicationBody = a});

instance AWSRequest CreateCase where
        type Sv CreateCase = Support
        type Rs CreateCase = CreateCaseResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateCaseResponse' <$>
                   (x .?> "caseId") <*> (pure (fromEnum s)))

instance ToHeaders CreateCase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.CreateCase" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCase where
        toJSON CreateCase'{..}
          = object
              ["severityCode" .= _ccSeverityCode,
               "issueType" .= _ccIssueType,
               "ccEmailAddresses" .= _ccCcEmailAddresses,
               "language" .= _ccLanguage,
               "categoryCode" .= _ccCategoryCode,
               "serviceCode" .= _ccServiceCode,
               "attachmentSetId" .= _ccAttachmentSetId,
               "subject" .= _ccSubject,
               "communicationBody" .= _ccCommunicationBody]

instance ToPath CreateCase where
        toPath = const "/"

instance ToQuery CreateCase where
        toQuery = const mempty

-- | The AWS Support case ID returned by a successful completion of the
-- CreateCase operation.
--
-- /See:/ 'createCaseResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrCaseId'
--
-- * 'ccrStatusCode'
data CreateCaseResponse = CreateCaseResponse'{_ccrCaseId :: Maybe Text, _ccrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'CreateCaseResponse' smart constructor.
createCaseResponse :: Int -> CreateCaseResponse
createCaseResponse pStatusCode = CreateCaseResponse'{_ccrCaseId = Nothing, _ccrStatusCode = pStatusCode};

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
ccrCaseId :: Lens' CreateCaseResponse (Maybe Text)
ccrCaseId = lens _ccrCaseId (\ s a -> s{_ccrCaseId = a});

-- | FIXME: Undocumented member.
ccrStatusCode :: Lens' CreateCaseResponse Int
ccrStatusCode = lens _ccrStatusCode (\ s a -> s{_ccrStatusCode = a});
