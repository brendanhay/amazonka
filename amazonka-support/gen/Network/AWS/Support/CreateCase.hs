{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.CreateCase
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new case in the AWS Support Center. This operation is modeled
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
    , ccrqSeverityCode
    , ccrqIssueType
    , ccrqCcEmailAddresses
    , ccrqLanguage
    , ccrqCategoryCode
    , ccrqServiceCode
    , ccrqAttachmentSetId
    , ccrqSubject
    , ccrqCommunicationBody

    -- * Response
    , CreateCaseResponse
    -- ** Response constructor
    , createCaseResponse
    -- ** Response lenses
    , ccrsCaseId
    , ccrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'createCase' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrqSeverityCode'
--
-- * 'ccrqIssueType'
--
-- * 'ccrqCcEmailAddresses'
--
-- * 'ccrqLanguage'
--
-- * 'ccrqCategoryCode'
--
-- * 'ccrqServiceCode'
--
-- * 'ccrqAttachmentSetId'
--
-- * 'ccrqSubject'
--
-- * 'ccrqCommunicationBody'
data CreateCase = CreateCase'
    { _ccrqSeverityCode      :: !(Maybe Text)
    , _ccrqIssueType         :: !(Maybe Text)
    , _ccrqCcEmailAddresses  :: !(Maybe [Text])
    , _ccrqLanguage          :: !(Maybe Text)
    , _ccrqCategoryCode      :: !(Maybe Text)
    , _ccrqServiceCode       :: !(Maybe Text)
    , _ccrqAttachmentSetId   :: !(Maybe Text)
    , _ccrqSubject           :: !Text
    , _ccrqCommunicationBody :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCase' smart constructor.
createCase :: Text -> Text -> CreateCase
createCase pSubject pCommunicationBody =
    CreateCase'
    { _ccrqSeverityCode = Nothing
    , _ccrqIssueType = Nothing
    , _ccrqCcEmailAddresses = Nothing
    , _ccrqLanguage = Nothing
    , _ccrqCategoryCode = Nothing
    , _ccrqServiceCode = Nothing
    , _ccrqAttachmentSetId = Nothing
    , _ccrqSubject = pSubject
    , _ccrqCommunicationBody = pCommunicationBody
    }

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
--
-- The availability of severity levels depends on each customer\'s support
-- subscription. In other words, your subscription may not necessarily
-- require the urgent level of response time.
ccrqSeverityCode :: Lens' CreateCase (Maybe Text)
ccrqSeverityCode = lens _ccrqSeverityCode (\ s a -> s{_ccrqSeverityCode = a});

-- | The type of issue for the case. You can specify either
-- \"customer-service\" or \"technical.\" If you do not indicate a value,
-- the default is \"technical.\"
ccrqIssueType :: Lens' CreateCase (Maybe Text)
ccrqIssueType = lens _ccrqIssueType (\ s a -> s{_ccrqIssueType = a});

-- | A list of email addresses that AWS Support copies on case
-- correspondence.
ccrqCcEmailAddresses :: Lens' CreateCase [Text]
ccrqCcEmailAddresses = lens _ccrqCcEmailAddresses (\ s a -> s{_ccrqCcEmailAddresses = a}) . _Default;

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
ccrqLanguage :: Lens' CreateCase (Maybe Text)
ccrqLanguage = lens _ccrqLanguage (\ s a -> s{_ccrqLanguage = a});

-- | The category of problem for the AWS Support case.
ccrqCategoryCode :: Lens' CreateCase (Maybe Text)
ccrqCategoryCode = lens _ccrqCategoryCode (\ s a -> s{_ccrqCategoryCode = a});

-- | The code for the AWS service returned by the call to DescribeServices.
ccrqServiceCode :: Lens' CreateCase (Maybe Text)
ccrqServiceCode = lens _ccrqServiceCode (\ s a -> s{_ccrqServiceCode = a});

-- | The ID of a set of one or more attachments for the case. Create the set
-- by using AddAttachmentsToSet.
ccrqAttachmentSetId :: Lens' CreateCase (Maybe Text)
ccrqAttachmentSetId = lens _ccrqAttachmentSetId (\ s a -> s{_ccrqAttachmentSetId = a});

-- | The title of the AWS Support case.
ccrqSubject :: Lens' CreateCase Text
ccrqSubject = lens _ccrqSubject (\ s a -> s{_ccrqSubject = a});

-- | The communication body text when you create an AWS Support case by
-- calling CreateCase.
ccrqCommunicationBody :: Lens' CreateCase Text
ccrqCommunicationBody = lens _ccrqCommunicationBody (\ s a -> s{_ccrqCommunicationBody = a});

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
              ["severityCode" .= _ccrqSeverityCode,
               "issueType" .= _ccrqIssueType,
               "ccEmailAddresses" .= _ccrqCcEmailAddresses,
               "language" .= _ccrqLanguage,
               "categoryCode" .= _ccrqCategoryCode,
               "serviceCode" .= _ccrqServiceCode,
               "attachmentSetId" .= _ccrqAttachmentSetId,
               "subject" .= _ccrqSubject,
               "communicationBody" .= _ccrqCommunicationBody]

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
-- * 'ccrsCaseId'
--
-- * 'ccrsStatus'
data CreateCaseResponse = CreateCaseResponse'
    { _ccrsCaseId :: !(Maybe Text)
    , _ccrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCaseResponse' smart constructor.
createCaseResponse :: Int -> CreateCaseResponse
createCaseResponse pStatus =
    CreateCaseResponse'
    { _ccrsCaseId = Nothing
    , _ccrsStatus = pStatus
    }

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
ccrsCaseId :: Lens' CreateCaseResponse (Maybe Text)
ccrsCaseId = lens _ccrsCaseId (\ s a -> s{_ccrsCaseId = a});

-- | FIXME: Undocumented member.
ccrsStatus :: Lens' CreateCaseResponse Int
ccrsStatus = lens _ccrsStatus (\ s a -> s{_ccrsStatus = a});
