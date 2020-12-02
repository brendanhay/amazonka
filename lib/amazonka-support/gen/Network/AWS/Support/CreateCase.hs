{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.CreateCase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new case in the AWS Support Center. This operation is modeled on the behavior of the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page. Its parameters require you to specify the following information:
--
--
--     * __issueType.__ The type of issue for the case. You can specify either "customer-service" or "technical." If you do not indicate a value, the default is "technical."
--
--     * __serviceCode.__ The code for an AWS service. You obtain the @serviceCode@ by calling 'DescribeServices' .
--
--     * __categoryCode.__ The category for the service defined for the @serviceCode@ value. You also obtain the category code for a service by calling 'DescribeServices' . Each AWS service defines its own set of category codes.
--
--     * __severityCode.__ A value that indicates the urgency of the case, which in turn determines the response time according to your service level agreement with AWS Support. You obtain the SeverityCode by calling 'DescribeSeverityLevels' .
--
--     * __subject.__ The __Subject__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
--
--     * __communicationBody.__ The __Description__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
--
--     * __attachmentSetId.__ The ID of a set of attachments that has been created by using 'AddAttachmentsToSet' .
--
--     * __language.__ The human language in which AWS Support handles the case. English and Japanese are currently supported.
--
--     * __ccEmailAddresses.__ The AWS Support Center __CC__ field on the <https://console.aws.amazon.com/support/home#/case/create Create Case> page. You can list email addresses to be copied on any correspondence about the case. The account that opens the case is already identified by passing the AWS Credentials in the HTTP POST method or in a method or function call from one of the programming languages supported by an <http://aws.amazon.com/tools/ AWS SDK> .
--
--
--
-- A successful 'CreateCase' request returns an AWS Support case number. Case numbers are used by the 'DescribeCases' operation to retrieve existing AWS Support cases.
--
module Network.AWS.Support.CreateCase
    (
    -- * Creating a Request
      createCase
    , CreateCase
    -- * Request Lenses
    , ccSeverityCode
    , ccIssueType
    , ccCcEmailAddresses
    , ccLanguage
    , ccCategoryCode
    , ccServiceCode
    , ccAttachmentSetId
    , ccSubject
    , ccCommunicationBody

    -- * Destructuring the Response
    , createCaseResponse
    , CreateCaseResponse
    -- * Response Lenses
    , ccrsCaseId
    , ccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types
import Network.AWS.Support.Types.Product

-- |
--
--
--
-- /See:/ 'createCase' smart constructor.
data CreateCase = CreateCase'
  { _ccSeverityCode      :: !(Maybe Text)
  , _ccIssueType         :: !(Maybe Text)
  , _ccCcEmailAddresses  :: !(Maybe [Text])
  , _ccLanguage          :: !(Maybe Text)
  , _ccCategoryCode      :: !(Maybe Text)
  , _ccServiceCode       :: !(Maybe Text)
  , _ccAttachmentSetId   :: !(Maybe Text)
  , _ccSubject           :: !Text
  , _ccCommunicationBody :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccSeverityCode' - The code for the severity level returned by the call to 'DescribeSeverityLevels' .
--
-- * 'ccIssueType' - The type of issue for the case. You can specify either "customer-service" or "technical." If you do not indicate a value, the default is "technical."
--
-- * 'ccCcEmailAddresses' - A list of email addresses that AWS Support copies on case correspondence.
--
-- * 'ccLanguage' - The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- * 'ccCategoryCode' - The category of problem for the AWS Support case.
--
-- * 'ccServiceCode' - The code for the AWS service returned by the call to 'DescribeServices' .
--
-- * 'ccAttachmentSetId' - The ID of a set of one or more attachments for the case. Create the set by using 'AddAttachmentsToSet' .
--
-- * 'ccSubject' - The title of the AWS Support case.
--
-- * 'ccCommunicationBody' - The communication body text when you create an AWS Support case by calling 'CreateCase' .
createCase
    :: Text -- ^ 'ccSubject'
    -> Text -- ^ 'ccCommunicationBody'
    -> CreateCase
createCase pSubject_ pCommunicationBody_ =
  CreateCase'
    { _ccSeverityCode = Nothing
    , _ccIssueType = Nothing
    , _ccCcEmailAddresses = Nothing
    , _ccLanguage = Nothing
    , _ccCategoryCode = Nothing
    , _ccServiceCode = Nothing
    , _ccAttachmentSetId = Nothing
    , _ccSubject = pSubject_
    , _ccCommunicationBody = pCommunicationBody_
    }


-- | The code for the severity level returned by the call to 'DescribeSeverityLevels' .
ccSeverityCode :: Lens' CreateCase (Maybe Text)
ccSeverityCode = lens _ccSeverityCode (\ s a -> s{_ccSeverityCode = a})

-- | The type of issue for the case. You can specify either "customer-service" or "technical." If you do not indicate a value, the default is "technical."
ccIssueType :: Lens' CreateCase (Maybe Text)
ccIssueType = lens _ccIssueType (\ s a -> s{_ccIssueType = a})

-- | A list of email addresses that AWS Support copies on case correspondence.
ccCcEmailAddresses :: Lens' CreateCase [Text]
ccCcEmailAddresses = lens _ccCcEmailAddresses (\ s a -> s{_ccCcEmailAddresses = a}) . _Default . _Coerce

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
ccLanguage :: Lens' CreateCase (Maybe Text)
ccLanguage = lens _ccLanguage (\ s a -> s{_ccLanguage = a})

-- | The category of problem for the AWS Support case.
ccCategoryCode :: Lens' CreateCase (Maybe Text)
ccCategoryCode = lens _ccCategoryCode (\ s a -> s{_ccCategoryCode = a})

-- | The code for the AWS service returned by the call to 'DescribeServices' .
ccServiceCode :: Lens' CreateCase (Maybe Text)
ccServiceCode = lens _ccServiceCode (\ s a -> s{_ccServiceCode = a})

-- | The ID of a set of one or more attachments for the case. Create the set by using 'AddAttachmentsToSet' .
ccAttachmentSetId :: Lens' CreateCase (Maybe Text)
ccAttachmentSetId = lens _ccAttachmentSetId (\ s a -> s{_ccAttachmentSetId = a})

-- | The title of the AWS Support case.
ccSubject :: Lens' CreateCase Text
ccSubject = lens _ccSubject (\ s a -> s{_ccSubject = a})

-- | The communication body text when you create an AWS Support case by calling 'CreateCase' .
ccCommunicationBody :: Lens' CreateCase Text
ccCommunicationBody = lens _ccCommunicationBody (\ s a -> s{_ccCommunicationBody = a})

instance AWSRequest CreateCase where
        type Rs CreateCase = CreateCaseResponse
        request = postJSON support
        response
          = receiveJSON
              (\ s h x ->
                 CreateCaseResponse' <$>
                   (x .?> "caseId") <*> (pure (fromEnum s)))

instance Hashable CreateCase where

instance NFData CreateCase where

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
              (catMaybes
                 [("severityCode" .=) <$> _ccSeverityCode,
                  ("issueType" .=) <$> _ccIssueType,
                  ("ccEmailAddresses" .=) <$> _ccCcEmailAddresses,
                  ("language" .=) <$> _ccLanguage,
                  ("categoryCode" .=) <$> _ccCategoryCode,
                  ("serviceCode" .=) <$> _ccServiceCode,
                  ("attachmentSetId" .=) <$> _ccAttachmentSetId,
                  Just ("subject" .= _ccSubject),
                  Just ("communicationBody" .= _ccCommunicationBody)])

instance ToPath CreateCase where
        toPath = const "/"

instance ToQuery CreateCase where
        toQuery = const mempty

-- | The AWS Support case ID returned by a successful completion of the 'CreateCase' operation.
--
--
--
-- /See:/ 'createCaseResponse' smart constructor.
data CreateCaseResponse = CreateCaseResponse'
  { _ccrsCaseId         :: !(Maybe Text)
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsCaseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createCaseResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateCaseResponse
createCaseResponse pResponseStatus_ =
  CreateCaseResponse'
    {_ccrsCaseId = Nothing, _ccrsResponseStatus = pResponseStatus_}


-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
ccrsCaseId :: Lens' CreateCaseResponse (Maybe Text)
ccrsCaseId = lens _ccrsCaseId (\ s a -> s{_ccrsCaseId = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateCaseResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateCaseResponse where
