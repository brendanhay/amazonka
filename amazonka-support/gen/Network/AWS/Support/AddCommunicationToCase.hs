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
-- Module      : Network.AWS.Support.AddCommunicationToCase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional customer communication to an AWS Support case. You use the @caseId@ value to identify the case to add communication to. You can list a set of email addresses to copy on the communication using the @ccEmailAddresses@ value. The @communicationBody@ value contains the text of the communication.
--
--
-- The response indicates the success or failure of the request.
--
-- This operation implements a subset of the features of the AWS Support Center.
--
module Network.AWS.Support.AddCommunicationToCase
    (
    -- * Creating a Request
      addCommunicationToCase
    , AddCommunicationToCase
    -- * Request Lenses
    , actcCaseId
    , actcCcEmailAddresses
    , actcAttachmentSetId
    , actcCommunicationBody

    -- * Destructuring the Response
    , addCommunicationToCaseResponse
    , AddCommunicationToCaseResponse
    -- * Response Lenses
    , actcrsResult
    , actcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types
import Network.AWS.Support.Types.Product

-- | To be written.
--
--
--
-- /See:/ 'addCommunicationToCase' smart constructor.
data AddCommunicationToCase = AddCommunicationToCase'
  { _actcCaseId            :: !(Maybe Text)
  , _actcCcEmailAddresses  :: !(Maybe [Text])
  , _actcAttachmentSetId   :: !(Maybe Text)
  , _actcCommunicationBody :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddCommunicationToCase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'actcCaseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- * 'actcCcEmailAddresses' - The email addresses in the CC line of an email to be added to the support case.
--
-- * 'actcAttachmentSetId' - The ID of a set of one or more attachments for the communication to add to the case. Create the set by calling 'AddAttachmentsToSet'
--
-- * 'actcCommunicationBody' - The body of an email communication to add to the support case.
addCommunicationToCase
    :: Text -- ^ 'actcCommunicationBody'
    -> AddCommunicationToCase
addCommunicationToCase pCommunicationBody_ =
  AddCommunicationToCase'
    { _actcCaseId = Nothing
    , _actcCcEmailAddresses = Nothing
    , _actcAttachmentSetId = Nothing
    , _actcCommunicationBody = pCommunicationBody_
    }


-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
actcCaseId :: Lens' AddCommunicationToCase (Maybe Text)
actcCaseId = lens _actcCaseId (\ s a -> s{_actcCaseId = a})

-- | The email addresses in the CC line of an email to be added to the support case.
actcCcEmailAddresses :: Lens' AddCommunicationToCase [Text]
actcCcEmailAddresses = lens _actcCcEmailAddresses (\ s a -> s{_actcCcEmailAddresses = a}) . _Default . _Coerce

-- | The ID of a set of one or more attachments for the communication to add to the case. Create the set by calling 'AddAttachmentsToSet'
actcAttachmentSetId :: Lens' AddCommunicationToCase (Maybe Text)
actcAttachmentSetId = lens _actcAttachmentSetId (\ s a -> s{_actcAttachmentSetId = a})

-- | The body of an email communication to add to the support case.
actcCommunicationBody :: Lens' AddCommunicationToCase Text
actcCommunicationBody = lens _actcCommunicationBody (\ s a -> s{_actcCommunicationBody = a})

instance AWSRequest AddCommunicationToCase where
        type Rs AddCommunicationToCase =
             AddCommunicationToCaseResponse
        request = postJSON support
        response
          = receiveJSON
              (\ s h x ->
                 AddCommunicationToCaseResponse' <$>
                   (x .?> "result") <*> (pure (fromEnum s)))

instance Hashable AddCommunicationToCase where

instance NFData AddCommunicationToCase where

instance ToHeaders AddCommunicationToCase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.AddCommunicationToCase" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddCommunicationToCase where
        toJSON AddCommunicationToCase'{..}
          = object
              (catMaybes
                 [("caseId" .=) <$> _actcCaseId,
                  ("ccEmailAddresses" .=) <$> _actcCcEmailAddresses,
                  ("attachmentSetId" .=) <$> _actcAttachmentSetId,
                  Just
                    ("communicationBody" .= _actcCommunicationBody)])

instance ToPath AddCommunicationToCase where
        toPath = const "/"

instance ToQuery AddCommunicationToCase where
        toQuery = const mempty

-- | The result of the 'AddCommunicationToCase' operation.
--
--
--
-- /See:/ 'addCommunicationToCaseResponse' smart constructor.
data AddCommunicationToCaseResponse = AddCommunicationToCaseResponse'
  { _actcrsResult         :: !(Maybe Bool)
  , _actcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddCommunicationToCaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'actcrsResult' - True if 'AddCommunicationToCase' succeeds. Otherwise, returns an error.
--
-- * 'actcrsResponseStatus' - -- | The response status code.
addCommunicationToCaseResponse
    :: Int -- ^ 'actcrsResponseStatus'
    -> AddCommunicationToCaseResponse
addCommunicationToCaseResponse pResponseStatus_ =
  AddCommunicationToCaseResponse'
    {_actcrsResult = Nothing, _actcrsResponseStatus = pResponseStatus_}


-- | True if 'AddCommunicationToCase' succeeds. Otherwise, returns an error.
actcrsResult :: Lens' AddCommunicationToCaseResponse (Maybe Bool)
actcrsResult = lens _actcrsResult (\ s a -> s{_actcrsResult = a})

-- | -- | The response status code.
actcrsResponseStatus :: Lens' AddCommunicationToCaseResponse Int
actcrsResponseStatus = lens _actcrsResponseStatus (\ s a -> s{_actcrsResponseStatus = a})

instance NFData AddCommunicationToCaseResponse where
