{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.AddCommunicationToCase
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds additional customer communication to an AWS Support case. You use
-- the @CaseId@ value to identify the case to add communication to. You can
-- list a set of email addresses to copy on the communication using the
-- @CcEmailAddresses@ value. The @CommunicationBody@ value contains the
-- text of the communication.
--
-- The response indicates the success or failure of the request.
--
-- This operation implements a subset of the features of the AWS Support
-- Center.
--
-- /See:/ <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_AddCommunicationToCase.html AWS API Reference> for AddCommunicationToCase.
module Network.AWS.Support.AddCommunicationToCase
    (
    -- * Creating a Request
      AddCommunicationToCase
    , addCommunicationToCase
    -- * Request Lenses
    , actcCaseId
    , actcCcEmailAddresses
    , actcAttachmentSetId
    , actcCommunicationBody

    -- * Destructuring the Response
    , AddCommunicationToCaseResponse
    , addCommunicationToCaseResponse
    -- * Response Lenses
    , actcrsResult
    , actcrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | To be written.
--
-- /See:/ 'addCommunicationToCase' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'actcCaseId'
--
-- * 'actcCcEmailAddresses'
--
-- * 'actcAttachmentSetId'
--
-- * 'actcCommunicationBody'
data AddCommunicationToCase = AddCommunicationToCase'
    { _actcCaseId            :: !(Maybe Text)
    , _actcCcEmailAddresses  :: !(Maybe [Text])
    , _actcAttachmentSetId   :: !(Maybe Text)
    , _actcCommunicationBody :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddCommunicationToCase' smart constructor.
addCommunicationToCase :: Text -> AddCommunicationToCase
addCommunicationToCase pCommunicationBody_ =
    AddCommunicationToCase'
    { _actcCaseId = Nothing
    , _actcCcEmailAddresses = Nothing
    , _actcAttachmentSetId = Nothing
    , _actcCommunicationBody = pCommunicationBody_
    }

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
actcCaseId :: Lens' AddCommunicationToCase (Maybe Text)
actcCaseId = lens _actcCaseId (\ s a -> s{_actcCaseId = a});

-- | The email addresses in the CC line of an email to be added to the
-- support case.
actcCcEmailAddresses :: Lens' AddCommunicationToCase [Text]
actcCcEmailAddresses = lens _actcCcEmailAddresses (\ s a -> s{_actcCcEmailAddresses = a}) . _Default . _Coerce;

-- | The ID of a set of one or more attachments for the communication to add
-- to the case. Create the set by calling AddAttachmentsToSet
actcAttachmentSetId :: Lens' AddCommunicationToCase (Maybe Text)
actcAttachmentSetId = lens _actcAttachmentSetId (\ s a -> s{_actcAttachmentSetId = a});

-- | The body of an email communication to add to the support case.
actcCommunicationBody :: Lens' AddCommunicationToCase Text
actcCommunicationBody = lens _actcCommunicationBody (\ s a -> s{_actcCommunicationBody = a});

instance AWSRequest AddCommunicationToCase where
        type Sv AddCommunicationToCase = Support
        type Rs AddCommunicationToCase =
             AddCommunicationToCaseResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AddCommunicationToCaseResponse' <$>
                   (x .?> "result") <*> (pure (fromEnum s)))

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
              ["caseId" .= _actcCaseId,
               "ccEmailAddresses" .= _actcCcEmailAddresses,
               "attachmentSetId" .= _actcAttachmentSetId,
               "communicationBody" .= _actcCommunicationBody]

instance ToPath AddCommunicationToCase where
        toPath = const "/"

instance ToQuery AddCommunicationToCase where
        toQuery = const mempty

-- | The result of the AddCommunicationToCase operation.
--
-- /See:/ 'addCommunicationToCaseResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'actcrsResult'
--
-- * 'actcrsStatus'
data AddCommunicationToCaseResponse = AddCommunicationToCaseResponse'
    { _actcrsResult :: !(Maybe Bool)
    , _actcrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddCommunicationToCaseResponse' smart constructor.
addCommunicationToCaseResponse :: Int -> AddCommunicationToCaseResponse
addCommunicationToCaseResponse pStatus_ =
    AddCommunicationToCaseResponse'
    { _actcrsResult = Nothing
    , _actcrsStatus = pStatus_
    }

-- | True if AddCommunicationToCase succeeds. Otherwise, returns an error.
actcrsResult :: Lens' AddCommunicationToCaseResponse (Maybe Bool)
actcrsResult = lens _actcrsResult (\ s a -> s{_actcrsResult = a});

-- | Undocumented member.
actcrsStatus :: Lens' AddCommunicationToCaseResponse Int
actcrsStatus = lens _actcrsStatus (\ s a -> s{_actcrsStatus = a});
