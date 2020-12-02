{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Communication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.Communication where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Support.Types.AttachmentDetails

-- | A communication associated with an AWS Support case. The communication consists of the case ID, the message body, attachment information, the submitter of the communication, and the date and time of the communication.
--
--
--
-- /See:/ 'communication' smart constructor.
data Communication = Communication'
  { _cBody :: !(Maybe Text),
    _cCaseId :: !(Maybe Text),
    _cSubmittedBy :: !(Maybe Text),
    _cTimeCreated :: !(Maybe Text),
    _cAttachmentSet :: !(Maybe [AttachmentDetails])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Communication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cBody' - The text of the communication between the customer and AWS Support.
--
-- * 'cCaseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- * 'cSubmittedBy' - The identity of the account that submitted, or responded to, the support case. Customer entries include the role or IAM user as well as the email address. For example, "AdminRole (Role) <someone@example.com>. Entries from the AWS Support team display "Amazon Web Services," and do not show an email address.
--
-- * 'cTimeCreated' - The time the communication was created.
--
-- * 'cAttachmentSet' - Information about the attachments to the case communication.
communication ::
  Communication
communication =
  Communication'
    { _cBody = Nothing,
      _cCaseId = Nothing,
      _cSubmittedBy = Nothing,
      _cTimeCreated = Nothing,
      _cAttachmentSet = Nothing
    }

-- | The text of the communication between the customer and AWS Support.
cBody :: Lens' Communication (Maybe Text)
cBody = lens _cBody (\s a -> s {_cBody = a})

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
cCaseId :: Lens' Communication (Maybe Text)
cCaseId = lens _cCaseId (\s a -> s {_cCaseId = a})

-- | The identity of the account that submitted, or responded to, the support case. Customer entries include the role or IAM user as well as the email address. For example, "AdminRole (Role) <someone@example.com>. Entries from the AWS Support team display "Amazon Web Services," and do not show an email address.
cSubmittedBy :: Lens' Communication (Maybe Text)
cSubmittedBy = lens _cSubmittedBy (\s a -> s {_cSubmittedBy = a})

-- | The time the communication was created.
cTimeCreated :: Lens' Communication (Maybe Text)
cTimeCreated = lens _cTimeCreated (\s a -> s {_cTimeCreated = a})

-- | Information about the attachments to the case communication.
cAttachmentSet :: Lens' Communication [AttachmentDetails]
cAttachmentSet = lens _cAttachmentSet (\s a -> s {_cAttachmentSet = a}) . _Default . _Coerce

instance FromJSON Communication where
  parseJSON =
    withObject
      "Communication"
      ( \x ->
          Communication'
            <$> (x .:? "body")
            <*> (x .:? "caseId")
            <*> (x .:? "submittedBy")
            <*> (x .:? "timeCreated")
            <*> (x .:? "attachmentSet" .!= mempty)
      )

instance Hashable Communication

instance NFData Communication
