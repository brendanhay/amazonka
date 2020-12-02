{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StartChatContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a contact flow to start a new chat for the customer. Response of this API provides a token required to obtain credentials from the <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> API in the Amazon Connect Participant Service.
--
--
-- When a new chat contact is successfully created, clients need to subscribe to the participant’s connection for the created chat within 5 minutes. This is achieved by invoking <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> with WEBSOCKET and CONNECTION_CREDENTIALS.
--
-- A 429 error occurs in two situations:
--
--     * API rate limit is exceeded. API TPS throttling returns a @TooManyRequests@ exception from the API Gateway.
--
--     * The <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html quota for concurrent active chats> is exceeded. Active chat throttling returns a @LimitExceededException@ .
--
--
--
-- For more information about how chat works, see <https://docs.aws.amazon.com/connect/latest/adminguide/chat.html Chat> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.StartChatContact
  ( -- * Creating a Request
    startChatContact,
    StartChatContact,

    -- * Request Lenses
    sccClientToken,
    sccAttributes,
    sccInitialMessage,
    sccInstanceId,
    sccContactFlowId,
    sccParticipantDetails,

    -- * Destructuring the Response
    startChatContactResponse,
    StartChatContactResponse,

    -- * Response Lenses
    sccrsParticipantToken,
    sccrsParticipantId,
    sccrsContactId,
    sccrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startChatContact' smart constructor.
data StartChatContact = StartChatContact'
  { _sccClientToken ::
      !(Maybe Text),
    _sccAttributes :: !(Maybe (Map Text (Text))),
    _sccInitialMessage :: !(Maybe ChatMessage),
    _sccInstanceId :: !Text,
    _sccContactFlowId :: !Text,
    _sccParticipantDetails :: !ParticipantDetails
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartChatContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sccClientToken' - A unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- * 'sccAttributes' - A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.  There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
--
-- * 'sccInitialMessage' - The initial message to be sent to the newly created chat.
--
-- * 'sccInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'sccContactFlowId' - The identifier of the contact flow for initiating the chat. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:  arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- * 'sccParticipantDetails' - Information identifying the participant.
startChatContact ::
  -- | 'sccInstanceId'
  Text ->
  -- | 'sccContactFlowId'
  Text ->
  -- | 'sccParticipantDetails'
  ParticipantDetails ->
  StartChatContact
startChatContact pInstanceId_ pContactFlowId_ pParticipantDetails_ =
  StartChatContact'
    { _sccClientToken = Nothing,
      _sccAttributes = Nothing,
      _sccInitialMessage = Nothing,
      _sccInstanceId = pInstanceId_,
      _sccContactFlowId = pContactFlowId_,
      _sccParticipantDetails = pParticipantDetails_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
sccClientToken :: Lens' StartChatContact (Maybe Text)
sccClientToken = lens _sccClientToken (\s a -> s {_sccClientToken = a})

-- | A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.  There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
sccAttributes :: Lens' StartChatContact (HashMap Text (Text))
sccAttributes = lens _sccAttributes (\s a -> s {_sccAttributes = a}) . _Default . _Map

-- | The initial message to be sent to the newly created chat.
sccInitialMessage :: Lens' StartChatContact (Maybe ChatMessage)
sccInitialMessage = lens _sccInitialMessage (\s a -> s {_sccInitialMessage = a})

-- | The identifier of the Amazon Connect instance.
sccInstanceId :: Lens' StartChatContact Text
sccInstanceId = lens _sccInstanceId (\s a -> s {_sccInstanceId = a})

-- | The identifier of the contact flow for initiating the chat. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:  arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
sccContactFlowId :: Lens' StartChatContact Text
sccContactFlowId = lens _sccContactFlowId (\s a -> s {_sccContactFlowId = a})

-- | Information identifying the participant.
sccParticipantDetails :: Lens' StartChatContact ParticipantDetails
sccParticipantDetails = lens _sccParticipantDetails (\s a -> s {_sccParticipantDetails = a})

instance AWSRequest StartChatContact where
  type Rs StartChatContact = StartChatContactResponse
  request = putJSON connect
  response =
    receiveJSON
      ( \s h x ->
          StartChatContactResponse'
            <$> (x .?> "ParticipantToken")
            <*> (x .?> "ParticipantId")
            <*> (x .?> "ContactId")
            <*> (pure (fromEnum s))
      )

instance Hashable StartChatContact

instance NFData StartChatContact

instance ToHeaders StartChatContact where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON StartChatContact where
  toJSON StartChatContact' {..} =
    object
      ( catMaybes
          [ ("ClientToken" .=) <$> _sccClientToken,
            ("Attributes" .=) <$> _sccAttributes,
            ("InitialMessage" .=) <$> _sccInitialMessage,
            Just ("InstanceId" .= _sccInstanceId),
            Just ("ContactFlowId" .= _sccContactFlowId),
            Just ("ParticipantDetails" .= _sccParticipantDetails)
          ]
      )

instance ToPath StartChatContact where
  toPath = const "/contact/chat"

instance ToQuery StartChatContact where
  toQuery = const mempty

-- | /See:/ 'startChatContactResponse' smart constructor.
data StartChatContactResponse = StartChatContactResponse'
  { _sccrsParticipantToken ::
      !(Maybe Text),
    _sccrsParticipantId :: !(Maybe Text),
    _sccrsContactId :: !(Maybe Text),
    _sccrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartChatContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sccrsParticipantToken' - The token used by the chat participant to call <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> . The participant token is valid for the lifetime of a chat participant.
--
-- * 'sccrsParticipantId' - The identifier for a chat participant. The participantId for a chat participant is the same throughout the chat lifecycle.
--
-- * 'sccrsContactId' - The identifier of this contact within the Amazon Connect instance.
--
-- * 'sccrsResponseStatus' - -- | The response status code.
startChatContactResponse ::
  -- | 'sccrsResponseStatus'
  Int ->
  StartChatContactResponse
startChatContactResponse pResponseStatus_ =
  StartChatContactResponse'
    { _sccrsParticipantToken = Nothing,
      _sccrsParticipantId = Nothing,
      _sccrsContactId = Nothing,
      _sccrsResponseStatus = pResponseStatus_
    }

-- | The token used by the chat participant to call <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> . The participant token is valid for the lifetime of a chat participant.
sccrsParticipantToken :: Lens' StartChatContactResponse (Maybe Text)
sccrsParticipantToken = lens _sccrsParticipantToken (\s a -> s {_sccrsParticipantToken = a})

-- | The identifier for a chat participant. The participantId for a chat participant is the same throughout the chat lifecycle.
sccrsParticipantId :: Lens' StartChatContactResponse (Maybe Text)
sccrsParticipantId = lens _sccrsParticipantId (\s a -> s {_sccrsParticipantId = a})

-- | The identifier of this contact within the Amazon Connect instance.
sccrsContactId :: Lens' StartChatContactResponse (Maybe Text)
sccrsContactId = lens _sccrsContactId (\s a -> s {_sccrsContactId = a})

-- | -- | The response status code.
sccrsResponseStatus :: Lens' StartChatContactResponse Int
sccrsResponseStatus = lens _sccrsResponseStatus (\s a -> s {_sccrsResponseStatus = a})

instance NFData StartChatContactResponse
