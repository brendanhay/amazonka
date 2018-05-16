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
-- Module      : Network.AWS.Connect.StartOutboundVoiceContact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @StartOutboundVoiceContact@ operation initiates a contact flow to place an outbound call to a customer.
--
--
-- There is a throttling limit placed on usage of the API that includes a @RateLimit@ of 2 per second, and a @BurstLimit@ of 5 per second.
--
-- If you are using an IAM account, it must have permissions to the @connect:StartOutboundVoiceContact@ action.
--
module Network.AWS.Connect.StartOutboundVoiceContact
    (
    -- * Creating a Request
      startOutboundVoiceContact
    , StartOutboundVoiceContact
    -- * Request Lenses
    , sovcClientToken
    , sovcQueueId
    , sovcAttributes
    , sovcSourcePhoneNumber
    , sovcDestinationPhoneNumber
    , sovcContactFlowId
    , sovcInstanceId

    -- * Destructuring the Response
    , startOutboundVoiceContactResponse
    , StartOutboundVoiceContactResponse
    -- * Response Lenses
    , sovcrsContactId
    , sovcrsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startOutboundVoiceContact' smart constructor.
data StartOutboundVoiceContact = StartOutboundVoiceContact'
  { _sovcClientToken            :: !(Maybe Text)
  , _sovcQueueId                :: !(Maybe Text)
  , _sovcAttributes             :: !(Maybe (Map Text Text))
  , _sovcSourcePhoneNumber      :: !(Maybe Text)
  , _sovcDestinationPhoneNumber :: !Text
  , _sovcContactFlowId          :: !Text
  , _sovcInstanceId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartOutboundVoiceContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sovcClientToken' - A unique, case-sensitive identifier that you provide to ensure the idempotency of the request. The token is valid for 7 days after creation. If a contact is already started, the contact ID is returned. If the contact is disconnected, a new contact is started.
--
-- * 'sovcQueueId' - The queue to which to add the call. If you specify a queue, the phone displayed for caller ID is the phone number defined for the queue. If you do not specify a queue, the queue used is the queue defined in the contact flow specified by @ContactFlowId@ . To find the @QueueId@ , open the queue to use in the Amazon Connect queue editor. The ID for the queue is displayed in the address bar as part of the URL. For example, the @QueueId@ value is the set of characters at the end of the URL, after "queue/", such as @aeg40574-2d01-51c3-73d6-bf8624d2168c@ .
--
-- * 'sovcAttributes' - Specify a custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes. There can be up to 32,768 UTF-8 bytes across all key-value pairs. Attribute keys can include only alphanumeric, dash, and underscore characters. For example, to play a greeting when the customer answers the call, you can pass the customer name in attributes similar to the following:
--
-- * 'sovcSourcePhoneNumber' - The phone number, in E.164 format, associated with your Amazon Connect instance to use to place the outbound call.
--
-- * 'sovcDestinationPhoneNumber' - The phone number, in E.164 format, of the customer to call with the outbound contact.
--
-- * 'sovcContactFlowId' - The identifier for the contact flow to execute for the outbound call. This is a GUID value only. Amazon Resource Name (ARN) values are not supported. To find the @ContactFlowId@ , open the contact flow to use in the Amazon Connect contact flow designer. The ID for the contact flow is displayed in the address bar as part of the URL. For example, an address displayed when you open a contact flow is similar to the following: @https://myconnectinstance.awsapps.com/connect/contact-flows/edit?id=arn:aws:connect:us-east-1:361814831152:instance/2fb42df9-78a2-4b99-b484-f5cf80dc300c/contact-flow//b0b8f2dd-ed1b-4c44-af36-ce189a178181/ @ . At the end of the URL, you see @contact-flow/b0b8f2dd-ed1b-4c44-af36-ce189a178181@ . The @ContactFlowID@ for this contact flow is @/b0b8f2dd-ed1b-4c44-af36-ce189a178181/ @ . Make sure to include only the GUID after the "contact-flow/" in your requests.
--
-- * 'sovcInstanceId' - The identifier for your Amazon Connect instance. To find the @InstanceId@ value for your Amazon Connect instance, open the <https://console.aws.amazon.com/connect/ Amazon Connect console> . Select the instance alias of the instance and view the instance ID in the __Overview__ section. For example, the instance ID is the set of characters at the end of the instance ARN, after "instance/", such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
startOutboundVoiceContact
    :: Text -- ^ 'sovcDestinationPhoneNumber'
    -> Text -- ^ 'sovcContactFlowId'
    -> Text -- ^ 'sovcInstanceId'
    -> StartOutboundVoiceContact
startOutboundVoiceContact pDestinationPhoneNumber_ pContactFlowId_ pInstanceId_ =
  StartOutboundVoiceContact'
    { _sovcClientToken = Nothing
    , _sovcQueueId = Nothing
    , _sovcAttributes = Nothing
    , _sovcSourcePhoneNumber = Nothing
    , _sovcDestinationPhoneNumber = pDestinationPhoneNumber_
    , _sovcContactFlowId = pContactFlowId_
    , _sovcInstanceId = pInstanceId_
    }


-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request. The token is valid for 7 days after creation. If a contact is already started, the contact ID is returned. If the contact is disconnected, a new contact is started.
sovcClientToken :: Lens' StartOutboundVoiceContact (Maybe Text)
sovcClientToken = lens _sovcClientToken (\ s a -> s{_sovcClientToken = a})

-- | The queue to which to add the call. If you specify a queue, the phone displayed for caller ID is the phone number defined for the queue. If you do not specify a queue, the queue used is the queue defined in the contact flow specified by @ContactFlowId@ . To find the @QueueId@ , open the queue to use in the Amazon Connect queue editor. The ID for the queue is displayed in the address bar as part of the URL. For example, the @QueueId@ value is the set of characters at the end of the URL, after "queue/", such as @aeg40574-2d01-51c3-73d6-bf8624d2168c@ .
sovcQueueId :: Lens' StartOutboundVoiceContact (Maybe Text)
sovcQueueId = lens _sovcQueueId (\ s a -> s{_sovcQueueId = a})

-- | Specify a custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes. There can be up to 32,768 UTF-8 bytes across all key-value pairs. Attribute keys can include only alphanumeric, dash, and underscore characters. For example, to play a greeting when the customer answers the call, you can pass the customer name in attributes similar to the following:
sovcAttributes :: Lens' StartOutboundVoiceContact (HashMap Text Text)
sovcAttributes = lens _sovcAttributes (\ s a -> s{_sovcAttributes = a}) . _Default . _Map

-- | The phone number, in E.164 format, associated with your Amazon Connect instance to use to place the outbound call.
sovcSourcePhoneNumber :: Lens' StartOutboundVoiceContact (Maybe Text)
sovcSourcePhoneNumber = lens _sovcSourcePhoneNumber (\ s a -> s{_sovcSourcePhoneNumber = a})

-- | The phone number, in E.164 format, of the customer to call with the outbound contact.
sovcDestinationPhoneNumber :: Lens' StartOutboundVoiceContact Text
sovcDestinationPhoneNumber = lens _sovcDestinationPhoneNumber (\ s a -> s{_sovcDestinationPhoneNumber = a})

-- | The identifier for the contact flow to execute for the outbound call. This is a GUID value only. Amazon Resource Name (ARN) values are not supported. To find the @ContactFlowId@ , open the contact flow to use in the Amazon Connect contact flow designer. The ID for the contact flow is displayed in the address bar as part of the URL. For example, an address displayed when you open a contact flow is similar to the following: @https://myconnectinstance.awsapps.com/connect/contact-flows/edit?id=arn:aws:connect:us-east-1:361814831152:instance/2fb42df9-78a2-4b99-b484-f5cf80dc300c/contact-flow//b0b8f2dd-ed1b-4c44-af36-ce189a178181/ @ . At the end of the URL, you see @contact-flow/b0b8f2dd-ed1b-4c44-af36-ce189a178181@ . The @ContactFlowID@ for this contact flow is @/b0b8f2dd-ed1b-4c44-af36-ce189a178181/ @ . Make sure to include only the GUID after the "contact-flow/" in your requests.
sovcContactFlowId :: Lens' StartOutboundVoiceContact Text
sovcContactFlowId = lens _sovcContactFlowId (\ s a -> s{_sovcContactFlowId = a})

-- | The identifier for your Amazon Connect instance. To find the @InstanceId@ value for your Amazon Connect instance, open the <https://console.aws.amazon.com/connect/ Amazon Connect console> . Select the instance alias of the instance and view the instance ID in the __Overview__ section. For example, the instance ID is the set of characters at the end of the instance ARN, after "instance/", such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
sovcInstanceId :: Lens' StartOutboundVoiceContact Text
sovcInstanceId = lens _sovcInstanceId (\ s a -> s{_sovcInstanceId = a})

instance AWSRequest StartOutboundVoiceContact where
        type Rs StartOutboundVoiceContact =
             StartOutboundVoiceContactResponse
        request = putJSON connect
        response
          = receiveJSON
              (\ s h x ->
                 StartOutboundVoiceContactResponse' <$>
                   (x .?> "ContactId") <*> (pure (fromEnum s)))

instance Hashable StartOutboundVoiceContact where

instance NFData StartOutboundVoiceContact where

instance ToHeaders StartOutboundVoiceContact where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartOutboundVoiceContact where
        toJSON StartOutboundVoiceContact'{..}
          = object
              (catMaybes
                 [("ClientToken" .=) <$> _sovcClientToken,
                  ("QueueId" .=) <$> _sovcQueueId,
                  ("Attributes" .=) <$> _sovcAttributes,
                  ("SourcePhoneNumber" .=) <$> _sovcSourcePhoneNumber,
                  Just
                    ("DestinationPhoneNumber" .=
                       _sovcDestinationPhoneNumber),
                  Just ("ContactFlowId" .= _sovcContactFlowId),
                  Just ("InstanceId" .= _sovcInstanceId)])

instance ToPath StartOutboundVoiceContact where
        toPath = const "/contact/outbound-voice"

instance ToQuery StartOutboundVoiceContact where
        toQuery = const mempty

-- | /See:/ 'startOutboundVoiceContactResponse' smart constructor.
data StartOutboundVoiceContactResponse = StartOutboundVoiceContactResponse'
  { _sovcrsContactId      :: !(Maybe Text)
  , _sovcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartOutboundVoiceContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sovcrsContactId' - The unique identifier of this contact within your Amazon Connect instance.
--
-- * 'sovcrsResponseStatus' - -- | The response status code.
startOutboundVoiceContactResponse
    :: Int -- ^ 'sovcrsResponseStatus'
    -> StartOutboundVoiceContactResponse
startOutboundVoiceContactResponse pResponseStatus_ =
  StartOutboundVoiceContactResponse'
    {_sovcrsContactId = Nothing, _sovcrsResponseStatus = pResponseStatus_}


-- | The unique identifier of this contact within your Amazon Connect instance.
sovcrsContactId :: Lens' StartOutboundVoiceContactResponse (Maybe Text)
sovcrsContactId = lens _sovcrsContactId (\ s a -> s{_sovcrsContactId = a})

-- | -- | The response status code.
sovcrsResponseStatus :: Lens' StartOutboundVoiceContactResponse Int
sovcrsResponseStatus = lens _sovcrsResponseStatus (\ s a -> s{_sovcrsResponseStatus = a})

instance NFData StartOutboundVoiceContactResponse
         where
