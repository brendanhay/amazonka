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
-- Module      : Network.AWS.SES.SendBounce
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates and sends a bounce message to the sender of an email you received through Amazon SES. You can only use this API on an email up to 24 hours after you receive it.
--
-- You cannot use this API to send generic bounces for mail that was not received by Amazon SES.
--
-- For information about receiving email through Amazon SES, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide>.
--
-- This action is throttled at one request per second.
module Network.AWS.SES.SendBounce
    (
    -- * Creating a Request
      sendBounce
    , SendBounce
    -- * Request Lenses
    , sbMessageDsn
    , sbExplanation
    , sbBounceSenderARN
    , sbOriginalMessageId
    , sbBounceSender
    , sbBouncedRecipientInfoList

    -- * Destructuring the Response
    , sendBounceResponse
    , SendBounceResponse
    -- * Response Lenses
    , sbrsMessageId
    , sbrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'sendBounce' smart constructor.
data SendBounce = SendBounce'
    { _sbMessageDsn               :: !(Maybe MessageDsn)
    , _sbExplanation              :: !(Maybe Text)
    , _sbBounceSenderARN          :: !(Maybe Text)
    , _sbOriginalMessageId        :: !Text
    , _sbBounceSender             :: !Text
    , _sbBouncedRecipientInfoList :: ![BouncedRecipientInfo]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SendBounce' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbMessageDsn'
--
-- * 'sbExplanation'
--
-- * 'sbBounceSenderARN'
--
-- * 'sbOriginalMessageId'
--
-- * 'sbBounceSender'
--
-- * 'sbBouncedRecipientInfoList'
sendBounce
    :: Text -- ^ 'sbOriginalMessageId'
    -> Text -- ^ 'sbBounceSender'
    -> SendBounce
sendBounce pOriginalMessageId_ pBounceSender_ =
    SendBounce'
    { _sbMessageDsn = Nothing
    , _sbExplanation = Nothing
    , _sbBounceSenderARN = Nothing
    , _sbOriginalMessageId = pOriginalMessageId_
    , _sbBounceSender = pBounceSender_
    , _sbBouncedRecipientInfoList = mempty
    }

-- | Message-related DSN fields. If not specified, Amazon SES will choose the values.
sbMessageDsn :: Lens' SendBounce (Maybe MessageDsn)
sbMessageDsn = lens _sbMessageDsn (\ s a -> s{_sbMessageDsn = a});

-- | Human-readable text for the bounce message to explain the failure. If not specified, the text will be auto-generated based on the bounced recipient information.
sbExplanation :: Lens' SendBounce (Maybe Text)
sbExplanation = lens _sbExplanation (\ s a -> s{_sbExplanation = a});

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the address in the \"From\" header of the bounce. For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
sbBounceSenderARN :: Lens' SendBounce (Maybe Text)
sbBounceSenderARN = lens _sbBounceSenderARN (\ s a -> s{_sbBounceSenderARN = a});

-- | The message ID of the message to be bounced.
sbOriginalMessageId :: Lens' SendBounce Text
sbOriginalMessageId = lens _sbOriginalMessageId (\ s a -> s{_sbOriginalMessageId = a});

-- | The address to use in the \"From\" header of the bounce message. This must be an identity that you have verified with Amazon SES.
sbBounceSender :: Lens' SendBounce Text
sbBounceSender = lens _sbBounceSender (\ s a -> s{_sbBounceSender = a});

-- | A list of recipients of the bounced message, including the information required to create the Delivery Status Notifications (DSNs) for the recipients. You must specify at least one 'BouncedRecipientInfo' in the list.
sbBouncedRecipientInfoList :: Lens' SendBounce [BouncedRecipientInfo]
sbBouncedRecipientInfoList = lens _sbBouncedRecipientInfoList (\ s a -> s{_sbBouncedRecipientInfoList = a}) . _Coerce;

instance AWSRequest SendBounce where
        type Rs SendBounce = SendBounceResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "SendBounceResult"
              (\ s h x ->
                 SendBounceResponse' <$>
                   (x .@? "MessageId") <*> (pure (fromEnum s)))

instance Hashable SendBounce

instance NFData SendBounce

instance ToHeaders SendBounce where
        toHeaders = const mempty

instance ToPath SendBounce where
        toPath = const "/"

instance ToQuery SendBounce where
        toQuery SendBounce'{..}
          = mconcat
              ["Action" =: ("SendBounce" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "MessageDsn" =: _sbMessageDsn,
               "Explanation" =: _sbExplanation,
               "BounceSenderArn" =: _sbBounceSenderARN,
               "OriginalMessageId" =: _sbOriginalMessageId,
               "BounceSender" =: _sbBounceSender,
               "BouncedRecipientInfoList" =:
                 toQueryList "member" _sbBouncedRecipientInfoList]

-- | /See:/ 'sendBounceResponse' smart constructor.
data SendBounceResponse = SendBounceResponse'
    { _sbrsMessageId      :: !(Maybe Text)
    , _sbrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SendBounceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbrsMessageId'
--
-- * 'sbrsResponseStatus'
sendBounceResponse
    :: Int -- ^ 'sbrsResponseStatus'
    -> SendBounceResponse
sendBounceResponse pResponseStatus_ =
    SendBounceResponse'
    { _sbrsMessageId = Nothing
    , _sbrsResponseStatus = pResponseStatus_
    }

-- | The message ID of the bounce message.
sbrsMessageId :: Lens' SendBounceResponse (Maybe Text)
sbrsMessageId = lens _sbrsMessageId (\ s a -> s{_sbrsMessageId = a});

-- | The response status code.
sbrsResponseStatus :: Lens' SendBounceResponse Int
sbrsResponseStatus = lens _sbrsResponseStatus (\ s a -> s{_sbrsResponseStatus = a});

instance NFData SendBounceResponse
