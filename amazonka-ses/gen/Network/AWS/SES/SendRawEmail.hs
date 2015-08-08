{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendRawEmail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an email message, with header and content specified by the client.
-- The @SendRawEmail@ action is useful for sending multipart MIME emails.
-- The raw text of the message must comply with Internet email standards;
-- otherwise, the message cannot be sent.
--
-- There are several important points to know about @SendRawEmail@:
--
-- -   You can only send email from verified email addresses and domains;
--     otherwise, you will get an \"Email address not verified\" error. If
--     your account is still in the Amazon SES sandbox, you must also
--     verify every recipient email address except for the recipients
--     provided by the Amazon SES mailbox simulator. For more information,
--     go to the
--     <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide>.
-- -   The total size of the message cannot exceed 10 MB. This includes any
--     attachments that are part of the message.
-- -   Amazon SES has a limit on the total number of recipients per
--     message. The combined number of To:, CC: and BCC: email addresses
--     cannot exceed 50. If you need to send an email message to a larger
--     audience, you can divide your recipient list into groups of 50 or
--     fewer, and then call Amazon SES repeatedly to send the message to
--     each group.
-- -   The To:, CC:, and BCC: headers in the raw message can contain a
--     group list. Note that each recipient in a group list counts towards
--     the 50-recipient limit.
-- -   For every message that you send, the total number of recipients
--     (To:, CC: and BCC:) is counted against your sending quota - the
--     maximum number of emails you can send in a 24-hour period. For
--     information about your sending quota, go to the
--     <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html Amazon SES Developer Guide>.
-- -   If you are using sending authorization to send on behalf of another
--     user, @SendRawEmail@ enables you to specify the cross-account
--     identity for the email\'s \"Source,\" \"From,\" and \"Return-Path\"
--     parameters in one of two ways: you can pass optional parameters
--     @SourceArn@, @FromArn@, and\/or @ReturnPathArn@ to the API, or you
--     can include the following X-headers in the header of your raw email:
--     -   @X-SES-SOURCE-ARN@
--     -   @X-SES-FROM-ARN@
--     -   @X-SES-RETURN-PATH-ARN@
--
--     Do not include these X-headers in the DKIM signature, because they
--     are removed by Amazon SES before sending the email.
--     For the most common sending authorization use case, we recommend
--     that you specify the @SourceIdentityArn@ and do not specify either
--     the @FromIdentityArn@ or @ReturnPathIdentityArn@. (The same note
--     applies to the corresponding X-headers.) If you only specify the
--     @SourceIdentityArn@, Amazon SES will simply set the \"From\" address
--     and the \"Return Path\" address to the identity specified in
--     @SourceIdentityArn@. For more information about sending
--     authorization, see the
--     <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_SendRawEmail.html AWS API Reference> for SendRawEmail.
module Network.AWS.SES.SendRawEmail
    (
    -- * Creating a Request
      SendRawEmail
    , sendRawEmail
    -- * Request Lenses
    , sreSourceARN
    , sreDestinations
    , sreReturnPathARN
    , sreSource
    , sreFromARN
    , sreRawMessage

    -- * Destructuring the Response
    , SendRawEmailResponse
    , sendRawEmailResponse
    -- * Response Lenses
    , srersStatus
    , srersMessageId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to send a raw email
-- message.
--
-- This datatype can be used in application code to compose a message
-- consisting of source, destination, and raw message text. This object can
-- then be sent using the @SendRawEmail@ action.
--
-- /See:/ 'sendRawEmail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sreSourceARN'
--
-- * 'sreDestinations'
--
-- * 'sreReturnPathARN'
--
-- * 'sreSource'
--
-- * 'sreFromARN'
--
-- * 'sreRawMessage'
data SendRawEmail = SendRawEmail'
    { _sreSourceARN     :: !(Maybe Text)
    , _sreDestinations  :: !(Maybe [Text])
    , _sreReturnPathARN :: !(Maybe Text)
    , _sreSource        :: !(Maybe Text)
    , _sreFromARN       :: !(Maybe Text)
    , _sreRawMessage    :: !RawMessage
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SendRawEmail' smart constructor.
sendRawEmail :: RawMessage -> SendRawEmail
sendRawEmail pRawMessage_ =
    SendRawEmail'
    { _sreSourceARN = Nothing
    , _sreDestinations = Nothing
    , _sreReturnPathARN = Nothing
    , _sreSource = Nothing
    , _sreFromARN = Nothing
    , _sreRawMessage = pRawMessage_
    }

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to send for the email address specified in the @Source@
-- parameter.
--
-- For example, if the owner of @example.com@ (which has ARN
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
-- policy to it that authorizes you to send from @user\@example.com@, then
-- you would specify the @SourceArn@ to be
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
-- @Source@ to be @user\@example.com@.
--
-- Instead of using this parameter, you can use the X-header
-- @X-SES-SOURCE-ARN@ in the raw message of the email. If you use both the
-- @SourceArn@ parameter and the corresponding X-header, Amazon SES uses
-- the value of the @SourceArn@ parameter.
--
-- For information about when to use this parameter, see the description of
-- @SendRawEmail@ in this guide, or see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
sreSourceARN :: Lens' SendRawEmail (Maybe Text)
sreSourceARN = lens _sreSourceARN (\ s a -> s{_sreSourceARN = a});

-- | A list of destinations for the message, consisting of To:, CC:, and BCC:
-- addresses.
sreDestinations :: Lens' SendRawEmail [Text]
sreDestinations = lens _sreDestinations (\ s a -> s{_sreDestinations = a}) . _Default . _Coerce;

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the email address specified in the @ReturnPath@
-- parameter.
--
-- For example, if the owner of @example.com@ (which has ARN
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
-- policy to it that authorizes you to use @feedback\@example.com@, then
-- you would specify the @ReturnPathArn@ to be
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
-- @ReturnPath@ to be @feedback\@example.com@.
--
-- Instead of using this parameter, you can use the X-header
-- @X-SES-RETURN-PATH-ARN@ in the raw message of the email. If you use both
-- the @ReturnPathArn@ parameter and the corresponding X-header, Amazon SES
-- uses the value of the @ReturnPathArn@ parameter.
--
-- For information about when to use this parameter, see the description of
-- @SendRawEmail@ in this guide, or see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
sreReturnPathARN :: Lens' SendRawEmail (Maybe Text)
sreReturnPathARN = lens _sreReturnPathARN (\ s a -> s{_sreReturnPathARN = a});

-- | The identity\'s email address. If you do not provide a value for this
-- parameter, you must specify a \"From\" address in the raw text of the
-- message. (You can also specify both.)
--
-- By default, the string must be 7-bit ASCII. If the text must contain any
-- other characters, then you must use MIME encoded-word syntax (RFC 2047)
-- instead of a literal string. MIME encoded-word syntax uses the following
-- form: @=?charset?encoding?encoded-text?=@. For more information, see
-- <http://tools.ietf.org/html/rfc2047 RFC 2047>.
--
-- If you specify the @Source@ parameter and have feedback forwarding
-- enabled, then bounces and complaints will be sent to this email address.
-- This takes precedence over any /Return-Path/ header that you might
-- include in the raw text of the message.
sreSource :: Lens' SendRawEmail (Maybe Text)
sreSource = lens _sreSource (\ s a -> s{_sreSource = a});

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to specify a particular \"From\" address in the header
-- of the raw email.
--
-- Instead of using this parameter, you can use the X-header
-- @X-SES-FROM-ARN@ in the raw message of the email. If you use both the
-- @FromArn@ parameter and the corresponding X-header, Amazon SES uses the
-- value of the @FromArn@ parameter.
--
-- For information about when to use this parameter, see the description of
-- @SendRawEmail@ in this guide, or see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
sreFromARN :: Lens' SendRawEmail (Maybe Text)
sreFromARN = lens _sreFromARN (\ s a -> s{_sreFromARN = a});

-- | The raw text of the message. The client is responsible for ensuring the
-- following:
--
-- -   Message must contain a header and a body, separated by a blank line.
-- -   All required header fields must be present.
-- -   Each part of a multipart MIME message must be formatted properly.
-- -   MIME content types must be among those supported by Amazon SES. For
--     more information, go to the
--     <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mime-types.html Amazon SES Developer Guide>.
-- -   Content must be base64-encoded, if MIME requires it.
sreRawMessage :: Lens' SendRawEmail RawMessage
sreRawMessage = lens _sreRawMessage (\ s a -> s{_sreRawMessage = a});

instance AWSRequest SendRawEmail where
        type Sv SendRawEmail = SES
        type Rs SendRawEmail = SendRawEmailResponse
        request = postQuery
        response
          = receiveXMLWrapper "SendRawEmailResult"
              (\ s h x ->
                 SendRawEmailResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "MessageId"))

instance ToHeaders SendRawEmail where
        toHeaders = const mempty

instance ToPath SendRawEmail where
        toPath = const "/"

instance ToQuery SendRawEmail where
        toQuery SendRawEmail'{..}
          = mconcat
              ["Action" =: ("SendRawEmail" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "SourceArn" =: _sreSourceARN,
               "Destinations" =:
                 toQuery (toQueryList "member" <$> _sreDestinations),
               "ReturnPathArn" =: _sreReturnPathARN,
               "Source" =: _sreSource, "FromArn" =: _sreFromARN,
               "RawMessage" =: _sreRawMessage]

-- | Represents a unique message ID returned from a successful @SendRawEmail@
-- request.
--
-- /See:/ 'sendRawEmailResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srersStatus'
--
-- * 'srersMessageId'
data SendRawEmailResponse = SendRawEmailResponse'
    { _srersStatus    :: !Int
    , _srersMessageId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SendRawEmailResponse' smart constructor.
sendRawEmailResponse :: Int -> Text -> SendRawEmailResponse
sendRawEmailResponse pStatus_ pMessageId_ =
    SendRawEmailResponse'
    { _srersStatus = pStatus_
    , _srersMessageId = pMessageId_
    }

-- | Undocumented member.
srersStatus :: Lens' SendRawEmailResponse Int
srersStatus = lens _srersStatus (\ s a -> s{_srersStatus = a});

-- | The unique message identifier returned from the @SendRawEmail@ action.
srersMessageId :: Lens' SendRawEmailResponse Text
srersMessageId = lens _srersMessageId (\ s a -> s{_srersMessageId = a});
