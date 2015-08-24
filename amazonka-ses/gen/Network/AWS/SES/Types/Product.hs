{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.SES.Types.Sum

-- | Represents the body of the message. You can specify text, HTML, or both.
-- If you use both, then the message should display correctly in the widest
-- variety of email clients.
--
-- /See:/ 'body' smart constructor.
data Body = Body'
    { _bText :: !(Maybe Content)
    , _bHTML :: !(Maybe Content)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Body' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bText'
--
-- * 'bHTML'
body
    :: Body
body =
    Body'
    { _bText = Nothing
    , _bHTML = Nothing
    }

-- | The content of the message, in text format. Use this for text-based
-- email clients, or clients on high-latency networks (such as mobile
-- devices).
bText :: Lens' Body (Maybe Content)
bText = lens _bText (\ s a -> s{_bText = a});

-- | The content of the message, in HTML format. Use this for email clients
-- that can process HTML. You can include clickable links, formatted text,
-- and much more in an HTML message.
bHTML :: Lens' Body (Maybe Content)
bHTML = lens _bHTML (\ s a -> s{_bHTML = a});

instance ToQuery Body where
        toQuery Body'{..}
          = mconcat ["Text" =: _bText, "Html" =: _bHTML]

-- | Represents textual data, plus an optional character set specification.
--
-- By default, the text must be 7-bit ASCII, due to the constraints of the
-- SMTP protocol. If the text must contain any other characters, then you
-- must also specify a character set. Examples include UTF-8, ISO-8859-1,
-- and Shift_JIS.
--
-- /See:/ 'content' smart constructor.
data Content = Content'
    { _cCharset :: !(Maybe Text)
    , _cData    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Content' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCharset'
--
-- * 'cData'
content
    :: Text -- ^ 'cData'
    -> Content
content pData_ =
    Content'
    { _cCharset = Nothing
    , _cData = pData_
    }

-- | The character set of the content.
cCharset :: Lens' Content (Maybe Text)
cCharset = lens _cCharset (\ s a -> s{_cCharset = a});

-- | The textual data of the content.
cData :: Lens' Content Text
cData = lens _cData (\ s a -> s{_cData = a});

instance ToQuery Content where
        toQuery Content'{..}
          = mconcat ["Charset" =: _cCharset, "Data" =: _cData]

-- | Represents the destination of the message, consisting of To:, CC:, and
-- BCC: fields.
--
-- By default, the string must be 7-bit ASCII. If the text must contain any
-- other characters, then you must use MIME encoded-word syntax (RFC 2047)
-- instead of a literal string. MIME encoded-word syntax uses the following
-- form: '=?charset?encoding?encoded-text?='. For more information, see
-- <http://tools.ietf.org/html/rfc2047 RFC 2047>.
--
-- /See:/ 'destination' smart constructor.
data Destination = Destination'
    { _dBCCAddresses :: !(Maybe [Text])
    , _dCCAddresses  :: !(Maybe [Text])
    , _dToAddresses  :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dBCCAddresses'
--
-- * 'dCCAddresses'
--
-- * 'dToAddresses'
destination
    :: Destination
destination =
    Destination'
    { _dBCCAddresses = Nothing
    , _dCCAddresses = Nothing
    , _dToAddresses = Nothing
    }

-- | The BCC: field(s) of the message.
dBCCAddresses :: Lens' Destination [Text]
dBCCAddresses = lens _dBCCAddresses (\ s a -> s{_dBCCAddresses = a}) . _Default . _Coerce;

-- | The CC: field(s) of the message.
dCCAddresses :: Lens' Destination [Text]
dCCAddresses = lens _dCCAddresses (\ s a -> s{_dCCAddresses = a}) . _Default . _Coerce;

-- | The To: field(s) of the message.
dToAddresses :: Lens' Destination [Text]
dToAddresses = lens _dToAddresses (\ s a -> s{_dToAddresses = a}) . _Default . _Coerce;

instance ToQuery Destination where
        toQuery Destination'{..}
          = mconcat
              ["BccAddresses" =:
                 toQuery (toQueryList "member" <$> _dBCCAddresses),
               "CcAddresses" =:
                 toQuery (toQueryList "member" <$> _dCCAddresses),
               "ToAddresses" =:
                 toQuery (toQueryList "member" <$> _dToAddresses)]

-- | Represents the DKIM attributes of a verified email address or a domain.
--
-- /See:/ 'identityDkimAttributes' smart constructor.
data IdentityDkimAttributes = IdentityDkimAttributes'
    { _idaDkimTokens             :: !(Maybe [Text])
    , _idaDkimEnabled            :: !Bool
    , _idaDkimVerificationStatus :: !VerificationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IdentityDkimAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idaDkimTokens'
--
-- * 'idaDkimEnabled'
--
-- * 'idaDkimVerificationStatus'
identityDkimAttributes
    :: Bool -- ^ 'idaDkimEnabled'
    -> VerificationStatus -- ^ 'idaDkimVerificationStatus'
    -> IdentityDkimAttributes
identityDkimAttributes pDkimEnabled_ pDkimVerificationStatus_ =
    IdentityDkimAttributes'
    { _idaDkimTokens = Nothing
    , _idaDkimEnabled = pDkimEnabled_
    , _idaDkimVerificationStatus = pDkimVerificationStatus_
    }

-- | A set of character strings that represent the domain\'s identity. Using
-- these tokens, you will need to create DNS CNAME records that point to
-- DKIM public keys hosted by Amazon SES. Amazon Web Services will
-- eventually detect that you have updated your DNS records; this detection
-- process may take up to 72 hours. Upon successful detection, Amazon SES
-- will be able to DKIM-sign email originating from that domain. (This only
-- applies to domain identities, not email address identities.)
--
-- For more information about creating DNS records using DKIM tokens, go to
-- the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide>.
idaDkimTokens :: Lens' IdentityDkimAttributes [Text]
idaDkimTokens = lens _idaDkimTokens (\ s a -> s{_idaDkimTokens = a}) . _Default . _Coerce;

-- | True if DKIM signing is enabled for email sent from the identity; false
-- otherwise.
idaDkimEnabled :: Lens' IdentityDkimAttributes Bool
idaDkimEnabled = lens _idaDkimEnabled (\ s a -> s{_idaDkimEnabled = a});

-- | Describes whether Amazon SES has successfully verified the DKIM DNS
-- records (tokens) published in the domain name\'s DNS. (This only applies
-- to domain identities, not email address identities.)
idaDkimVerificationStatus :: Lens' IdentityDkimAttributes VerificationStatus
idaDkimVerificationStatus = lens _idaDkimVerificationStatus (\ s a -> s{_idaDkimVerificationStatus = a});

instance FromXML IdentityDkimAttributes where
        parseXML x
          = IdentityDkimAttributes' <$>
              (x .@? "DkimTokens" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@ "DkimEnabled")
                <*> (x .@ "DkimVerificationStatus")

-- | Represents the notification attributes of an identity, including whether
-- an identity has Amazon Simple Notification Service (Amazon SNS) topics
-- set for bounce, complaint, and\/or delivery notifications, and whether
-- feedback forwarding is enabled for bounce and complaint notifications.
--
-- /See:/ 'identityNotificationAttributes' smart constructor.
data IdentityNotificationAttributes = IdentityNotificationAttributes'
    { _inaBounceTopic       :: !Text
    , _inaComplaintTopic    :: !Text
    , _inaDeliveryTopic     :: !Text
    , _inaForwardingEnabled :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IdentityNotificationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'inaBounceTopic'
--
-- * 'inaComplaintTopic'
--
-- * 'inaDeliveryTopic'
--
-- * 'inaForwardingEnabled'
identityNotificationAttributes
    :: Text -- ^ 'inaBounceTopic'
    -> Text -- ^ 'inaComplaintTopic'
    -> Text -- ^ 'inaDeliveryTopic'
    -> Bool -- ^ 'inaForwardingEnabled'
    -> IdentityNotificationAttributes
identityNotificationAttributes pBounceTopic_ pComplaintTopic_ pDeliveryTopic_ pForwardingEnabled_ =
    IdentityNotificationAttributes'
    { _inaBounceTopic = pBounceTopic_
    , _inaComplaintTopic = pComplaintTopic_
    , _inaDeliveryTopic = pDeliveryTopic_
    , _inaForwardingEnabled = pForwardingEnabled_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish bounce notifications.
inaBounceTopic :: Lens' IdentityNotificationAttributes Text
inaBounceTopic = lens _inaBounceTopic (\ s a -> s{_inaBounceTopic = a});

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish complaint notifications.
inaComplaintTopic :: Lens' IdentityNotificationAttributes Text
inaComplaintTopic = lens _inaComplaintTopic (\ s a -> s{_inaComplaintTopic = a});

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish delivery notifications.
inaDeliveryTopic :: Lens' IdentityNotificationAttributes Text
inaDeliveryTopic = lens _inaDeliveryTopic (\ s a -> s{_inaDeliveryTopic = a});

-- | Describes whether Amazon SES will forward bounce and complaint
-- notifications as email. 'true' indicates that Amazon SES will forward
-- bounce and complaint notifications as email, while 'false' indicates
-- that bounce and complaint notifications will be published only to the
-- specified bounce and complaint Amazon SNS topics.
inaForwardingEnabled :: Lens' IdentityNotificationAttributes Bool
inaForwardingEnabled = lens _inaForwardingEnabled (\ s a -> s{_inaForwardingEnabled = a});

instance FromXML IdentityNotificationAttributes where
        parseXML x
          = IdentityNotificationAttributes' <$>
              (x .@ "BounceTopic") <*> (x .@ "ComplaintTopic") <*>
                (x .@ "DeliveryTopic")
                <*> (x .@ "ForwardingEnabled")

-- | Represents the verification attributes of a single identity.
--
-- /See:/ 'identityVerificationAttributes' smart constructor.
data IdentityVerificationAttributes = IdentityVerificationAttributes'
    { _ivaVerificationToken  :: !(Maybe Text)
    , _ivaVerificationStatus :: !VerificationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IdentityVerificationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivaVerificationToken'
--
-- * 'ivaVerificationStatus'
identityVerificationAttributes
    :: VerificationStatus -- ^ 'ivaVerificationStatus'
    -> IdentityVerificationAttributes
identityVerificationAttributes pVerificationStatus_ =
    IdentityVerificationAttributes'
    { _ivaVerificationToken = Nothing
    , _ivaVerificationStatus = pVerificationStatus_
    }

-- | The verification token for a domain identity. Null for email address
-- identities.
ivaVerificationToken :: Lens' IdentityVerificationAttributes (Maybe Text)
ivaVerificationToken = lens _ivaVerificationToken (\ s a -> s{_ivaVerificationToken = a});

-- | The verification status of the identity: \"Pending\", \"Success\",
-- \"Failed\", or \"TemporaryFailure\".
ivaVerificationStatus :: Lens' IdentityVerificationAttributes VerificationStatus
ivaVerificationStatus = lens _ivaVerificationStatus (\ s a -> s{_ivaVerificationStatus = a});

instance FromXML IdentityVerificationAttributes where
        parseXML x
          = IdentityVerificationAttributes' <$>
              (x .@? "VerificationToken") <*>
                (x .@ "VerificationStatus")

-- | Represents the message to be sent, composed of a subject and a body.
--
-- /See:/ 'message' smart constructor.
data Message = Message'
    { _mSubject :: !Content
    , _mBody    :: !Body
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mSubject'
--
-- * 'mBody'
message
    :: Content -- ^ 'mSubject'
    -> Body -- ^ 'mBody'
    -> Message
message pSubject_ pBody_ =
    Message'
    { _mSubject = pSubject_
    , _mBody = pBody_
    }

-- | The subject of the message: A short summary of the content, which will
-- appear in the recipient\'s inbox.
mSubject :: Lens' Message Content
mSubject = lens _mSubject (\ s a -> s{_mSubject = a});

-- | The message body.
mBody :: Lens' Message Body
mBody = lens _mBody (\ s a -> s{_mBody = a});

instance ToQuery Message where
        toQuery Message'{..}
          = mconcat ["Subject" =: _mSubject, "Body" =: _mBody]

-- | Represents the raw data of the message.
--
-- /See:/ 'rawMessage' smart constructor.
newtype RawMessage = RawMessage'
    { _rmData :: Base64
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RawMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmData'
rawMessage
    :: ByteString -- ^ 'rmData'
    -> RawMessage
rawMessage pData_ =
    RawMessage'
    { _rmData = _Base64 # pData_
    }

-- | The raw data of the message. The client must ensure that the message
-- format complies with Internet email standards regarding email header
-- fields, MIME types, MIME encoding, and base64 encoding (if necessary).
--
-- The To:, CC:, and BCC: headers in the raw message can contain a group
-- list.
--
-- If you are using 'SendRawEmail' with sending authorization, you can
-- include X-headers in the raw message to specify the \"Source,\"
-- \"From,\" and \"Return-Path\" addresses. For more information, see the
-- documentation for 'SendRawEmail'.
--
-- Do not include these X-headers in the DKIM signature, because they are
-- removed by Amazon SES before sending the email.
--
-- For more information, go to the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide>.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
rmData :: Lens' RawMessage ByteString
rmData = lens _rmData (\ s a -> s{_rmData = a}) . _Base64;

instance ToQuery RawMessage where
        toQuery RawMessage'{..} = mconcat ["Data" =: _rmData]

-- | Represents sending statistics data. Each 'SendDataPoint' contains
-- statistics for a 15-minute period of sending activity.
--
-- /See:/ 'sendDataPoint' smart constructor.
data SendDataPoint = SendDataPoint'
    { _sdpRejects          :: !(Maybe Integer)
    , _sdpComplaints       :: !(Maybe Integer)
    , _sdpDeliveryAttempts :: !(Maybe Integer)
    , _sdpBounces          :: !(Maybe Integer)
    , _sdpTimestamp        :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SendDataPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdpRejects'
--
-- * 'sdpComplaints'
--
-- * 'sdpDeliveryAttempts'
--
-- * 'sdpBounces'
--
-- * 'sdpTimestamp'
sendDataPoint
    :: SendDataPoint
sendDataPoint =
    SendDataPoint'
    { _sdpRejects = Nothing
    , _sdpComplaints = Nothing
    , _sdpDeliveryAttempts = Nothing
    , _sdpBounces = Nothing
    , _sdpTimestamp = Nothing
    }

-- | Number of emails rejected by Amazon SES.
sdpRejects :: Lens' SendDataPoint (Maybe Integer)
sdpRejects = lens _sdpRejects (\ s a -> s{_sdpRejects = a});

-- | Number of unwanted emails that were rejected by recipients.
sdpComplaints :: Lens' SendDataPoint (Maybe Integer)
sdpComplaints = lens _sdpComplaints (\ s a -> s{_sdpComplaints = a});

-- | Number of emails that have been enqueued for sending.
sdpDeliveryAttempts :: Lens' SendDataPoint (Maybe Integer)
sdpDeliveryAttempts = lens _sdpDeliveryAttempts (\ s a -> s{_sdpDeliveryAttempts = a});

-- | Number of emails that have bounced.
sdpBounces :: Lens' SendDataPoint (Maybe Integer)
sdpBounces = lens _sdpBounces (\ s a -> s{_sdpBounces = a});

-- | Time of the data point.
sdpTimestamp :: Lens' SendDataPoint (Maybe UTCTime)
sdpTimestamp = lens _sdpTimestamp (\ s a -> s{_sdpTimestamp = a}) . mapping _Time;

instance FromXML SendDataPoint where
        parseXML x
          = SendDataPoint' <$>
              (x .@? "Rejects") <*> (x .@? "Complaints") <*>
                (x .@? "DeliveryAttempts")
                <*> (x .@? "Bounces")
                <*> (x .@? "Timestamp")
