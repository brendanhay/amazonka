{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.SES.Types
    (
    -- * Service
      SES

    -- * Errors
    , _MessageRejected

    -- * IdentityType
    , IdentityType (..)

    -- * NotificationType
    , NotificationType (..)

    -- * VerificationStatus
    , VerificationStatus (..)

    -- * Body
    , Body
    , body
    , bodText
    , bodHTML

    -- * Content
    , Content
    , content
    , conCharset
    , conData

    -- * Destination
    , Destination
    , destination
    , desBCCAddresses
    , desCCAddresses
    , desToAddresses

    -- * IdentityDkimAttributes
    , IdentityDkimAttributes
    , identityDkimAttributes
    , idaDkimTokens
    , idaDkimEnabled
    , idaDkimVerificationStatus

    -- * IdentityNotificationAttributes
    , IdentityNotificationAttributes
    , identityNotificationAttributes
    , inaBounceTopic
    , inaComplaintTopic
    , inaDeliveryTopic
    , inaForwardingEnabled

    -- * IdentityVerificationAttributes
    , IdentityVerificationAttributes
    , identityVerificationAttributes
    , ivaVerificationToken
    , ivaVerificationStatus

    -- * Message
    , Message
    , message
    , mesSubject
    , mesBody

    -- * RawMessage
    , RawMessage
    , rawMessage
    , rmData

    -- * SendDataPoint
    , SendDataPoint
    , sendDataPoint
    , sdpRejects
    , sdpComplaints
    , sdpDeliveryAttempts
    , sdpBounces
    , sdpTimestamp
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2010-12-01@ of the Amazon Simple Email Service SDK.
data SES

instance AWSService SES where
    type Sg SES = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "SES"
            , _svcPrefix = "email"
            , _svcVersion = "2010-12-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Indicates that the action failed, and the message could not be sent.
-- Check the error stack for more information about what caused the error.
_MessageRejected :: AWSError a => Getting (First ServiceError) a ServiceError
_MessageRejected = _ServiceError . hasStatus 400 . hasCode "MessageRejected"

data IdentityType
    = Domain
    | EmailAddress
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText IdentityType where
    parser = takeLowerText >>= \case
        "domain" -> pure Domain
        "emailaddress" -> pure EmailAddress
        e -> fromTextError $ "Failure parsing IdentityType from value: '" <> e
           <> "'. Accepted values: domain, emailaddress"

instance ToText IdentityType where
    toText = \case
        Domain -> "domain"
        EmailAddress -> "emailaddress"

instance Hashable IdentityType where
    hashWithSalt = hashUsing fromEnum

instance ToQuery IdentityType
instance ToHeader IdentityType

data NotificationType
    = Delivery
    | Bounce
    | Complaint
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText NotificationType where
    parser = takeLowerText >>= \case
        "bounce" -> pure Bounce
        "complaint" -> pure Complaint
        "delivery" -> pure Delivery
        e -> fromTextError $ "Failure parsing NotificationType from value: '" <> e
           <> "'. Accepted values: bounce, complaint, delivery"

instance ToText NotificationType where
    toText = \case
        Bounce -> "bounce"
        Complaint -> "complaint"
        Delivery -> "delivery"

instance Hashable NotificationType where
    hashWithSalt = hashUsing fromEnum

instance ToQuery NotificationType
instance ToHeader NotificationType

data VerificationStatus
    = NotStarted
    | Pending
    | Success
    | TemporaryFailure
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText VerificationStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "notstarted" -> pure NotStarted
        "pending" -> pure Pending
        "success" -> pure Success
        "temporaryfailure" -> pure TemporaryFailure
        e -> fromTextError $ "Failure parsing VerificationStatus from value: '" <> e
           <> "'. Accepted values: failed, notstarted, pending, success, temporaryfailure"

instance ToText VerificationStatus where
    toText = \case
        Failed -> "failed"
        NotStarted -> "notstarted"
        Pending -> "pending"
        Success -> "success"
        TemporaryFailure -> "temporaryfailure"

instance Hashable VerificationStatus where
    hashWithSalt = hashUsing fromEnum

instance ToQuery VerificationStatus
instance ToHeader VerificationStatus

instance FromXML VerificationStatus where
    parseXML = parseXMLText "VerificationStatus"

-- | Represents the body of the message. You can specify text, HTML, or both.
-- If you use both, then the message should display correctly in the widest
-- variety of email clients.
--
-- /See:/ 'body' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bodText'
--
-- * 'bodHTML'
data Body = Body'
    { _bodText :: !(Maybe Content)
    , _bodHTML :: !(Maybe Content)
    } deriving (Eq,Read,Show)

-- | 'Body' smart constructor.
body :: Body
body =
    Body'
    { _bodText = Nothing
    , _bodHTML = Nothing
    }

-- | The content of the message, in text format. Use this for text-based
-- email clients, or clients on high-latency networks (such as mobile
-- devices).
bodText :: Lens' Body (Maybe Content)
bodText = lens _bodText (\ s a -> s{_bodText = a});

-- | The content of the message, in HTML format. Use this for email clients
-- that can process HTML. You can include clickable links, formatted text,
-- and much more in an HTML message.
bodHTML :: Lens' Body (Maybe Content)
bodHTML = lens _bodHTML (\ s a -> s{_bodHTML = a});

instance ToQuery Body where
        toQuery Body'{..}
          = mconcat ["Text" =: _bodText, "Html" =: _bodHTML]

-- | Represents textual data, plus an optional character set specification.
--
-- By default, the text must be 7-bit ASCII, due to the constraints of the
-- SMTP protocol. If the text must contain any other characters, then you
-- must also specify a character set. Examples include UTF-8, ISO-8859-1,
-- and Shift_JIS.
--
-- /See:/ 'content' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'conCharset'
--
-- * 'conData'
data Content = Content'
    { _conCharset :: !(Maybe Text)
    , _conData    :: !Text
    } deriving (Eq,Read,Show)

-- | 'Content' smart constructor.
content :: Text -> Content
content pData =
    Content'
    { _conCharset = Nothing
    , _conData = pData
    }

-- | The character set of the content.
conCharset :: Lens' Content (Maybe Text)
conCharset = lens _conCharset (\ s a -> s{_conCharset = a});

-- | The textual data of the content.
conData :: Lens' Content Text
conData = lens _conData (\ s a -> s{_conData = a});

instance ToQuery Content where
        toQuery Content'{..}
          = mconcat
              ["Charset" =: _conCharset, "Data" =: _conData]

-- | Represents the destination of the message, consisting of To:, CC:, and
-- BCC: fields.
--
-- By default, the string must be 7-bit ASCII. If the text must contain any
-- other characters, then you must use MIME encoded-word syntax (RFC 2047)
-- instead of a literal string. MIME encoded-word syntax uses the following
-- form: @=?charset?encoding?encoded-text?=@. For more information, see
-- <http://tools.ietf.org/html/rfc2047 RFC 2047>.
--
-- /See:/ 'destination' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desBCCAddresses'
--
-- * 'desCCAddresses'
--
-- * 'desToAddresses'
data Destination = Destination'
    { _desBCCAddresses :: !(Maybe [Text])
    , _desCCAddresses  :: !(Maybe [Text])
    , _desToAddresses  :: !(Maybe [Text])
    } deriving (Eq,Read,Show)

-- | 'Destination' smart constructor.
destination :: Destination
destination =
    Destination'
    { _desBCCAddresses = Nothing
    , _desCCAddresses = Nothing
    , _desToAddresses = Nothing
    }

-- | The BCC: field(s) of the message.
desBCCAddresses :: Lens' Destination [Text]
desBCCAddresses = lens _desBCCAddresses (\ s a -> s{_desBCCAddresses = a}) . _Default;

-- | The CC: field(s) of the message.
desCCAddresses :: Lens' Destination [Text]
desCCAddresses = lens _desCCAddresses (\ s a -> s{_desCCAddresses = a}) . _Default;

-- | The To: field(s) of the message.
desToAddresses :: Lens' Destination [Text]
desToAddresses = lens _desToAddresses (\ s a -> s{_desToAddresses = a}) . _Default;

instance ToQuery Destination where
        toQuery Destination'{..}
          = mconcat
              ["BccAddresses" =:
                 toQuery (toQueryList "member" <$> _desBCCAddresses),
               "CcAddresses" =:
                 toQuery (toQueryList "member" <$> _desCCAddresses),
               "ToAddresses" =:
                 toQuery (toQueryList "member" <$> _desToAddresses)]

-- | Represents the DKIM attributes of a verified email address or a domain.
--
-- /See:/ 'identityDkimAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idaDkimTokens'
--
-- * 'idaDkimEnabled'
--
-- * 'idaDkimVerificationStatus'
data IdentityDkimAttributes = IdentityDkimAttributes'
    { _idaDkimTokens             :: !(Maybe [Text])
    , _idaDkimEnabled            :: !Bool
    , _idaDkimVerificationStatus :: !VerificationStatus
    } deriving (Eq,Read,Show)

-- | 'IdentityDkimAttributes' smart constructor.
identityDkimAttributes :: Bool -> VerificationStatus -> IdentityDkimAttributes
identityDkimAttributes pDkimEnabled pDkimVerificationStatus =
    IdentityDkimAttributes'
    { _idaDkimTokens = Nothing
    , _idaDkimEnabled = pDkimEnabled
    , _idaDkimVerificationStatus = pDkimVerificationStatus
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
idaDkimTokens = lens _idaDkimTokens (\ s a -> s{_idaDkimTokens = a}) . _Default;

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'inaBounceTopic'
--
-- * 'inaComplaintTopic'
--
-- * 'inaDeliveryTopic'
--
-- * 'inaForwardingEnabled'
data IdentityNotificationAttributes = IdentityNotificationAttributes'
    { _inaBounceTopic       :: !Text
    , _inaComplaintTopic    :: !Text
    , _inaDeliveryTopic     :: !Text
    , _inaForwardingEnabled :: !Bool
    } deriving (Eq,Read,Show)

-- | 'IdentityNotificationAttributes' smart constructor.
identityNotificationAttributes :: Text -> Text -> Text -> Bool -> IdentityNotificationAttributes
identityNotificationAttributes pBounceTopic pComplaintTopic pDeliveryTopic pForwardingEnabled =
    IdentityNotificationAttributes'
    { _inaBounceTopic = pBounceTopic
    , _inaComplaintTopic = pComplaintTopic
    , _inaDeliveryTopic = pDeliveryTopic
    , _inaForwardingEnabled = pForwardingEnabled
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
-- notifications as email. @true@ indicates that Amazon SES will forward
-- bounce and complaint notifications as email, while @false@ indicates
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivaVerificationToken'
--
-- * 'ivaVerificationStatus'
data IdentityVerificationAttributes = IdentityVerificationAttributes'
    { _ivaVerificationToken  :: !(Maybe Text)
    , _ivaVerificationStatus :: !VerificationStatus
    } deriving (Eq,Read,Show)

-- | 'IdentityVerificationAttributes' smart constructor.
identityVerificationAttributes :: VerificationStatus -> IdentityVerificationAttributes
identityVerificationAttributes pVerificationStatus =
    IdentityVerificationAttributes'
    { _ivaVerificationToken = Nothing
    , _ivaVerificationStatus = pVerificationStatus
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mesSubject'
--
-- * 'mesBody'
data Message = Message'
    { _mesSubject :: !Content
    , _mesBody    :: !Body
    } deriving (Eq,Read,Show)

-- | 'Message' smart constructor.
message :: Content -> Body -> Message
message pSubject pBody =
    Message'
    { _mesSubject = pSubject
    , _mesBody = pBody
    }

-- | The subject of the message: A short summary of the content, which will
-- appear in the recipient\'s inbox.
mesSubject :: Lens' Message Content
mesSubject = lens _mesSubject (\ s a -> s{_mesSubject = a});

-- | The message body.
mesBody :: Lens' Message Body
mesBody = lens _mesBody (\ s a -> s{_mesBody = a});

instance ToQuery Message where
        toQuery Message'{..}
          = mconcat
              ["Subject" =: _mesSubject, "Body" =: _mesBody]

-- | Represents the raw data of the message.
--
-- /See:/ 'rawMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmData'
newtype RawMessage = RawMessage'
    { _rmData :: Base64
    } deriving (Eq,Read,Show)

-- | 'RawMessage' smart constructor.
rawMessage :: Base64 -> RawMessage
rawMessage pData =
    RawMessage'
    { _rmData = pData
    }

-- | The raw data of the message. The client must ensure that the message
-- format complies with Internet email standards regarding email header
-- fields, MIME types, MIME encoding, and base64 encoding (if necessary).
--
-- The To:, CC:, and BCC: headers in the raw message can contain a group
-- list.
--
-- For more information, go to the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide>.
rmData :: Lens' RawMessage Base64
rmData = lens _rmData (\ s a -> s{_rmData = a});

instance ToQuery RawMessage where
        toQuery RawMessage'{..} = mconcat ["Data" =: _rmData]

-- | Represents sending statistics data. Each @SendDataPoint@ contains
-- statistics for a 15-minute period of sending activity.
--
-- /See:/ 'sendDataPoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data SendDataPoint = SendDataPoint'
    { _sdpRejects          :: !(Maybe Integer)
    , _sdpComplaints       :: !(Maybe Integer)
    , _sdpDeliveryAttempts :: !(Maybe Integer)
    , _sdpBounces          :: !(Maybe Integer)
    , _sdpTimestamp        :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show)

-- | 'SendDataPoint' smart constructor.
sendDataPoint :: SendDataPoint
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
