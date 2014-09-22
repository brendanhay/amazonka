{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Email Service (Amazon SES) is a cost-effective outbound-only
-- email-sending service built on the reliable and scalable infrastructure
-- that Amazon.com has developed to serve its own customer base. With Amazon
-- SES, you can send transactional email, marketing messages, or any other
-- type of high-quality content and you only pay for what you use. Along with
-- high deliverability, Amazon SES provides easy, real-time access to your
-- sending statistics and built-in notifications for bounces, complaints, and
-- deliveries to help you fine-tune your email-sending strategy.
module Network.AWS.SES.Types
    (
    -- * Service
      SES
    -- ** Errors
    , SESError (..)
    , _MessageRejected
    , _SESClient
    , _SESSerializer
    , _SESService
    -- ** XML
    , xmlOptions

    -- * IdentityType
    , IdentityType (..)

    -- * NotificationType
    , NotificationType (..)

    -- * VerificationStatus
    , VerificationStatus (..)

    -- * RawMessage
    , RawMessage
    , rawMessage
    , rmData

    -- * Body
    , Body
    , body
    , bText
    , bHtml

    -- * Content
    , Content
    , content
    , cData
    , cCharset

    -- * Destination
    , Destination
    , destination
    , dToAddresses
    , dCcAddresses
    , dBccAddresses

    -- * IdentityDkimAttributes
    , IdentityDkimAttributes
    , identityDkimAttributes
    , idaDkimEnabled
    , idaDkimVerificationStatus
    , idaDkimTokens

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
    , ivaVerificationStatus
    , ivaVerificationToken

    -- * Message
    , Message
    , message
    , mSubject
    , mBody

    -- * SendDataPoint
    , SendDataPoint
    , sendDataPoint
    , sdpTimestamp
    , sdpDeliveryAttempts
    , sdpBounces
    , sdpComplaints
    , sdpRejects
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2010-12-01@) of the
-- @Amazon Simple Email Service@ service.
data SES deriving (Typeable)

instance AWSService SES where
    type Sg SES = V4
    type Er SES = SESError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "email"
        , _svcVersion  = "2010-12-01"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'SES' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data SESError
      -- | Indicates that the action failed, and the message could not be
      -- sent. Check the error stack for more information about what
      -- caused the error.
    = MessageRejected
    | SESClient HttpException
    | SESSerializer String
    | SESService String
      deriving (Show, Typeable, Generic)

instance AWSError SESError where
    awsError = const "SESError"

instance AWSServiceError SESError where
    serviceError    = SESService
    clientError     = SESClient
    serializerError = SESSerializer

instance Exception SESError

-- | Indicates that the action failed, and the message could not be sent. Check
-- the error stack for more information about what caused the error.
--
-- See: 'MessageRejected'
_MessageRejected :: Prism' SESError ()
_MessageRejected = prism
    (const MessageRejected)
    (\case
        MessageRejected -> Right ()
        x -> Left x)

-- | See: 'SESClient'
_SESClient :: Prism' SESError HttpException
_SESClient = prism
    SESClient
    (\case
        SESClient p1 -> Right p1
        x -> Left x)

-- | See: 'SESSerializer'
_SESSerializer :: Prism' SESError String
_SESSerializer = prism
    SESSerializer
    (\case
        SESSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'SESService'
_SESService :: Prism' SESError String
_SESService = prism
    SESService
    (\case
        SESService p1 -> Right p1
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

data IdentityType
    = IdentityTypeDomain -- ^ Domain
    | IdentityTypeEmailAddress -- ^ EmailAddress
      deriving (Eq, Ord, Show, Generic)

instance Hashable IdentityType

instance FromText IdentityType where
    parser = match "Domain" IdentityTypeDomain
         <|> match "EmailAddress" IdentityTypeEmailAddress

instance ToText IdentityType where
    toText IdentityTypeDomain = "Domain"
    toText IdentityTypeEmailAddress = "EmailAddress"

instance ToByteString IdentityType

instance ToQuery IdentityType where
    toQuery = genericQuery def

data NotificationType
    = NotificationTypeBounce -- ^ Bounce
    | NotificationTypeComplaint -- ^ Complaint
    | NotificationTypeDelivery -- ^ Delivery
      deriving (Eq, Ord, Show, Generic)

instance Hashable NotificationType

instance FromText NotificationType where
    parser = match "Bounce" NotificationTypeBounce
         <|> match "Complaint" NotificationTypeComplaint
         <|> match "Delivery" NotificationTypeDelivery

instance ToText NotificationType where
    toText NotificationTypeBounce = "Bounce"
    toText NotificationTypeComplaint = "Complaint"
    toText NotificationTypeDelivery = "Delivery"

instance ToByteString NotificationType

instance ToQuery NotificationType where
    toQuery = genericQuery def

data VerificationStatus
    = VerificationStatusFailed -- ^ Failed
    | VerificationStatusNotStarted -- ^ NotStarted
    | VerificationStatusPending -- ^ Pending
    | VerificationStatusSuccess -- ^ Success
    | VerificationStatusTemporaryFailure -- ^ TemporaryFailure
      deriving (Eq, Ord, Show, Generic)

instance Hashable VerificationStatus

instance FromText VerificationStatus where
    parser = match "Failed" VerificationStatusFailed
         <|> match "NotStarted" VerificationStatusNotStarted
         <|> match "Pending" VerificationStatusPending
         <|> match "Success" VerificationStatusSuccess
         <|> match "TemporaryFailure" VerificationStatusTemporaryFailure

instance ToText VerificationStatus where
    toText VerificationStatusFailed = "Failed"
    toText VerificationStatusNotStarted = "NotStarted"
    toText VerificationStatusPending = "Pending"
    toText VerificationStatusSuccess = "Success"
    toText VerificationStatusTemporaryFailure = "TemporaryFailure"

instance ToByteString VerificationStatus

instance FromXML VerificationStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VerificationStatus"

instance ToQuery VerificationStatus where
    toQuery = genericQuery def

-- | The raw text of the message. The client is responsible for ensuring the
-- following: Message must contain a header and a body, separated by a blank
-- line. All required header fields must be present. Each part of a multipart
-- MIME message must be formatted properly. MIME content types must be among
-- those supported by Amazon SES. For more information, go to the Amazon SES
-- Developer Guide. Content must be base64-encoded, if MIME requires it.
newtype RawMessage = RawMessage
    { _rmData :: ByteString
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RawMessage' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Data ::@ @ByteString@
--
rawMessage :: ByteString -- ^ 'rmData'
           -> RawMessage
rawMessage p1 = RawMessage
    { _rmData = p1
    }

-- | The raw data of the message. The client must ensure that the message format
-- complies with Internet email standards regarding email header fields, MIME
-- types, MIME encoding, and base64 encoding (if necessary). The To:, CC:, and
-- BCC: headers in the raw message can contain a group list. For more
-- information, go to the Amazon SES Developer Guide.
rmData :: Lens' RawMessage ByteString
rmData = lens _rmData (\s a -> s { _rmData = a })

instance ToQuery RawMessage where
    toQuery = genericQuery def

-- | The message body.
data Body = Body
    { _bText :: Maybe Content
    , _bHtml :: Maybe Content
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Body' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Text ::@ @Maybe Content@
--
-- * @Html ::@ @Maybe Content@
--
body :: Body
body = Body
    { _bText = Nothing
    , _bHtml = Nothing
    }

-- | The content of the message, in text format. Use this for text-based email
-- clients, or clients on high-latency networks (such as mobile devices).
bText :: Lens' Body (Maybe Content)
bText = lens _bText (\s a -> s { _bText = a })

-- | The content of the message, in HTML format. Use this for email clients that
-- can process HTML. You can include clickable links, formatted text, and much
-- more in an HTML message.
bHtml :: Lens' Body (Maybe Content)
bHtml = lens _bHtml (\s a -> s { _bHtml = a })

instance ToQuery Body where
    toQuery = genericQuery def

-- | The content of the message, in HTML format. Use this for email clients that
-- can process HTML. You can include clickable links, formatted text, and much
-- more in an HTML message.
data Content = Content
    { _cData :: Text
    , _cCharset :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Content' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Data ::@ @Text@
--
-- * @Charset ::@ @Maybe Text@
--
content :: Text -- ^ 'cData'
        -> Content
content p1 = Content
    { _cData = p1
    , _cCharset = Nothing
    }

-- | The textual data of the content.
cData :: Lens' Content Text
cData = lens _cData (\s a -> s { _cData = a })

-- | The character set of the content.
cCharset :: Lens' Content (Maybe Text)
cCharset = lens _cCharset (\s a -> s { _cCharset = a })

instance ToQuery Content where
    toQuery = genericQuery def

-- | The destination for this email, composed of To:, CC:, and BCC: fields.
data Destination = Destination
    { _dToAddresses :: [Text]
    , _dCcAddresses :: [Text]
    , _dBccAddresses :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Destination' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ToAddresses ::@ @[Text]@
--
-- * @CcAddresses ::@ @[Text]@
--
-- * @BccAddresses ::@ @[Text]@
--
destination :: Destination
destination = Destination
    { _dToAddresses = mempty
    , _dCcAddresses = mempty
    , _dBccAddresses = mempty
    }

-- | The To: field(s) of the message.
dToAddresses :: Lens' Destination [Text]
dToAddresses = lens _dToAddresses (\s a -> s { _dToAddresses = a })

-- | The CC: field(s) of the message.
dCcAddresses :: Lens' Destination [Text]
dCcAddresses = lens _dCcAddresses (\s a -> s { _dCcAddresses = a })

-- | The BCC: field(s) of the message.
dBccAddresses :: Lens' Destination [Text]
dBccAddresses = lens _dBccAddresses (\s a -> s { _dBccAddresses = a })

instance ToQuery Destination where
    toQuery = genericQuery def

-- | Represents the DKIM attributes of a verified email address or a domain.
data IdentityDkimAttributes = IdentityDkimAttributes
    { _idaDkimEnabled :: !Bool
    , _idaDkimVerificationStatus :: VerificationStatus
    , _idaDkimTokens :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IdentityDkimAttributes' data type.
--
-- 'IdentityDkimAttributes' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DkimEnabled ::@ @Bool@
--
-- * @DkimVerificationStatus ::@ @VerificationStatus@
--
-- * @DkimTokens ::@ @[Text]@
--
identityDkimAttributes :: Bool -- ^ 'idaDkimEnabled'
                       -> VerificationStatus -- ^ 'idaDkimVerificationStatus'
                       -> IdentityDkimAttributes
identityDkimAttributes p1 p2 = IdentityDkimAttributes
    { _idaDkimEnabled = p1
    , _idaDkimVerificationStatus = p2
    , _idaDkimTokens = mempty
    }

-- | True if DKIM signing is enabled for email sent from the identity; false
-- otherwise.
idaDkimEnabled :: Lens' IdentityDkimAttributes Bool
idaDkimEnabled = lens _idaDkimEnabled (\s a -> s { _idaDkimEnabled = a })

-- | Describes whether Amazon SES has successfully verified the DKIM DNS records
-- (tokens) published in the domain name's DNS. (This only applies to domain
-- identities, not email address identities.).
idaDkimVerificationStatus :: Lens' IdentityDkimAttributes VerificationStatus
idaDkimVerificationStatus =
    lens _idaDkimVerificationStatus
         (\s a -> s { _idaDkimVerificationStatus = a })

-- | A set of character strings that represent the domain's identity. Using
-- these tokens, you will need to create DNS CNAME records that point to DKIM
-- public keys hosted by Amazon SES. Amazon Web Services will eventually
-- detect that you have updated your DNS records; this detection process may
-- take up to 72 hours. Upon successful detection, Amazon SES will be able to
-- DKIM-sign email originating from that domain. (This only applies to domain
-- identities, not email address identities.) For more information about
-- creating DNS records using DKIM tokens, go to the Amazon SES Developer
-- Guide.
idaDkimTokens :: Lens' IdentityDkimAttributes [Text]
idaDkimTokens = lens _idaDkimTokens (\s a -> s { _idaDkimTokens = a })

instance FromXML IdentityDkimAttributes where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IdentityDkimAttributes"

-- | Represents the notification attributes of an identity, including whether an
-- identity has Amazon Simple Notification Service (Amazon SNS) topics set for
-- bounce, complaint, and/or delivery notifications, and whether feedback
-- forwarding is enabled for bounce and complaint notifications.
data IdentityNotificationAttributes = IdentityNotificationAttributes
    { _inaBounceTopic :: Text
    , _inaComplaintTopic :: Text
    , _inaDeliveryTopic :: Text
    , _inaForwardingEnabled :: !Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IdentityNotificationAttributes' data type.
--
-- 'IdentityNotificationAttributes' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @BounceTopic ::@ @Text@
--
-- * @ComplaintTopic ::@ @Text@
--
-- * @DeliveryTopic ::@ @Text@
--
-- * @ForwardingEnabled ::@ @Bool@
--
identityNotificationAttributes :: Text -- ^ 'inaBounceTopic'
                               -> Text -- ^ 'inaComplaintTopic'
                               -> Text -- ^ 'inaDeliveryTopic'
                               -> Bool -- ^ 'inaForwardingEnabled'
                               -> IdentityNotificationAttributes
identityNotificationAttributes p1 p2 p3 p4 = IdentityNotificationAttributes
    { _inaBounceTopic = p1
    , _inaComplaintTopic = p2
    , _inaDeliveryTopic = p3
    , _inaForwardingEnabled = p4
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish bounce notifications.
inaBounceTopic :: Lens' IdentityNotificationAttributes Text
inaBounceTopic = lens _inaBounceTopic (\s a -> s { _inaBounceTopic = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish complaint notifications.
inaComplaintTopic :: Lens' IdentityNotificationAttributes Text
inaComplaintTopic =
    lens _inaComplaintTopic (\s a -> s { _inaComplaintTopic = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish delivery notifications.
inaDeliveryTopic :: Lens' IdentityNotificationAttributes Text
inaDeliveryTopic =
    lens _inaDeliveryTopic (\s a -> s { _inaDeliveryTopic = a })

-- | Describes whether Amazon SES will forward bounce and complaint
-- notifications as email. true indicates that Amazon SES will forward bounce
-- and complaint notifications as email, while false indicates that bounce and
-- complaint notifications will be published only to the specified bounce and
-- complaint Amazon SNS topics.
inaForwardingEnabled :: Lens' IdentityNotificationAttributes Bool
inaForwardingEnabled =
    lens _inaForwardingEnabled (\s a -> s { _inaForwardingEnabled = a })

instance FromXML IdentityNotificationAttributes where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IdentityNotificationAttributes"

-- | Represents the verification attributes of a single identity.
data IdentityVerificationAttributes = IdentityVerificationAttributes
    { _ivaVerificationStatus :: VerificationStatus
    , _ivaVerificationToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IdentityVerificationAttributes' data type.
--
-- 'IdentityVerificationAttributes' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VerificationStatus ::@ @VerificationStatus@
--
-- * @VerificationToken ::@ @Maybe Text@
--
identityVerificationAttributes :: VerificationStatus -- ^ 'ivaVerificationStatus'
                               -> IdentityVerificationAttributes
identityVerificationAttributes p1 = IdentityVerificationAttributes
    { _ivaVerificationStatus = p1
    , _ivaVerificationToken = Nothing
    }

-- | The verification status of the identity: "Pending", "Success", "Failed", or
-- "TemporaryFailure".
ivaVerificationStatus :: Lens' IdentityVerificationAttributes VerificationStatus
ivaVerificationStatus =
    lens _ivaVerificationStatus (\s a -> s { _ivaVerificationStatus = a })

-- | The verification token for a domain identity. Null for email address
-- identities.
ivaVerificationToken :: Lens' IdentityVerificationAttributes (Maybe Text)
ivaVerificationToken =
    lens _ivaVerificationToken (\s a -> s { _ivaVerificationToken = a })

instance FromXML IdentityVerificationAttributes where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IdentityVerificationAttributes"

-- | The message to be sent.
data Message = Message
    { _mSubject :: Content
    , _mBody :: Body
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Message' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Subject ::@ @Content@
--
-- * @Body ::@ @Body@
--
message :: Content -- ^ 'mSubject'
        -> Body -- ^ 'mBody'
        -> Message
message p1 p2 = Message
    { _mSubject = p1
    , _mBody = p2
    }

-- | The subject of the message: A short summary of the content, which will
-- appear in the recipient's inbox.
mSubject :: Lens' Message Content
mSubject = lens _mSubject (\s a -> s { _mSubject = a })

-- | The message body.
mBody :: Lens' Message Body
mBody = lens _mBody (\s a -> s { _mBody = a })

instance ToQuery Message where
    toQuery = genericQuery def

-- | Represents sending statistics data. Each SendDataPoint contains statistics
-- for a 15-minute period of sending activity.
data SendDataPoint = SendDataPoint
    { _sdpTimestamp :: Maybe ISO8601
    , _sdpDeliveryAttempts :: Maybe Integer
    , _sdpBounces :: Maybe Integer
    , _sdpComplaints :: Maybe Integer
    , _sdpRejects :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SendDataPoint' data type.
--
-- 'SendDataPoint' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Timestamp ::@ @Maybe ISO8601@
--
-- * @DeliveryAttempts ::@ @Maybe Integer@
--
-- * @Bounces ::@ @Maybe Integer@
--
-- * @Complaints ::@ @Maybe Integer@
--
-- * @Rejects ::@ @Maybe Integer@
--
sendDataPoint :: SendDataPoint
sendDataPoint = SendDataPoint
    { _sdpTimestamp = Nothing
    , _sdpDeliveryAttempts = Nothing
    , _sdpBounces = Nothing
    , _sdpComplaints = Nothing
    , _sdpRejects = Nothing
    }

-- | Time of the data point.
sdpTimestamp :: Lens' SendDataPoint (Maybe ISO8601)
sdpTimestamp = lens _sdpTimestamp (\s a -> s { _sdpTimestamp = a })

-- | Number of emails that have been enqueued for sending.
sdpDeliveryAttempts :: Lens' SendDataPoint (Maybe Integer)
sdpDeliveryAttempts =
    lens _sdpDeliveryAttempts (\s a -> s { _sdpDeliveryAttempts = a })

-- | Number of emails that have bounced.
sdpBounces :: Lens' SendDataPoint (Maybe Integer)
sdpBounces = lens _sdpBounces (\s a -> s { _sdpBounces = a })

-- | Number of unwanted emails that were rejected by recipients.
sdpComplaints :: Lens' SendDataPoint (Maybe Integer)
sdpComplaints = lens _sdpComplaints (\s a -> s { _sdpComplaints = a })

-- | Number of emails rejected by Amazon SES.
sdpRejects :: Lens' SendDataPoint (Maybe Integer)
sdpRejects = lens _sdpRejects (\s a -> s { _sdpRejects = a })

instance FromXML SendDataPoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SendDataPoint"
