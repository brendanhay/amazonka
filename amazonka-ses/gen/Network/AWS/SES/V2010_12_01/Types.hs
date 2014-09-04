{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.Types
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
module Network.AWS.SES.V2010_12_01.Types
    (
    -- * Service
      SES
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * IdentityType
    , IdentityType (..)

    -- * NotificationType
    , NotificationType (..)

    -- * VerificationStatus
    , VerificationStatus (..)

    -- * RawMessage
    , RawMessage (..)
    , rmData

    -- * Body
    , Body (..)
    , byText
    , byHtml

    -- * Content
    , Content (..)
    , ctData
    , ctCharset

    -- * Destination
    , Destination (..)
    , vToAddresses
    , vCcAddresses
    , vBccAddresses

    -- * IdentityDkimAttributes
    , IdentityDkimAttributes (..)
    , idaDkimEnabled
    , idaDkimVerificationStatus
    , idaDkimTokens

    -- * IdentityNotificationAttributes
    , IdentityNotificationAttributes (..)
    , inaBounceTopic
    , inaComplaintTopic
    , inaDeliveryTopic
    , inaForwardingEnabled

    -- * IdentityVerificationAttributes
    , IdentityVerificationAttributes (..)
    , ivaVerificationStatus
    , ivaVerificationToken

    -- * Message
    , Message (..)
    , zSubject
    , zBody

    -- * SendDataPoint
    , SendDataPoint (..)
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
    data Er SES
        = MessageRejected
        | SESClient HttpException
        | SESSerializer String
        | SESService String

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "email"
        , _svcVersion  = "2010-12-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er SES)
deriving instance Generic (Er SES)

instance AWSError (Er SES) where
    awsError = const "SESError"

instance AWSServiceError (Er SES) where
    serviceError    = SESService
    clientError     = SESClient
    serializerError = SESSerializer

instance Exception (Er SES)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://ses.amazonaws.com/doc/2010-12-01/"
    }

-- | The type of the identities to list. Possible values are "EmailAddress" and
-- "Domain". If this parameter is omitted, then all identities will be listed.
data IdentityType
    = IdentityTypeDomain -- ^ Domain
    | IdentityTypeEmailAddress -- ^ EmailAddress
      deriving (Eq, Show, Generic)

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

-- | The type of notifications that will be published to the specified Amazon
-- SNS topic.
data NotificationType
    = NotificationTypeBounce -- ^ Bounce
    | NotificationTypeComplaint -- ^ Complaint
    | NotificationTypeDelivery -- ^ Delivery
      deriving (Eq, Show, Generic)

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

-- | Describes whether Amazon SES has successfully verified the DKIM DNS records
-- (tokens) published in the domain name's DNS. (This only applies to domain
-- identities, not email address identities.).
data VerificationStatus
    = VerificationStatusFailed -- ^ Failed
    | VerificationStatusNotStarted -- ^ NotStarted
    | VerificationStatusPending -- ^ Pending
    | VerificationStatusSuccess -- ^ Success
    | VerificationStatusTemporaryFailure -- ^ TemporaryFailure
      deriving (Eq, Show, Generic)

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
      -- ^ The raw data of the message. The client must ensure that the
      -- message format complies with Internet email standards regarding
      -- email header fields, MIME types, MIME encoding, and base64
      -- encoding (if necessary). The To:, CC:, and BCC: headers in the
      -- raw message can contain a group list. For more information, go to
      -- the Amazon SES Developer Guide.
    } deriving (Show, Generic)

-- | The raw data of the message. The client must ensure that the message format
-- complies with Internet email standards regarding email header fields, MIME
-- types, MIME encoding, and base64 encoding (if necessary). The To:, CC:, and
-- BCC: headers in the raw message can contain a group list. For more
-- information, go to the Amazon SES Developer Guide.
rmData :: Lens' RawMessage (ByteString)
rmData f x =
    f (_rmData x)
        <&> \y -> x { _rmData = y }
{-# INLINE rmData #-}

instance ToQuery RawMessage where
    toQuery = genericQuery def

-- | The message body.
data Body = Body
    { _byText :: Maybe Content
      -- ^ The content of the message, in text format. Use this for
      -- text-based email clients, or clients on high-latency networks
      -- (such as mobile devices).
    , _byHtml :: Maybe Content
      -- ^ The content of the message, in HTML format. Use this for email
      -- clients that can process HTML. You can include clickable links,
      -- formatted text, and much more in an HTML message.
    } deriving (Show, Generic)

-- | The content of the message, in text format. Use this for text-based email
-- clients, or clients on high-latency networks (such as mobile devices).
byText :: Lens' Body (Maybe Content)
byText f x =
    f (_byText x)
        <&> \y -> x { _byText = y }
{-# INLINE byText #-}

-- | The content of the message, in HTML format. Use this for email clients that
-- can process HTML. You can include clickable links, formatted text, and much
-- more in an HTML message.
byHtml :: Lens' Body (Maybe Content)
byHtml f x =
    f (_byHtml x)
        <&> \y -> x { _byHtml = y }
{-# INLINE byHtml #-}

instance ToQuery Body where
    toQuery = genericQuery def

-- | The subject of the message: A short summary of the content, which will
-- appear in the recipient's inbox.
data Content = Content
    { _ctData :: Text
      -- ^ The textual data of the content.
    , _ctCharset :: Maybe Text
      -- ^ The character set of the content.
    } deriving (Show, Generic)

-- | The textual data of the content.
ctData :: Lens' Content (Text)
ctData f x =
    f (_ctData x)
        <&> \y -> x { _ctData = y }
{-# INLINE ctData #-}

-- | The character set of the content.
ctCharset :: Lens' Content (Maybe Text)
ctCharset f x =
    f (_ctCharset x)
        <&> \y -> x { _ctCharset = y }
{-# INLINE ctCharset #-}

instance ToQuery Content where
    toQuery = genericQuery def

-- | The destination for this email, composed of To:, CC:, and BCC: fields.
data Destination = Destination
    { _vToAddresses :: [Text]
      -- ^ The To: field(s) of the message.
    , _vCcAddresses :: [Text]
      -- ^ The CC: field(s) of the message.
    , _vBccAddresses :: [Text]
      -- ^ The BCC: field(s) of the message.
    } deriving (Show, Generic)

-- | The To: field(s) of the message.
vToAddresses :: Lens' Destination ([Text])
vToAddresses f x =
    f (_vToAddresses x)
        <&> \y -> x { _vToAddresses = y }
{-# INLINE vToAddresses #-}

-- | The CC: field(s) of the message.
vCcAddresses :: Lens' Destination ([Text])
vCcAddresses f x =
    f (_vCcAddresses x)
        <&> \y -> x { _vCcAddresses = y }
{-# INLINE vCcAddresses #-}

-- | The BCC: field(s) of the message.
vBccAddresses :: Lens' Destination ([Text])
vBccAddresses f x =
    f (_vBccAddresses x)
        <&> \y -> x { _vBccAddresses = y }
{-# INLINE vBccAddresses #-}

instance ToQuery Destination where
    toQuery = genericQuery def

-- | Represents the DKIM attributes of a verified email address or a domain.
data IdentityDkimAttributes = IdentityDkimAttributes
    { _idaDkimEnabled :: Bool
      -- ^ True if DKIM signing is enabled for email sent from the identity;
      -- false otherwise.
    , _idaDkimVerificationStatus :: VerificationStatus
      -- ^ Describes whether Amazon SES has successfully verified the DKIM
      -- DNS records (tokens) published in the domain name's DNS. (This
      -- only applies to domain identities, not email address
      -- identities.).
    , _idaDkimTokens :: [Text]
      -- ^ A set of character strings that represent the domain's identity.
      -- Using these tokens, you will need to create DNS CNAME records
      -- that point to DKIM public keys hosted by Amazon SES. Amazon Web
      -- Services will eventually detect that you have updated your DNS
      -- records; this detection process may take up to 72 hours. Upon
      -- successful detection, Amazon SES will be able to DKIM-sign email
      -- originating from that domain. (This only applies to domain
      -- identities, not email address identities.) For more information
      -- about creating DNS records using DKIM tokens, go to the Amazon
      -- SES Developer Guide.
    } deriving (Show, Generic)

-- | True if DKIM signing is enabled for email sent from the identity; false
-- otherwise.
idaDkimEnabled :: Lens' IdentityDkimAttributes (Bool)
idaDkimEnabled f x =
    f (_idaDkimEnabled x)
        <&> \y -> x { _idaDkimEnabled = y }
{-# INLINE idaDkimEnabled #-}

-- | Describes whether Amazon SES has successfully verified the DKIM DNS records
-- (tokens) published in the domain name's DNS. (This only applies to domain
-- identities, not email address identities.).
idaDkimVerificationStatus :: Lens' IdentityDkimAttributes (VerificationStatus)
idaDkimVerificationStatus f x =
    f (_idaDkimVerificationStatus x)
        <&> \y -> x { _idaDkimVerificationStatus = y }
{-# INLINE idaDkimVerificationStatus #-}

-- | A set of character strings that represent the domain's identity. Using
-- these tokens, you will need to create DNS CNAME records that point to DKIM
-- public keys hosted by Amazon SES. Amazon Web Services will eventually
-- detect that you have updated your DNS records; this detection process may
-- take up to 72 hours. Upon successful detection, Amazon SES will be able to
-- DKIM-sign email originating from that domain. (This only applies to domain
-- identities, not email address identities.) For more information about
-- creating DNS records using DKIM tokens, go to the Amazon SES Developer
-- Guide.
idaDkimTokens :: Lens' IdentityDkimAttributes ([Text])
idaDkimTokens f x =
    f (_idaDkimTokens x)
        <&> \y -> x { _idaDkimTokens = y }
{-# INLINE idaDkimTokens #-}

instance FromXML IdentityDkimAttributes where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IdentityDkimAttributes"

-- | Represents the notification attributes of an identity, including whether an
-- identity has Amazon Simple Notification Service (Amazon SNS) topics set for
-- bounce, complaint, and/or delivery notifications, and whether feedback
-- forwarding is enabled for bounce and complaint notifications.
data IdentityNotificationAttributes = IdentityNotificationAttributes
    { _inaBounceTopic :: Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic where
      -- Amazon SES will publish bounce notifications.
    , _inaComplaintTopic :: Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic where
      -- Amazon SES will publish complaint notifications.
    , _inaDeliveryTopic :: Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic where
      -- Amazon SES will publish delivery notifications.
    , _inaForwardingEnabled :: Bool
      -- ^ Describes whether Amazon SES will forward bounce and complaint
      -- notifications as email. true indicates that Amazon SES will
      -- forward bounce and complaint notifications as email, while false
      -- indicates that bounce and complaint notifications will be
      -- published only to the specified bounce and complaint Amazon SNS
      -- topics.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish bounce notifications.
inaBounceTopic :: Lens' IdentityNotificationAttributes (Text)
inaBounceTopic f x =
    f (_inaBounceTopic x)
        <&> \y -> x { _inaBounceTopic = y }
{-# INLINE inaBounceTopic #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish complaint notifications.
inaComplaintTopic :: Lens' IdentityNotificationAttributes (Text)
inaComplaintTopic f x =
    f (_inaComplaintTopic x)
        <&> \y -> x { _inaComplaintTopic = y }
{-# INLINE inaComplaintTopic #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish delivery notifications.
inaDeliveryTopic :: Lens' IdentityNotificationAttributes (Text)
inaDeliveryTopic f x =
    f (_inaDeliveryTopic x)
        <&> \y -> x { _inaDeliveryTopic = y }
{-# INLINE inaDeliveryTopic #-}

-- | Describes whether Amazon SES will forward bounce and complaint
-- notifications as email. true indicates that Amazon SES will forward bounce
-- and complaint notifications as email, while false indicates that bounce and
-- complaint notifications will be published only to the specified bounce and
-- complaint Amazon SNS topics.
inaForwardingEnabled :: Lens' IdentityNotificationAttributes (Bool)
inaForwardingEnabled f x =
    f (_inaForwardingEnabled x)
        <&> \y -> x { _inaForwardingEnabled = y }
{-# INLINE inaForwardingEnabled #-}

instance FromXML IdentityNotificationAttributes where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IdentityNotificationAttributes"

-- | Represents the verification attributes of a single identity.
data IdentityVerificationAttributes = IdentityVerificationAttributes
    { _ivaVerificationStatus :: VerificationStatus
      -- ^ The verification status of the identity: "Pending", "Success",
      -- "Failed", or "TemporaryFailure".
    , _ivaVerificationToken :: Maybe Text
      -- ^ The verification token for a domain identity. Null for email
      -- address identities.
    } deriving (Show, Generic)

-- | The verification status of the identity: "Pending", "Success", "Failed", or
-- "TemporaryFailure".
ivaVerificationStatus :: Lens' IdentityVerificationAttributes (VerificationStatus)
ivaVerificationStatus f x =
    f (_ivaVerificationStatus x)
        <&> \y -> x { _ivaVerificationStatus = y }
{-# INLINE ivaVerificationStatus #-}

-- | The verification token for a domain identity. Null for email address
-- identities.
ivaVerificationToken :: Lens' IdentityVerificationAttributes (Maybe Text)
ivaVerificationToken f x =
    f (_ivaVerificationToken x)
        <&> \y -> x { _ivaVerificationToken = y }
{-# INLINE ivaVerificationToken #-}

instance FromXML IdentityVerificationAttributes where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IdentityVerificationAttributes"

-- | The message to be sent.
data Message = Message
    { _zSubject :: Content
      -- ^ The subject of the message: A short summary of the content, which
      -- will appear in the recipient's inbox.
    , _zBody :: Body
      -- ^ The message body.
    } deriving (Show, Generic)

-- | The subject of the message: A short summary of the content, which will
-- appear in the recipient's inbox.
zSubject :: Lens' Message (Content)
zSubject f x =
    f (_zSubject x)
        <&> \y -> x { _zSubject = y }
{-# INLINE zSubject #-}

-- | The message body.
zBody :: Lens' Message (Body)
zBody f x =
    f (_zBody x)
        <&> \y -> x { _zBody = y }
{-# INLINE zBody #-}

instance ToQuery Message where
    toQuery = genericQuery def

-- | Represents sending statistics data. Each SendDataPoint contains statistics
-- for a 15-minute period of sending activity.
data SendDataPoint = SendDataPoint
    { _sdpTimestamp :: Maybe ISO8601
      -- ^ Time of the data point.
    , _sdpDeliveryAttempts :: Maybe Integer
      -- ^ Number of emails that have been enqueued for sending.
    , _sdpBounces :: Maybe Integer
      -- ^ Number of emails that have bounced.
    , _sdpComplaints :: Maybe Integer
      -- ^ Number of unwanted emails that were rejected by recipients.
    , _sdpRejects :: Maybe Integer
      -- ^ Number of emails rejected by Amazon SES.
    } deriving (Show, Generic)

-- | Time of the data point.
sdpTimestamp :: Lens' SendDataPoint (Maybe ISO8601)
sdpTimestamp f x =
    f (_sdpTimestamp x)
        <&> \y -> x { _sdpTimestamp = y }
{-# INLINE sdpTimestamp #-}

-- | Number of emails that have been enqueued for sending.
sdpDeliveryAttempts :: Lens' SendDataPoint (Maybe Integer)
sdpDeliveryAttempts f x =
    f (_sdpDeliveryAttempts x)
        <&> \y -> x { _sdpDeliveryAttempts = y }
{-# INLINE sdpDeliveryAttempts #-}

-- | Number of emails that have bounced.
sdpBounces :: Lens' SendDataPoint (Maybe Integer)
sdpBounces f x =
    f (_sdpBounces x)
        <&> \y -> x { _sdpBounces = y }
{-# INLINE sdpBounces #-}

-- | Number of unwanted emails that were rejected by recipients.
sdpComplaints :: Lens' SendDataPoint (Maybe Integer)
sdpComplaints f x =
    f (_sdpComplaints x)
        <&> \y -> x { _sdpComplaints = y }
{-# INLINE sdpComplaints #-}

-- | Number of emails rejected by Amazon SES.
sdpRejects :: Lens' SendDataPoint (Maybe Integer)
sdpRejects f x =
    f (_sdpRejects x)
        <&> \y -> x { _sdpRejects = y }
{-# INLINE sdpRejects #-}

instance FromXML SendDataPoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SendDataPoint"
