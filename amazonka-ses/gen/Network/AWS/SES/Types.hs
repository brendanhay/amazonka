{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.SES.Types
    (
    -- * Service
      SES
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * Destination
    , Destination
    , destination
    , dBccAddresses
    , dCcAddresses
    , dToAddresses

    -- * IdentityDkimAttributes
    , IdentityDkimAttributes
    , identityDkimAttributes
    , idaDkimEnabled
    , idaDkimTokens
    , idaDkimVerificationStatus

    -- * Body
    , Body
    , body
    , bHtml
    , bText

    -- * IdentityVerificationAttributes
    , IdentityVerificationAttributes
    , identityVerificationAttributes
    , ivaVerificationStatus
    , ivaVerificationToken

    -- * SendDataPoint
    , SendDataPoint
    , sendDataPoint
    , sdpBounces
    , sdpComplaints
    , sdpDeliveryAttempts
    , sdpRejects
    , sdpTimestamp

    -- * IdentityType
    , IdentityType (..)

    -- * Content
    , Content
    , content
    , cCharset
    , cData

    -- * IdentityNotificationAttributes
    , IdentityNotificationAttributes
    , identityNotificationAttributes
    , inaBounceTopic
    , inaComplaintTopic
    , inaDeliveryTopic
    , inaForwardingEnabled

    -- * RawMessage
    , RawMessage
    , rawMessage
    , rmData

    -- * NotificationType
    , NotificationType (..)

    -- * VerificationStatus
    , VerificationStatus (..)

    -- * Message
    , Message
    , message
    , mBody
    , mSubject
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2010-12-01@ of the Amazon Simple Email Service service.
data SES

instance AWSService SES where
    type Sg SES = V4
    type Er SES = RESTError

    service = service'
      where
        service' :: Service SES
        service' = Service
              { _svcAbbrev       = "SES"
              , _svcPrefix       = "email"
              , _svcVersion      = "2010-12-01"
              , _svcTargetPrefix = Nothing
              , _svcJSONVersion  = Nothing
              , _svcHandle       = handle
              , _svcRetry        = retry
              }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry RESTError
        retry = Retry
            { _rPolicy   = exponentialBackon 0.05 2
            , _rAttempts = 5
            , _rCheck    = check
            }

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 400 && "Throttling" == e = True -- Throttling
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://ses.amazonaws.com/doc/2010-12-01/"
{-# INLINE ns #-}

data Destination = Destination
    { _dBccAddresses :: List "member" Text
    , _dCcAddresses  :: List "member" Text
    , _dToAddresses  :: List "member" Text
    } deriving (Eq, Ord, Show)

-- | 'Destination' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dBccAddresses' @::@ ['Text']
--
-- * 'dCcAddresses' @::@ ['Text']
--
-- * 'dToAddresses' @::@ ['Text']
--
destination :: Destination
destination = Destination
    { _dToAddresses  = mempty
    , _dCcAddresses  = mempty
    , _dBccAddresses = mempty
    }

-- | The BCC: field(s) of the message.
dBccAddresses :: Lens' Destination [Text]
dBccAddresses = lens _dBccAddresses (\s a -> s { _dBccAddresses = a }) . _List

-- | The CC: field(s) of the message.
dCcAddresses :: Lens' Destination [Text]
dCcAddresses = lens _dCcAddresses (\s a -> s { _dCcAddresses = a }) . _List

-- | The To: field(s) of the message.
dToAddresses :: Lens' Destination [Text]
dToAddresses = lens _dToAddresses (\s a -> s { _dToAddresses = a }) . _List

instance FromXML Destination where
    parseXML x = Destination
        <$> x .@? "BccAddresses" .!@ mempty
        <*> x .@? "CcAddresses" .!@ mempty
        <*> x .@? "ToAddresses" .!@ mempty

instance ToQuery Destination where
    toQuery Destination{..} = mconcat
        [ "BccAddresses" =? _dBccAddresses
        , "CcAddresses"  =? _dCcAddresses
        , "ToAddresses"  =? _dToAddresses
        ]

data IdentityDkimAttributes = IdentityDkimAttributes
    { _idaDkimEnabled            :: Bool
    , _idaDkimTokens             :: List "member" Text
    , _idaDkimVerificationStatus :: VerificationStatus
    } deriving (Eq, Show)

-- | 'IdentityDkimAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idaDkimEnabled' @::@ 'Bool'
--
-- * 'idaDkimTokens' @::@ ['Text']
--
-- * 'idaDkimVerificationStatus' @::@ 'VerificationStatus'
--
identityDkimAttributes :: Bool -- ^ 'idaDkimEnabled'
                       -> VerificationStatus -- ^ 'idaDkimVerificationStatus'
                       -> IdentityDkimAttributes
identityDkimAttributes p1 p2 = IdentityDkimAttributes
    { _idaDkimEnabled            = p1
    , _idaDkimVerificationStatus = p2
    , _idaDkimTokens             = mempty
    }

-- | True if DKIM signing is enabled for email sent from the identity; false
-- otherwise.
idaDkimEnabled :: Lens' IdentityDkimAttributes Bool
idaDkimEnabled = lens _idaDkimEnabled (\s a -> s { _idaDkimEnabled = a })

-- | A set of character strings that represent the domain's identity. Using these
-- tokens, you will need to create DNS CNAME records that point to DKIM public
-- keys hosted by Amazon SES. Amazon Web Services will eventually detect that
-- you have updated your DNS records; this detection process may take up to 72
-- hours. Upon successful detection, Amazon SES will be able to DKIM-sign email
-- originating from that domain. (This only applies to domain identities, not
-- email address identities.)
--
-- For more information about creating DNS records using DKIM tokens, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide>.
idaDkimTokens :: Lens' IdentityDkimAttributes [Text]
idaDkimTokens = lens _idaDkimTokens (\s a -> s { _idaDkimTokens = a }) . _List

-- | Describes whether Amazon SES has successfully verified the DKIM DNS records
-- (tokens) published in the domain name's DNS. (This only applies to domain
-- identities, not email address identities.)
idaDkimVerificationStatus :: Lens' IdentityDkimAttributes VerificationStatus
idaDkimVerificationStatus =
    lens _idaDkimVerificationStatus
        (\s a -> s { _idaDkimVerificationStatus = a })

instance FromXML IdentityDkimAttributes where
    parseXML x = IdentityDkimAttributes
        <$> x .@  "DkimEnabled"
        <*> x .@? "DkimTokens" .!@ mempty
        <*> x .@  "DkimVerificationStatus"

instance ToQuery IdentityDkimAttributes where
    toQuery IdentityDkimAttributes{..} = mconcat
        [ "DkimEnabled"            =? _idaDkimEnabled
        , "DkimTokens"             =? _idaDkimTokens
        , "DkimVerificationStatus" =? _idaDkimVerificationStatus
        ]

data Body = Body
    { _bHtml :: Maybe Content
    , _bText :: Maybe Content
    } deriving (Eq, Show)

-- | 'Body' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bHtml' @::@ 'Maybe' 'Content'
--
-- * 'bText' @::@ 'Maybe' 'Content'
--
body :: Body
body = Body
    { _bText = Nothing
    , _bHtml = Nothing
    }

-- | The content of the message, in HTML format. Use this for email clients that
-- can process HTML. You can include clickable links, formatted text, and much
-- more in an HTML message.
bHtml :: Lens' Body (Maybe Content)
bHtml = lens _bHtml (\s a -> s { _bHtml = a })

-- | The content of the message, in text format. Use this for text-based email
-- clients, or clients on high-latency networks (such as mobile devices).
bText :: Lens' Body (Maybe Content)
bText = lens _bText (\s a -> s { _bText = a })

instance FromXML Body where
    parseXML x = Body
        <$> x .@? "Html"
        <*> x .@? "Text"

instance ToQuery Body where
    toQuery Body{..} = mconcat
        [ "Html" =? _bHtml
        , "Text" =? _bText
        ]

data IdentityVerificationAttributes = IdentityVerificationAttributes
    { _ivaVerificationStatus :: VerificationStatus
    , _ivaVerificationToken  :: Maybe Text
    } deriving (Eq, Show)

-- | 'IdentityVerificationAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivaVerificationStatus' @::@ 'VerificationStatus'
--
-- * 'ivaVerificationToken' @::@ 'Maybe' 'Text'
--
identityVerificationAttributes :: VerificationStatus -- ^ 'ivaVerificationStatus'
                               -> IdentityVerificationAttributes
identityVerificationAttributes p1 = IdentityVerificationAttributes
    { _ivaVerificationStatus = p1
    , _ivaVerificationToken  = Nothing
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
    parseXML x = IdentityVerificationAttributes
        <$> x .@  "VerificationStatus"
        <*> x .@? "VerificationToken"

instance ToQuery IdentityVerificationAttributes where
    toQuery IdentityVerificationAttributes{..} = mconcat
        [ "VerificationStatus" =? _ivaVerificationStatus
        , "VerificationToken"  =? _ivaVerificationToken
        ]

data SendDataPoint = SendDataPoint
    { _sdpBounces          :: Maybe Integer
    , _sdpComplaints       :: Maybe Integer
    , _sdpDeliveryAttempts :: Maybe Integer
    , _sdpRejects          :: Maybe Integer
    , _sdpTimestamp        :: Maybe ISO8601
    } deriving (Eq, Ord, Show)

-- | 'SendDataPoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdpBounces' @::@ 'Maybe' 'Integer'
--
-- * 'sdpComplaints' @::@ 'Maybe' 'Integer'
--
-- * 'sdpDeliveryAttempts' @::@ 'Maybe' 'Integer'
--
-- * 'sdpRejects' @::@ 'Maybe' 'Integer'
--
-- * 'sdpTimestamp' @::@ 'Maybe' 'UTCTime'
--
sendDataPoint :: SendDataPoint
sendDataPoint = SendDataPoint
    { _sdpTimestamp        = Nothing
    , _sdpDeliveryAttempts = Nothing
    , _sdpBounces          = Nothing
    , _sdpComplaints       = Nothing
    , _sdpRejects          = Nothing
    }

-- | Number of emails that have bounced.
sdpBounces :: Lens' SendDataPoint (Maybe Integer)
sdpBounces = lens _sdpBounces (\s a -> s { _sdpBounces = a })

-- | Number of unwanted emails that were rejected by recipients.
sdpComplaints :: Lens' SendDataPoint (Maybe Integer)
sdpComplaints = lens _sdpComplaints (\s a -> s { _sdpComplaints = a })

-- | Number of emails that have been enqueued for sending.
sdpDeliveryAttempts :: Lens' SendDataPoint (Maybe Integer)
sdpDeliveryAttempts =
    lens _sdpDeliveryAttempts (\s a -> s { _sdpDeliveryAttempts = a })

-- | Number of emails rejected by Amazon SES.
sdpRejects :: Lens' SendDataPoint (Maybe Integer)
sdpRejects = lens _sdpRejects (\s a -> s { _sdpRejects = a })

-- | Time of the data point.
sdpTimestamp :: Lens' SendDataPoint (Maybe UTCTime)
sdpTimestamp = lens _sdpTimestamp (\s a -> s { _sdpTimestamp = a }) . mapping _Time

instance FromXML SendDataPoint where
    parseXML x = SendDataPoint
        <$> x .@? "Bounces"
        <*> x .@? "Complaints"
        <*> x .@? "DeliveryAttempts"
        <*> x .@? "Rejects"
        <*> x .@? "Timestamp"

instance ToQuery SendDataPoint where
    toQuery SendDataPoint{..} = mconcat
        [ "Bounces"          =? _sdpBounces
        , "Complaints"       =? _sdpComplaints
        , "DeliveryAttempts" =? _sdpDeliveryAttempts
        , "Rejects"          =? _sdpRejects
        , "Timestamp"        =? _sdpTimestamp
        ]

data IdentityType
    = ITDomain       -- ^ Domain
    | ITEmailAddress -- ^ EmailAddress
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable IdentityType

instance FromText IdentityType where
    parser = takeText >>= \case
        "Domain"       -> pure ITDomain
        "EmailAddress" -> pure ITEmailAddress
        e              -> fail $
            "Failure parsing IdentityType from " ++ show e

instance ToText IdentityType where
    toText = \case
        ITDomain       -> "Domain"
        ITEmailAddress -> "EmailAddress"

instance ToByteString IdentityType
instance ToHeader     IdentityType
instance ToQuery      IdentityType

instance FromXML IdentityType where
    parseXML = parseXMLText "IdentityType"

data Content = Content
    { _cCharset :: Maybe Text
    , _cData    :: Text
    } deriving (Eq, Ord, Show)

-- | 'Content' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cCharset' @::@ 'Maybe' 'Text'
--
-- * 'cData' @::@ 'Text'
--
content :: Text -- ^ 'cData'
        -> Content
content p1 = Content
    { _cData    = p1
    , _cCharset = Nothing
    }

-- | The character set of the content.
cCharset :: Lens' Content (Maybe Text)
cCharset = lens _cCharset (\s a -> s { _cCharset = a })

-- | The textual data of the content.
cData :: Lens' Content Text
cData = lens _cData (\s a -> s { _cData = a })

instance FromXML Content where
    parseXML x = Content
        <$> x .@? "Charset"
        <*> x .@  "Data"

instance ToQuery Content where
    toQuery Content{..} = mconcat
        [ "Charset" =? _cCharset
        , "Data"    =? _cData
        ]

data IdentityNotificationAttributes = IdentityNotificationAttributes
    { _inaBounceTopic       :: Text
    , _inaComplaintTopic    :: Text
    , _inaDeliveryTopic     :: Text
    , _inaForwardingEnabled :: Bool
    } deriving (Eq, Ord, Show)

-- | 'IdentityNotificationAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'inaBounceTopic' @::@ 'Text'
--
-- * 'inaComplaintTopic' @::@ 'Text'
--
-- * 'inaDeliveryTopic' @::@ 'Text'
--
-- * 'inaForwardingEnabled' @::@ 'Bool'
--
identityNotificationAttributes :: Text -- ^ 'inaBounceTopic'
                               -> Text -- ^ 'inaComplaintTopic'
                               -> Text -- ^ 'inaDeliveryTopic'
                               -> Bool -- ^ 'inaForwardingEnabled'
                               -> IdentityNotificationAttributes
identityNotificationAttributes p1 p2 p3 p4 = IdentityNotificationAttributes
    { _inaBounceTopic       = p1
    , _inaComplaintTopic    = p2
    , _inaDeliveryTopic     = p3
    , _inaForwardingEnabled = p4
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will
-- publish bounce notifications.
inaBounceTopic :: Lens' IdentityNotificationAttributes Text
inaBounceTopic = lens _inaBounceTopic (\s a -> s { _inaBounceTopic = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will
-- publish complaint notifications.
inaComplaintTopic :: Lens' IdentityNotificationAttributes Text
inaComplaintTopic =
    lens _inaComplaintTopic (\s a -> s { _inaComplaintTopic = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will
-- publish delivery notifications.
inaDeliveryTopic :: Lens' IdentityNotificationAttributes Text
inaDeliveryTopic = lens _inaDeliveryTopic (\s a -> s { _inaDeliveryTopic = a })

-- | Describes whether Amazon SES will forward bounce and complaint notifications
-- as email. 'true' indicates that Amazon SES will forward bounce and complaint
-- notifications as email, while 'false' indicates that bounce and complaint
-- notifications will be published only to the specified bounce and complaint
-- Amazon SNS topics.
inaForwardingEnabled :: Lens' IdentityNotificationAttributes Bool
inaForwardingEnabled =
    lens _inaForwardingEnabled (\s a -> s { _inaForwardingEnabled = a })

instance FromXML IdentityNotificationAttributes where
    parseXML x = IdentityNotificationAttributes
        <$> x .@  "BounceTopic"
        <*> x .@  "ComplaintTopic"
        <*> x .@  "DeliveryTopic"
        <*> x .@  "ForwardingEnabled"

instance ToQuery IdentityNotificationAttributes where
    toQuery IdentityNotificationAttributes{..} = mconcat
        [ "BounceTopic"       =? _inaBounceTopic
        , "ComplaintTopic"    =? _inaComplaintTopic
        , "DeliveryTopic"     =? _inaDeliveryTopic
        , "ForwardingEnabled" =? _inaForwardingEnabled
        ]

newtype RawMessage = RawMessage
    { _rmData :: Base64
    } deriving (Eq, Show)

-- | 'RawMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmData' @::@ 'Base64'
--
rawMessage :: Base64 -- ^ 'rmData'
           -> RawMessage
rawMessage p1 = RawMessage
    { _rmData = p1
    }

-- | The raw data of the message. The client must ensure that the message format
-- complies with Internet email standards regarding email header fields, MIME
-- types, MIME encoding, and base64 encoding (if necessary).
--
-- The To:, CC:, and BCC: headers in the raw message can contain a group list.
--
-- For more information, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide>.
rmData :: Lens' RawMessage Base64
rmData = lens _rmData (\s a -> s { _rmData = a })

instance FromXML RawMessage where
    parseXML x = RawMessage
        <$> x .@  "Data"

instance ToQuery RawMessage where
    toQuery RawMessage{..} = mconcat
        [ "Data" =? _rmData
        ]

data NotificationType
    = Bounce    -- ^ Bounce
    | Complaint -- ^ Complaint
    | Delivery  -- ^ Delivery
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable NotificationType

instance FromText NotificationType where
    parser = takeText >>= \case
        "Bounce"    -> pure Bounce
        "Complaint" -> pure Complaint
        "Delivery"  -> pure Delivery
        e           -> fail $
            "Failure parsing NotificationType from " ++ show e

instance ToText NotificationType where
    toText = \case
        Bounce    -> "Bounce"
        Complaint -> "Complaint"
        Delivery  -> "Delivery"

instance ToByteString NotificationType
instance ToHeader     NotificationType
instance ToQuery      NotificationType

instance FromXML NotificationType where
    parseXML = parseXMLText "NotificationType"

data VerificationStatus
    = Failed           -- ^ Failed
    | NotStarted       -- ^ NotStarted
    | Pending          -- ^ Pending
    | Success          -- ^ Success
    | TemporaryFailure -- ^ TemporaryFailure
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VerificationStatus

instance FromText VerificationStatus where
    parser = takeText >>= \case
        "Failed"           -> pure Failed
        "NotStarted"       -> pure NotStarted
        "Pending"          -> pure Pending
        "Success"          -> pure Success
        "TemporaryFailure" -> pure TemporaryFailure
        e                  -> fail $
            "Failure parsing VerificationStatus from " ++ show e

instance ToText VerificationStatus where
    toText = \case
        Failed           -> "Failed"
        NotStarted       -> "NotStarted"
        Pending          -> "Pending"
        Success          -> "Success"
        TemporaryFailure -> "TemporaryFailure"

instance ToByteString VerificationStatus
instance ToHeader     VerificationStatus
instance ToQuery      VerificationStatus

instance FromXML VerificationStatus where
    parseXML = parseXMLText "VerificationStatus"

data Message = Message
    { _mBody    :: Body
    , _mSubject :: Content
    } deriving (Eq, Show)

-- | 'Message' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mBody' @::@ 'Body'
--
-- * 'mSubject' @::@ 'Content'
--
message :: Content -- ^ 'mSubject'
        -> Body -- ^ 'mBody'
        -> Message
message p1 p2 = Message
    { _mSubject = p1
    , _mBody    = p2
    }

-- | The message body.
mBody :: Lens' Message Body
mBody = lens _mBody (\s a -> s { _mBody = a })

-- | The subject of the message: A short summary of the content, which will appear
-- in the recipient's inbox.
mSubject :: Lens' Message Content
mSubject = lens _mSubject (\s a -> s { _mSubject = a })

instance FromXML Message where
    parseXML x = Message
        <$> x .@  "Body"
        <*> x .@  "Subject"

instance ToQuery Message where
    toQuery Message{..} = mconcat
        [ "Body"    =? _mBody
        , "Subject" =? _mSubject
        ]
