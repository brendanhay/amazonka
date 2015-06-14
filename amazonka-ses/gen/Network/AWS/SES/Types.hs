{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Errors
    , RESTError

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

    -- * IdentityType
    , IdentityType (..)

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

    -- * NotificationType
    , NotificationType (..)

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

    -- * VerificationStatus
    , VerificationStatus (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2010-12-01@ of the Amazon Simple Email Service SDK.
data SES

instance AWSService SES where
    type Sg SES = V4
    type Er SES = RESTError

    service = service'
      where
        service' :: Service SES
        service' = Service
            { _svcAbbrev  = "SES"
            , _svcPrefix  = "email"
            , _svcVersion = "2010-12-01"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry SES
        retry = undefined

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'body' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bodText'
--
-- * 'bodHTML'
data Body = Body'{_bodText :: Maybe Content, _bodHTML :: Maybe Content} deriving (Eq, Read, Show)

-- | 'Body' smart constructor.
body :: Body
body = Body'{_bodText = Nothing, _bodHTML = Nothing};

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

-- | /See:/ 'content' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'conCharset'
--
-- * 'conData'
data Content = Content'{_conCharset :: Maybe Text, _conData :: Text} deriving (Eq, Read, Show)

-- | 'Content' smart constructor.
content :: Text -> Content
content pData = Content'{_conCharset = Nothing, _conData = pData};

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

-- | /See:/ 'destination' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desBCCAddresses'
--
-- * 'desCCAddresses'
--
-- * 'desToAddresses'
data Destination = Destination'{_desBCCAddresses :: Maybe [Text], _desCCAddresses :: Maybe [Text], _desToAddresses :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'Destination' smart constructor.
destination :: Destination
destination = Destination'{_desBCCAddresses = Nothing, _desCCAddresses = Nothing, _desToAddresses = Nothing};

-- | The BCC: field(s) of the message.
desBCCAddresses :: Lens' Destination (Maybe [Text])
desBCCAddresses = lens _desBCCAddresses (\ s a -> s{_desBCCAddresses = a});

-- | The CC: field(s) of the message.
desCCAddresses :: Lens' Destination (Maybe [Text])
desCCAddresses = lens _desCCAddresses (\ s a -> s{_desCCAddresses = a});

-- | The To: field(s) of the message.
desToAddresses :: Lens' Destination (Maybe [Text])
desToAddresses = lens _desToAddresses (\ s a -> s{_desToAddresses = a});

instance ToQuery Destination where
        toQuery Destination'{..}
          = mconcat
              ["BccAddresses" =: "member" =: _desBCCAddresses,
               "CcAddresses" =: "member" =: _desCCAddresses,
               "ToAddresses" =: "member" =: _desToAddresses]

-- | /See:/ 'identityDkimAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idaDkimTokens'
--
-- * 'idaDkimEnabled'
--
-- * 'idaDkimVerificationStatus'
data IdentityDkimAttributes = IdentityDkimAttributes'{_idaDkimTokens :: Maybe [Text], _idaDkimEnabled :: Bool, _idaDkimVerificationStatus :: VerificationStatus} deriving (Eq, Read, Show)

-- | 'IdentityDkimAttributes' smart constructor.
identityDkimAttributes :: Bool -> VerificationStatus -> IdentityDkimAttributes
identityDkimAttributes pDkimEnabled pDkimVerificationStatus = IdentityDkimAttributes'{_idaDkimTokens = Nothing, _idaDkimEnabled = pDkimEnabled, _idaDkimVerificationStatus = pDkimVerificationStatus};

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
idaDkimTokens :: Lens' IdentityDkimAttributes (Maybe [Text])
idaDkimTokens = lens _idaDkimTokens (\ s a -> s{_idaDkimTokens = a});

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
                 parseXMLList "member")
                <*> x .@ "DkimEnabled"
                <*> x .@ "DkimVerificationStatus"

-- | /See:/ 'identityNotificationAttributes' smart constructor.
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
data IdentityNotificationAttributes = IdentityNotificationAttributes'{_inaBounceTopic :: Text, _inaComplaintTopic :: Text, _inaDeliveryTopic :: Text, _inaForwardingEnabled :: Bool} deriving (Eq, Read, Show)

-- | 'IdentityNotificationAttributes' smart constructor.
identityNotificationAttributes :: Text -> Text -> Text -> Bool -> IdentityNotificationAttributes
identityNotificationAttributes pBounceTopic pComplaintTopic pDeliveryTopic pForwardingEnabled = IdentityNotificationAttributes'{_inaBounceTopic = pBounceTopic, _inaComplaintTopic = pComplaintTopic, _inaDeliveryTopic = pDeliveryTopic, _inaForwardingEnabled = pForwardingEnabled};

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
              x .@ "BounceTopic" <*> x .@ "ComplaintTopic" <*>
                x .@ "DeliveryTopic"
                <*> x .@ "ForwardingEnabled"

data IdentityType = Domain | EmailAddress deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText IdentityType where
    parser = takeLowerText >>= \case
        "Domain" -> pure Domain
        "EmailAddress" -> pure EmailAddress
        e -> fail ("Failure parsing IdentityType from " ++ show e)

instance ToText IdentityType where
    toText = \case
        Domain -> "Domain"
        EmailAddress -> "EmailAddress"

instance Hashable IdentityType
instance ToQuery IdentityType
instance ToHeader IdentityType

-- | /See:/ 'identityVerificationAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivaVerificationToken'
--
-- * 'ivaVerificationStatus'
data IdentityVerificationAttributes = IdentityVerificationAttributes'{_ivaVerificationToken :: Maybe Text, _ivaVerificationStatus :: VerificationStatus} deriving (Eq, Read, Show)

-- | 'IdentityVerificationAttributes' smart constructor.
identityVerificationAttributes :: VerificationStatus -> IdentityVerificationAttributes
identityVerificationAttributes pVerificationStatus = IdentityVerificationAttributes'{_ivaVerificationToken = Nothing, _ivaVerificationStatus = pVerificationStatus};

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
              x .@? "VerificationToken" <*>
                x .@ "VerificationStatus"

-- | /See:/ 'message' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mesSubject'
--
-- * 'mesBody'
data Message = Message'{_mesSubject :: Content, _mesBody :: Body} deriving (Eq, Read, Show)

-- | 'Message' smart constructor.
message :: Content -> Body -> Message
message pSubject pBody = Message'{_mesSubject = pSubject, _mesBody = pBody};

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

data NotificationType = Delivery | Bounce | Complaint deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText NotificationType where
    parser = takeLowerText >>= \case
        "Bounce" -> pure Bounce
        "Complaint" -> pure Complaint
        "Delivery" -> pure Delivery
        e -> fail ("Failure parsing NotificationType from " ++ show e)

instance ToText NotificationType where
    toText = \case
        Bounce -> "Bounce"
        Complaint -> "Complaint"
        Delivery -> "Delivery"

instance Hashable NotificationType
instance ToQuery NotificationType
instance ToHeader NotificationType

-- | /See:/ 'rawMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmData'
newtype RawMessage = RawMessage'{_rmData :: Base64} deriving (Eq, Read, Show)

-- | 'RawMessage' smart constructor.
rawMessage :: Base64 -> RawMessage
rawMessage pData = RawMessage'{_rmData = pData};

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

-- | /See:/ 'sendDataPoint' smart constructor.
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
data SendDataPoint = SendDataPoint'{_sdpRejects :: Maybe Integer, _sdpComplaints :: Maybe Integer, _sdpDeliveryAttempts :: Maybe Integer, _sdpBounces :: Maybe Integer, _sdpTimestamp :: Maybe ISO8601} deriving (Eq, Read, Show)

-- | 'SendDataPoint' smart constructor.
sendDataPoint :: SendDataPoint
sendDataPoint = SendDataPoint'{_sdpRejects = Nothing, _sdpComplaints = Nothing, _sdpDeliveryAttempts = Nothing, _sdpBounces = Nothing, _sdpTimestamp = Nothing};

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
              x .@? "Rejects" <*> x .@? "Complaints" <*>
                x .@? "DeliveryAttempts"
                <*> x .@? "Bounces"
                <*> x .@? "Timestamp"

data VerificationStatus = NotStarted | Pending | Success | TemporaryFailure | Failed deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VerificationStatus where
    parser = takeLowerText >>= \case
        "Failed" -> pure Failed
        "NotStarted" -> pure NotStarted
        "Pending" -> pure Pending
        "Success" -> pure Success
        "TemporaryFailure" -> pure TemporaryFailure
        e -> fail ("Failure parsing VerificationStatus from " ++ show e)

instance ToText VerificationStatus where
    toText = \case
        Failed -> "Failed"
        NotStarted -> "NotStarted"
        Pending -> "Pending"
        Success -> "Success"
        TemporaryFailure -> "TemporaryFailure"

instance Hashable VerificationStatus
instance ToQuery VerificationStatus
instance ToHeader VerificationStatus

instance FromXML VerificationStatus where
    parseXML = parseXMLText "VerificationStatus"
