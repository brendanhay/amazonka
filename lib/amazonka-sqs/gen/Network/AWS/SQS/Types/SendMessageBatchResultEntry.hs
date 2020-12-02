{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.SendMessageBatchResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.SendMessageBatchResultEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Encloses a @MessageId@ for a successfully-enqueued message in a @'SendMessageBatch' .@
--
--
--
-- /See:/ 'sendMessageBatchResultEntry' smart constructor.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry'
  { _smbreSequenceNumber ::
      !(Maybe Text),
    _smbreMD5OfMessageSystemAttributes ::
      !(Maybe Text),
    _smbreMD5OfMessageAttributes ::
      !(Maybe Text),
    _smbreId :: !Text,
    _smbreMessageId :: !Text,
    _smbreMD5OfMessageBody :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendMessageBatchResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbreSequenceNumber' - This parameter applies only to FIFO (first-in-first-out) queues. The large, non-consecutive number that Amazon SQS assigns to each message. The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
--
-- * 'smbreMD5OfMessageSystemAttributes' - An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- * 'smbreMD5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- * 'smbreId' - An identifier for the message in this batch.
--
-- * 'smbreMessageId' - An identifier for the message.
--
-- * 'smbreMD5OfMessageBody' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
sendMessageBatchResultEntry ::
  -- | 'smbreId'
  Text ->
  -- | 'smbreMessageId'
  Text ->
  -- | 'smbreMD5OfMessageBody'
  Text ->
  SendMessageBatchResultEntry
sendMessageBatchResultEntry pId_ pMessageId_ pMD5OfMessageBody_ =
  SendMessageBatchResultEntry'
    { _smbreSequenceNumber = Nothing,
      _smbreMD5OfMessageSystemAttributes = Nothing,
      _smbreMD5OfMessageAttributes = Nothing,
      _smbreId = pId_,
      _smbreMessageId = pMessageId_,
      _smbreMD5OfMessageBody = pMD5OfMessageBody_
    }

-- | This parameter applies only to FIFO (first-in-first-out) queues. The large, non-consecutive number that Amazon SQS assigns to each message. The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
smbreSequenceNumber :: Lens' SendMessageBatchResultEntry (Maybe Text)
smbreSequenceNumber = lens _smbreSequenceNumber (\s a -> s {_smbreSequenceNumber = a})

-- | An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
smbreMD5OfMessageSystemAttributes :: Lens' SendMessageBatchResultEntry (Maybe Text)
smbreMD5OfMessageSystemAttributes = lens _smbreMD5OfMessageSystemAttributes (\s a -> s {_smbreMD5OfMessageSystemAttributes = a})

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
smbreMD5OfMessageAttributes :: Lens' SendMessageBatchResultEntry (Maybe Text)
smbreMD5OfMessageAttributes = lens _smbreMD5OfMessageAttributes (\s a -> s {_smbreMD5OfMessageAttributes = a})

-- | An identifier for the message in this batch.
smbreId :: Lens' SendMessageBatchResultEntry Text
smbreId = lens _smbreId (\s a -> s {_smbreId = a})

-- | An identifier for the message.
smbreMessageId :: Lens' SendMessageBatchResultEntry Text
smbreMessageId = lens _smbreMessageId (\s a -> s {_smbreMessageId = a})

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
smbreMD5OfMessageBody :: Lens' SendMessageBatchResultEntry Text
smbreMD5OfMessageBody = lens _smbreMD5OfMessageBody (\s a -> s {_smbreMD5OfMessageBody = a})

instance FromXML SendMessageBatchResultEntry where
  parseXML x =
    SendMessageBatchResultEntry'
      <$> (x .@? "SequenceNumber")
      <*> (x .@? "MD5OfMessageSystemAttributes")
      <*> (x .@? "MD5OfMessageAttributes")
      <*> (x .@ "Id")
      <*> (x .@ "MessageId")
      <*> (x .@ "MD5OfMessageBody")

instance Hashable SendMessageBatchResultEntry

instance NFData SendMessageBatchResultEntry
