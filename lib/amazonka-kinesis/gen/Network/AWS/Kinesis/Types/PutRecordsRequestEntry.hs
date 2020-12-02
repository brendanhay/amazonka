{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.PutRecordsRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.PutRecordsRequestEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output for @PutRecords@ .
--
--
--
-- /See:/ 'putRecordsRequestEntry' smart constructor.
data PutRecordsRequestEntry = PutRecordsRequestEntry'
  { _prreExplicitHashKey ::
      !(Maybe Text),
    _prreData :: !Base64,
    _prrePartitionKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRecordsRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prreExplicitHashKey' - The hash value used to determine explicitly the shard that the data record is assigned to by overriding the partition key hash.
--
-- * 'prreData' - The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'prrePartitionKey' - Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
putRecordsRequestEntry ::
  -- | 'prreData'
  ByteString ->
  -- | 'prrePartitionKey'
  Text ->
  PutRecordsRequestEntry
putRecordsRequestEntry pData_ pPartitionKey_ =
  PutRecordsRequestEntry'
    { _prreExplicitHashKey = Nothing,
      _prreData = _Base64 # pData_,
      _prrePartitionKey = pPartitionKey_
    }

-- | The hash value used to determine explicitly the shard that the data record is assigned to by overriding the partition key hash.
prreExplicitHashKey :: Lens' PutRecordsRequestEntry (Maybe Text)
prreExplicitHashKey = lens _prreExplicitHashKey (\s a -> s {_prreExplicitHashKey = a})

-- | The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
prreData :: Lens' PutRecordsRequestEntry ByteString
prreData = lens _prreData (\s a -> s {_prreData = a}) . _Base64

-- | Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
prrePartitionKey :: Lens' PutRecordsRequestEntry Text
prrePartitionKey = lens _prrePartitionKey (\s a -> s {_prrePartitionKey = a})

instance Hashable PutRecordsRequestEntry

instance NFData PutRecordsRequestEntry

instance ToJSON PutRecordsRequestEntry where
  toJSON PutRecordsRequestEntry' {..} =
    object
      ( catMaybes
          [ ("ExplicitHashKey" .=) <$> _prreExplicitHashKey,
            Just ("Data" .= _prreData),
            Just ("PartitionKey" .= _prrePartitionKey)
          ]
      )
