{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Record
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.Record where

import Network.AWS.Kinesis.Types.EncryptionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The unit of data of the Kinesis data stream, which is composed of a sequence number, a partition key, and a data blob.
--
--
--
-- /See:/ 'record' smart constructor.
data Record = Record'
  { _rEncryptionType :: !(Maybe EncryptionType),
    _rApproximateArrivalTimestamp :: !(Maybe POSIX),
    _rSequenceNumber :: !Text,
    _rData :: !Base64,
    _rPartitionKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rEncryptionType' - The encryption type used on the record. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
-- * 'rApproximateArrivalTimestamp' - The approximate time that the record was inserted into the stream.
--
-- * 'rSequenceNumber' - The unique identifier of the record within its shard.
--
-- * 'rData' - The data blob. The data in the blob is both opaque and immutable to Kinesis Data Streams, which does not inspect, interpret, or change the data in the blob in any way. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'rPartitionKey' - Identifies which shard in the stream the data record is assigned to.
record ::
  -- | 'rSequenceNumber'
  Text ->
  -- | 'rData'
  ByteString ->
  -- | 'rPartitionKey'
  Text ->
  Record
record pSequenceNumber_ pData_ pPartitionKey_ =
  Record'
    { _rEncryptionType = Nothing,
      _rApproximateArrivalTimestamp = Nothing,
      _rSequenceNumber = pSequenceNumber_,
      _rData = _Base64 # pData_,
      _rPartitionKey = pPartitionKey_
    }

-- | The encryption type used on the record. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
rEncryptionType :: Lens' Record (Maybe EncryptionType)
rEncryptionType = lens _rEncryptionType (\s a -> s {_rEncryptionType = a})

-- | The approximate time that the record was inserted into the stream.
rApproximateArrivalTimestamp :: Lens' Record (Maybe UTCTime)
rApproximateArrivalTimestamp = lens _rApproximateArrivalTimestamp (\s a -> s {_rApproximateArrivalTimestamp = a}) . mapping _Time

-- | The unique identifier of the record within its shard.
rSequenceNumber :: Lens' Record Text
rSequenceNumber = lens _rSequenceNumber (\s a -> s {_rSequenceNumber = a})

-- | The data blob. The data in the blob is both opaque and immutable to Kinesis Data Streams, which does not inspect, interpret, or change the data in the blob in any way. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rData :: Lens' Record ByteString
rData = lens _rData (\s a -> s {_rData = a}) . _Base64

-- | Identifies which shard in the stream the data record is assigned to.
rPartitionKey :: Lens' Record Text
rPartitionKey = lens _rPartitionKey (\s a -> s {_rPartitionKey = a})

instance FromJSON Record where
  parseJSON =
    withObject
      "Record"
      ( \x ->
          Record'
            <$> (x .:? "EncryptionType")
            <*> (x .:? "ApproximateArrivalTimestamp")
            <*> (x .: "SequenceNumber")
            <*> (x .: "Data")
            <*> (x .: "PartitionKey")
      )

instance Hashable Record

instance NFData Record
