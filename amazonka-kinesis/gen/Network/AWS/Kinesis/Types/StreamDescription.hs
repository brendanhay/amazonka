{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StreamDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.StreamDescription where

import Network.AWS.Kinesis.Types.EncryptionType
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.Shard
import Network.AWS.Kinesis.Types.StreamStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output for DescribeStream.
--
-- /See:/ 'newStreamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { -- | The server-side encryption type used on the stream. This parameter can
    -- be one of the following values:
    --
    -- -   @NONE@: Do not encrypt the records in the stream.
    --
    -- -   @KMS@: Use server-side encryption on the records in the stream using
    --     a customer-managed AWS KMS key.
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | The GUID for the customer-managed AWS KMS key to use for encryption.
    -- This value can be a globally unique identifier, a fully specified ARN to
    -- either an alias or a key, or an alias name prefixed by \"alias\/\".You
    -- can also use a master key owned by Kinesis Data Streams by specifying
    -- the alias @aws\/kinesis@.
    --
    -- -   Key ARN example:
    --     @arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012@
    --
    -- -   Alias ARN example:
    --     @arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName@
    --
    -- -   Globally unique key ID example:
    --     @12345678-1234-1234-1234-123456789012@
    --
    -- -   Alias name example: @alias\/MyAliasName@
    --
    -- -   Master key owned by Kinesis Data Streams: @alias\/aws\/kinesis@
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream being described.
    streamName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the stream being described.
    streamARN :: Prelude.Text,
    -- | The current status of the stream being described. The stream status is
    -- one of the following states:
    --
    -- -   @CREATING@ - The stream is being created. Kinesis Data Streams
    --     immediately returns and sets @StreamStatus@ to @CREATING@.
    --
    -- -   @DELETING@ - The stream is being deleted. The specified stream is in
    --     the @DELETING@ state until Kinesis Data Streams completes the
    --     deletion.
    --
    -- -   @ACTIVE@ - The stream exists and is ready for read and write
    --     operations or deletion. You should perform read and write operations
    --     only on an @ACTIVE@ stream.
    --
    -- -   @UPDATING@ - Shards in the stream are being merged or split. Read
    --     and write operations continue to work while the stream is in the
    --     @UPDATING@ state.
    streamStatus :: StreamStatus,
    -- | The shards that comprise the stream.
    shards :: [Shard],
    -- | If set to @true@, more shards in the stream are available to describe.
    hasMoreShards :: Prelude.Bool,
    -- | The current retention period, in hours. Minimum value of 24. Maximum
    -- value of 168.
    retentionPeriodHours :: Prelude.Int,
    -- | The approximate time that the stream was created.
    streamCreationTimestamp :: Prelude.POSIX,
    -- | Represents the current enhanced monitoring settings of the stream.
    enhancedMonitoring :: [EnhancedMetrics]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StreamDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'streamDescription_encryptionType' - The server-side encryption type used on the stream. This parameter can
-- be one of the following values:
--
-- -   @NONE@: Do not encrypt the records in the stream.
--
-- -   @KMS@: Use server-side encryption on the records in the stream using
--     a customer-managed AWS KMS key.
--
-- 'keyId', 'streamDescription_keyId' - The GUID for the customer-managed AWS KMS key to use for encryption.
-- This value can be a globally unique identifier, a fully specified ARN to
-- either an alias or a key, or an alias name prefixed by \"alias\/\".You
-- can also use a master key owned by Kinesis Data Streams by specifying
-- the alias @aws\/kinesis@.
--
-- -   Key ARN example:
--     @arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- -   Alias ARN example:
--     @arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName@
--
-- -   Globally unique key ID example:
--     @12345678-1234-1234-1234-123456789012@
--
-- -   Alias name example: @alias\/MyAliasName@
--
-- -   Master key owned by Kinesis Data Streams: @alias\/aws\/kinesis@
--
-- 'streamName', 'streamDescription_streamName' - The name of the stream being described.
--
-- 'streamARN', 'streamDescription_streamARN' - The Amazon Resource Name (ARN) for the stream being described.
--
-- 'streamStatus', 'streamDescription_streamStatus' - The current status of the stream being described. The stream status is
-- one of the following states:
--
-- -   @CREATING@ - The stream is being created. Kinesis Data Streams
--     immediately returns and sets @StreamStatus@ to @CREATING@.
--
-- -   @DELETING@ - The stream is being deleted. The specified stream is in
--     the @DELETING@ state until Kinesis Data Streams completes the
--     deletion.
--
-- -   @ACTIVE@ - The stream exists and is ready for read and write
--     operations or deletion. You should perform read and write operations
--     only on an @ACTIVE@ stream.
--
-- -   @UPDATING@ - Shards in the stream are being merged or split. Read
--     and write operations continue to work while the stream is in the
--     @UPDATING@ state.
--
-- 'shards', 'streamDescription_shards' - The shards that comprise the stream.
--
-- 'hasMoreShards', 'streamDescription_hasMoreShards' - If set to @true@, more shards in the stream are available to describe.
--
-- 'retentionPeriodHours', 'streamDescription_retentionPeriodHours' - The current retention period, in hours. Minimum value of 24. Maximum
-- value of 168.
--
-- 'streamCreationTimestamp', 'streamDescription_streamCreationTimestamp' - The approximate time that the stream was created.
--
-- 'enhancedMonitoring', 'streamDescription_enhancedMonitoring' - Represents the current enhanced monitoring settings of the stream.
newStreamDescription ::
  -- | 'streamName'
  Prelude.Text ->
  -- | 'streamARN'
  Prelude.Text ->
  -- | 'streamStatus'
  StreamStatus ->
  -- | 'hasMoreShards'
  Prelude.Bool ->
  -- | 'retentionPeriodHours'
  Prelude.Int ->
  -- | 'streamCreationTimestamp'
  Prelude.UTCTime ->
  StreamDescription
newStreamDescription
  pStreamName_
  pStreamARN_
  pStreamStatus_
  pHasMoreShards_
  pRetentionPeriodHours_
  pStreamCreationTimestamp_ =
    StreamDescription'
      { encryptionType =
          Prelude.Nothing,
        keyId = Prelude.Nothing,
        streamName = pStreamName_,
        streamARN = pStreamARN_,
        streamStatus = pStreamStatus_,
        shards = Prelude.mempty,
        hasMoreShards = pHasMoreShards_,
        retentionPeriodHours = pRetentionPeriodHours_,
        streamCreationTimestamp =
          Prelude._Time Lens.# pStreamCreationTimestamp_,
        enhancedMonitoring = Prelude.mempty
      }

-- | The server-side encryption type used on the stream. This parameter can
-- be one of the following values:
--
-- -   @NONE@: Do not encrypt the records in the stream.
--
-- -   @KMS@: Use server-side encryption on the records in the stream using
--     a customer-managed AWS KMS key.
streamDescription_encryptionType :: Lens.Lens' StreamDescription (Prelude.Maybe EncryptionType)
streamDescription_encryptionType = Lens.lens (\StreamDescription' {encryptionType} -> encryptionType) (\s@StreamDescription' {} a -> s {encryptionType = a} :: StreamDescription)

-- | The GUID for the customer-managed AWS KMS key to use for encryption.
-- This value can be a globally unique identifier, a fully specified ARN to
-- either an alias or a key, or an alias name prefixed by \"alias\/\".You
-- can also use a master key owned by Kinesis Data Streams by specifying
-- the alias @aws\/kinesis@.
--
-- -   Key ARN example:
--     @arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- -   Alias ARN example:
--     @arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName@
--
-- -   Globally unique key ID example:
--     @12345678-1234-1234-1234-123456789012@
--
-- -   Alias name example: @alias\/MyAliasName@
--
-- -   Master key owned by Kinesis Data Streams: @alias\/aws\/kinesis@
streamDescription_keyId :: Lens.Lens' StreamDescription (Prelude.Maybe Prelude.Text)
streamDescription_keyId = Lens.lens (\StreamDescription' {keyId} -> keyId) (\s@StreamDescription' {} a -> s {keyId = a} :: StreamDescription)

-- | The name of the stream being described.
streamDescription_streamName :: Lens.Lens' StreamDescription Prelude.Text
streamDescription_streamName = Lens.lens (\StreamDescription' {streamName} -> streamName) (\s@StreamDescription' {} a -> s {streamName = a} :: StreamDescription)

-- | The Amazon Resource Name (ARN) for the stream being described.
streamDescription_streamARN :: Lens.Lens' StreamDescription Prelude.Text
streamDescription_streamARN = Lens.lens (\StreamDescription' {streamARN} -> streamARN) (\s@StreamDescription' {} a -> s {streamARN = a} :: StreamDescription)

-- | The current status of the stream being described. The stream status is
-- one of the following states:
--
-- -   @CREATING@ - The stream is being created. Kinesis Data Streams
--     immediately returns and sets @StreamStatus@ to @CREATING@.
--
-- -   @DELETING@ - The stream is being deleted. The specified stream is in
--     the @DELETING@ state until Kinesis Data Streams completes the
--     deletion.
--
-- -   @ACTIVE@ - The stream exists and is ready for read and write
--     operations or deletion. You should perform read and write operations
--     only on an @ACTIVE@ stream.
--
-- -   @UPDATING@ - Shards in the stream are being merged or split. Read
--     and write operations continue to work while the stream is in the
--     @UPDATING@ state.
streamDescription_streamStatus :: Lens.Lens' StreamDescription StreamStatus
streamDescription_streamStatus = Lens.lens (\StreamDescription' {streamStatus} -> streamStatus) (\s@StreamDescription' {} a -> s {streamStatus = a} :: StreamDescription)

-- | The shards that comprise the stream.
streamDescription_shards :: Lens.Lens' StreamDescription [Shard]
streamDescription_shards = Lens.lens (\StreamDescription' {shards} -> shards) (\s@StreamDescription' {} a -> s {shards = a} :: StreamDescription) Prelude.. Prelude._Coerce

-- | If set to @true@, more shards in the stream are available to describe.
streamDescription_hasMoreShards :: Lens.Lens' StreamDescription Prelude.Bool
streamDescription_hasMoreShards = Lens.lens (\StreamDescription' {hasMoreShards} -> hasMoreShards) (\s@StreamDescription' {} a -> s {hasMoreShards = a} :: StreamDescription)

-- | The current retention period, in hours. Minimum value of 24. Maximum
-- value of 168.
streamDescription_retentionPeriodHours :: Lens.Lens' StreamDescription Prelude.Int
streamDescription_retentionPeriodHours = Lens.lens (\StreamDescription' {retentionPeriodHours} -> retentionPeriodHours) (\s@StreamDescription' {} a -> s {retentionPeriodHours = a} :: StreamDescription)

-- | The approximate time that the stream was created.
streamDescription_streamCreationTimestamp :: Lens.Lens' StreamDescription Prelude.UTCTime
streamDescription_streamCreationTimestamp = Lens.lens (\StreamDescription' {streamCreationTimestamp} -> streamCreationTimestamp) (\s@StreamDescription' {} a -> s {streamCreationTimestamp = a} :: StreamDescription) Prelude.. Prelude._Time

-- | Represents the current enhanced monitoring settings of the stream.
streamDescription_enhancedMonitoring :: Lens.Lens' StreamDescription [EnhancedMetrics]
streamDescription_enhancedMonitoring = Lens.lens (\StreamDescription' {enhancedMonitoring} -> enhancedMonitoring) (\s@StreamDescription' {} a -> s {enhancedMonitoring = a} :: StreamDescription) Prelude.. Prelude._Coerce

instance Prelude.FromJSON StreamDescription where
  parseJSON =
    Prelude.withObject
      "StreamDescription"
      ( \x ->
          StreamDescription'
            Prelude.<$> (x Prelude..:? "EncryptionType")
            Prelude.<*> (x Prelude..:? "KeyId")
            Prelude.<*> (x Prelude..: "StreamName")
            Prelude.<*> (x Prelude..: "StreamARN")
            Prelude.<*> (x Prelude..: "StreamStatus")
            Prelude.<*> (x Prelude..:? "Shards" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..: "HasMoreShards")
            Prelude.<*> (x Prelude..: "RetentionPeriodHours")
            Prelude.<*> (x Prelude..: "StreamCreationTimestamp")
            Prelude.<*> ( x Prelude..:? "EnhancedMonitoring"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable StreamDescription

instance Prelude.NFData StreamDescription
