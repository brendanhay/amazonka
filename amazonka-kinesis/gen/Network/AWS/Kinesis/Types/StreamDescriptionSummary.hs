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
-- Module      : Network.AWS.Kinesis.Types.StreamDescriptionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.StreamDescriptionSummary where

import Network.AWS.Kinesis.Types.EncryptionType
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.StreamStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output for DescribeStreamSummary
--
-- /See:/ 'newStreamDescriptionSummary' smart constructor.
data StreamDescriptionSummary = StreamDescriptionSummary'
  { -- | The encryption type used. This value is one of the following:
    --
    -- -   @KMS@
    --
    -- -   @NONE@
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | The number of enhanced fan-out consumers registered with the stream.
    consumerCount :: Prelude.Maybe Prelude.Natural,
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
    --     @ arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName@
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
    -- | The current retention period, in hours.
    retentionPeriodHours :: Prelude.Int,
    -- | The approximate time that the stream was created.
    streamCreationTimestamp :: Prelude.POSIX,
    -- | Represents the current enhanced monitoring settings of the stream.
    enhancedMonitoring :: [EnhancedMetrics],
    -- | The number of open shards in the stream.
    openShardCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StreamDescriptionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'streamDescriptionSummary_encryptionType' - The encryption type used. This value is one of the following:
--
-- -   @KMS@
--
-- -   @NONE@
--
-- 'consumerCount', 'streamDescriptionSummary_consumerCount' - The number of enhanced fan-out consumers registered with the stream.
--
-- 'keyId', 'streamDescriptionSummary_keyId' - The GUID for the customer-managed AWS KMS key to use for encryption.
-- This value can be a globally unique identifier, a fully specified ARN to
-- either an alias or a key, or an alias name prefixed by \"alias\/\".You
-- can also use a master key owned by Kinesis Data Streams by specifying
-- the alias @aws\/kinesis@.
--
-- -   Key ARN example:
--     @arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- -   Alias ARN example:
--     @ arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName@
--
-- -   Globally unique key ID example:
--     @12345678-1234-1234-1234-123456789012@
--
-- -   Alias name example: @alias\/MyAliasName@
--
-- -   Master key owned by Kinesis Data Streams: @alias\/aws\/kinesis@
--
-- 'streamName', 'streamDescriptionSummary_streamName' - The name of the stream being described.
--
-- 'streamARN', 'streamDescriptionSummary_streamARN' - The Amazon Resource Name (ARN) for the stream being described.
--
-- 'streamStatus', 'streamDescriptionSummary_streamStatus' - The current status of the stream being described. The stream status is
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
-- 'retentionPeriodHours', 'streamDescriptionSummary_retentionPeriodHours' - The current retention period, in hours.
--
-- 'streamCreationTimestamp', 'streamDescriptionSummary_streamCreationTimestamp' - The approximate time that the stream was created.
--
-- 'enhancedMonitoring', 'streamDescriptionSummary_enhancedMonitoring' - Represents the current enhanced monitoring settings of the stream.
--
-- 'openShardCount', 'streamDescriptionSummary_openShardCount' - The number of open shards in the stream.
newStreamDescriptionSummary ::
  -- | 'streamName'
  Prelude.Text ->
  -- | 'streamARN'
  Prelude.Text ->
  -- | 'streamStatus'
  StreamStatus ->
  -- | 'retentionPeriodHours'
  Prelude.Int ->
  -- | 'streamCreationTimestamp'
  Prelude.UTCTime ->
  -- | 'openShardCount'
  Prelude.Natural ->
  StreamDescriptionSummary
newStreamDescriptionSummary
  pStreamName_
  pStreamARN_
  pStreamStatus_
  pRetentionPeriodHours_
  pStreamCreationTimestamp_
  pOpenShardCount_ =
    StreamDescriptionSummary'
      { encryptionType =
          Prelude.Nothing,
        consumerCount = Prelude.Nothing,
        keyId = Prelude.Nothing,
        streamName = pStreamName_,
        streamARN = pStreamARN_,
        streamStatus = pStreamStatus_,
        retentionPeriodHours = pRetentionPeriodHours_,
        streamCreationTimestamp =
          Prelude._Time Lens.# pStreamCreationTimestamp_,
        enhancedMonitoring = Prelude.mempty,
        openShardCount = pOpenShardCount_
      }

-- | The encryption type used. This value is one of the following:
--
-- -   @KMS@
--
-- -   @NONE@
streamDescriptionSummary_encryptionType :: Lens.Lens' StreamDescriptionSummary (Prelude.Maybe EncryptionType)
streamDescriptionSummary_encryptionType = Lens.lens (\StreamDescriptionSummary' {encryptionType} -> encryptionType) (\s@StreamDescriptionSummary' {} a -> s {encryptionType = a} :: StreamDescriptionSummary)

-- | The number of enhanced fan-out consumers registered with the stream.
streamDescriptionSummary_consumerCount :: Lens.Lens' StreamDescriptionSummary (Prelude.Maybe Prelude.Natural)
streamDescriptionSummary_consumerCount = Lens.lens (\StreamDescriptionSummary' {consumerCount} -> consumerCount) (\s@StreamDescriptionSummary' {} a -> s {consumerCount = a} :: StreamDescriptionSummary)

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
--     @ arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName@
--
-- -   Globally unique key ID example:
--     @12345678-1234-1234-1234-123456789012@
--
-- -   Alias name example: @alias\/MyAliasName@
--
-- -   Master key owned by Kinesis Data Streams: @alias\/aws\/kinesis@
streamDescriptionSummary_keyId :: Lens.Lens' StreamDescriptionSummary (Prelude.Maybe Prelude.Text)
streamDescriptionSummary_keyId = Lens.lens (\StreamDescriptionSummary' {keyId} -> keyId) (\s@StreamDescriptionSummary' {} a -> s {keyId = a} :: StreamDescriptionSummary)

-- | The name of the stream being described.
streamDescriptionSummary_streamName :: Lens.Lens' StreamDescriptionSummary Prelude.Text
streamDescriptionSummary_streamName = Lens.lens (\StreamDescriptionSummary' {streamName} -> streamName) (\s@StreamDescriptionSummary' {} a -> s {streamName = a} :: StreamDescriptionSummary)

-- | The Amazon Resource Name (ARN) for the stream being described.
streamDescriptionSummary_streamARN :: Lens.Lens' StreamDescriptionSummary Prelude.Text
streamDescriptionSummary_streamARN = Lens.lens (\StreamDescriptionSummary' {streamARN} -> streamARN) (\s@StreamDescriptionSummary' {} a -> s {streamARN = a} :: StreamDescriptionSummary)

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
streamDescriptionSummary_streamStatus :: Lens.Lens' StreamDescriptionSummary StreamStatus
streamDescriptionSummary_streamStatus = Lens.lens (\StreamDescriptionSummary' {streamStatus} -> streamStatus) (\s@StreamDescriptionSummary' {} a -> s {streamStatus = a} :: StreamDescriptionSummary)

-- | The current retention period, in hours.
streamDescriptionSummary_retentionPeriodHours :: Lens.Lens' StreamDescriptionSummary Prelude.Int
streamDescriptionSummary_retentionPeriodHours = Lens.lens (\StreamDescriptionSummary' {retentionPeriodHours} -> retentionPeriodHours) (\s@StreamDescriptionSummary' {} a -> s {retentionPeriodHours = a} :: StreamDescriptionSummary)

-- | The approximate time that the stream was created.
streamDescriptionSummary_streamCreationTimestamp :: Lens.Lens' StreamDescriptionSummary Prelude.UTCTime
streamDescriptionSummary_streamCreationTimestamp = Lens.lens (\StreamDescriptionSummary' {streamCreationTimestamp} -> streamCreationTimestamp) (\s@StreamDescriptionSummary' {} a -> s {streamCreationTimestamp = a} :: StreamDescriptionSummary) Prelude.. Prelude._Time

-- | Represents the current enhanced monitoring settings of the stream.
streamDescriptionSummary_enhancedMonitoring :: Lens.Lens' StreamDescriptionSummary [EnhancedMetrics]
streamDescriptionSummary_enhancedMonitoring = Lens.lens (\StreamDescriptionSummary' {enhancedMonitoring} -> enhancedMonitoring) (\s@StreamDescriptionSummary' {} a -> s {enhancedMonitoring = a} :: StreamDescriptionSummary) Prelude.. Prelude._Coerce

-- | The number of open shards in the stream.
streamDescriptionSummary_openShardCount :: Lens.Lens' StreamDescriptionSummary Prelude.Natural
streamDescriptionSummary_openShardCount = Lens.lens (\StreamDescriptionSummary' {openShardCount} -> openShardCount) (\s@StreamDescriptionSummary' {} a -> s {openShardCount = a} :: StreamDescriptionSummary)

instance Prelude.FromJSON StreamDescriptionSummary where
  parseJSON =
    Prelude.withObject
      "StreamDescriptionSummary"
      ( \x ->
          StreamDescriptionSummary'
            Prelude.<$> (x Prelude..:? "EncryptionType")
            Prelude.<*> (x Prelude..:? "ConsumerCount")
            Prelude.<*> (x Prelude..:? "KeyId")
            Prelude.<*> (x Prelude..: "StreamName")
            Prelude.<*> (x Prelude..: "StreamARN")
            Prelude.<*> (x Prelude..: "StreamStatus")
            Prelude.<*> (x Prelude..: "RetentionPeriodHours")
            Prelude.<*> (x Prelude..: "StreamCreationTimestamp")
            Prelude.<*> ( x Prelude..:? "EnhancedMonitoring"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "OpenShardCount")
      )

instance Prelude.Hashable StreamDescriptionSummary

instance Prelude.NFData StreamDescriptionSummary
