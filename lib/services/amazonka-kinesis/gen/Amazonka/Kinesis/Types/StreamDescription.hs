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
-- Module      : Amazonka.Kinesis.Types.StreamDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.StreamDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.EncryptionType
import Amazonka.Kinesis.Types.EnhancedMetrics
import Amazonka.Kinesis.Types.Shard
import Amazonka.Kinesis.Types.StreamModeDetails
import Amazonka.Kinesis.Types.StreamStatus
import qualified Amazonka.Prelude as Prelude

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
    --     a customer-managed Amazon Web Services KMS key.
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | The GUID for the customer-managed Amazon Web Services KMS key to use for
    -- encryption. This value can be a globally unique identifier, a fully
    -- specified ARN to either an alias or a key, or an alias name prefixed by
    -- \"alias\/\".You can also use a master key owned by Kinesis Data Streams
    -- by specifying the alias @aws\/kinesis@.
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
    -- | Specifies the capacity mode to which you want to set your data stream.
    -- Currently, in Kinesis Data Streams, you can choose between an
    -- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
    -- data streams.
    streamModeDetails :: Prelude.Maybe StreamModeDetails,
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
    streamCreationTimestamp :: Data.POSIX,
    -- | Represents the current enhanced monitoring settings of the stream.
    enhancedMonitoring :: [EnhancedMetrics]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--     a customer-managed Amazon Web Services KMS key.
--
-- 'keyId', 'streamDescription_keyId' - The GUID for the customer-managed Amazon Web Services KMS key to use for
-- encryption. This value can be a globally unique identifier, a fully
-- specified ARN to either an alias or a key, or an alias name prefixed by
-- \"alias\/\".You can also use a master key owned by Kinesis Data Streams
-- by specifying the alias @aws\/kinesis@.
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
-- 'streamModeDetails', 'streamDescription_streamModeDetails' - Specifies the capacity mode to which you want to set your data stream.
-- Currently, in Kinesis Data Streams, you can choose between an
-- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
-- data streams.
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
        streamModeDetails = Prelude.Nothing,
        streamName = pStreamName_,
        streamARN = pStreamARN_,
        streamStatus = pStreamStatus_,
        shards = Prelude.mempty,
        hasMoreShards = pHasMoreShards_,
        retentionPeriodHours = pRetentionPeriodHours_,
        streamCreationTimestamp =
          Data._Time Lens.# pStreamCreationTimestamp_,
        enhancedMonitoring = Prelude.mempty
      }

-- | The server-side encryption type used on the stream. This parameter can
-- be one of the following values:
--
-- -   @NONE@: Do not encrypt the records in the stream.
--
-- -   @KMS@: Use server-side encryption on the records in the stream using
--     a customer-managed Amazon Web Services KMS key.
streamDescription_encryptionType :: Lens.Lens' StreamDescription (Prelude.Maybe EncryptionType)
streamDescription_encryptionType = Lens.lens (\StreamDescription' {encryptionType} -> encryptionType) (\s@StreamDescription' {} a -> s {encryptionType = a} :: StreamDescription)

-- | The GUID for the customer-managed Amazon Web Services KMS key to use for
-- encryption. This value can be a globally unique identifier, a fully
-- specified ARN to either an alias or a key, or an alias name prefixed by
-- \"alias\/\".You can also use a master key owned by Kinesis Data Streams
-- by specifying the alias @aws\/kinesis@.
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

-- | Specifies the capacity mode to which you want to set your data stream.
-- Currently, in Kinesis Data Streams, you can choose between an
-- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
-- data streams.
streamDescription_streamModeDetails :: Lens.Lens' StreamDescription (Prelude.Maybe StreamModeDetails)
streamDescription_streamModeDetails = Lens.lens (\StreamDescription' {streamModeDetails} -> streamModeDetails) (\s@StreamDescription' {} a -> s {streamModeDetails = a} :: StreamDescription)

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
streamDescription_shards = Lens.lens (\StreamDescription' {shards} -> shards) (\s@StreamDescription' {} a -> s {shards = a} :: StreamDescription) Prelude.. Lens.coerced

-- | If set to @true@, more shards in the stream are available to describe.
streamDescription_hasMoreShards :: Lens.Lens' StreamDescription Prelude.Bool
streamDescription_hasMoreShards = Lens.lens (\StreamDescription' {hasMoreShards} -> hasMoreShards) (\s@StreamDescription' {} a -> s {hasMoreShards = a} :: StreamDescription)

-- | The current retention period, in hours. Minimum value of 24. Maximum
-- value of 168.
streamDescription_retentionPeriodHours :: Lens.Lens' StreamDescription Prelude.Int
streamDescription_retentionPeriodHours = Lens.lens (\StreamDescription' {retentionPeriodHours} -> retentionPeriodHours) (\s@StreamDescription' {} a -> s {retentionPeriodHours = a} :: StreamDescription)

-- | The approximate time that the stream was created.
streamDescription_streamCreationTimestamp :: Lens.Lens' StreamDescription Prelude.UTCTime
streamDescription_streamCreationTimestamp = Lens.lens (\StreamDescription' {streamCreationTimestamp} -> streamCreationTimestamp) (\s@StreamDescription' {} a -> s {streamCreationTimestamp = a} :: StreamDescription) Prelude.. Data._Time

-- | Represents the current enhanced monitoring settings of the stream.
streamDescription_enhancedMonitoring :: Lens.Lens' StreamDescription [EnhancedMetrics]
streamDescription_enhancedMonitoring = Lens.lens (\StreamDescription' {enhancedMonitoring} -> enhancedMonitoring) (\s@StreamDescription' {} a -> s {enhancedMonitoring = a} :: StreamDescription) Prelude.. Lens.coerced

instance Data.FromJSON StreamDescription where
  parseJSON =
    Data.withObject
      "StreamDescription"
      ( \x ->
          StreamDescription'
            Prelude.<$> (x Data..:? "EncryptionType")
            Prelude.<*> (x Data..:? "KeyId")
            Prelude.<*> (x Data..:? "StreamModeDetails")
            Prelude.<*> (x Data..: "StreamName")
            Prelude.<*> (x Data..: "StreamARN")
            Prelude.<*> (x Data..: "StreamStatus")
            Prelude.<*> (x Data..:? "Shards" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "HasMoreShards")
            Prelude.<*> (x Data..: "RetentionPeriodHours")
            Prelude.<*> (x Data..: "StreamCreationTimestamp")
            Prelude.<*> ( x
                            Data..:? "EnhancedMonitoring"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable StreamDescription where
  hashWithSalt _salt StreamDescription' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionType
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` streamModeDetails
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamStatus
      `Prelude.hashWithSalt` shards
      `Prelude.hashWithSalt` hasMoreShards
      `Prelude.hashWithSalt` retentionPeriodHours
      `Prelude.hashWithSalt` streamCreationTimestamp
      `Prelude.hashWithSalt` enhancedMonitoring

instance Prelude.NFData StreamDescription where
  rnf StreamDescription' {..} =
    Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf streamModeDetails
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamStatus
      `Prelude.seq` Prelude.rnf shards
      `Prelude.seq` Prelude.rnf hasMoreShards
      `Prelude.seq` Prelude.rnf retentionPeriodHours
      `Prelude.seq` Prelude.rnf streamCreationTimestamp
      `Prelude.seq` Prelude.rnf enhancedMonitoring
