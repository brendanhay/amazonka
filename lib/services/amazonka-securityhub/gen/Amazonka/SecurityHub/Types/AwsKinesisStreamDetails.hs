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
-- Module      : Amazonka.SecurityHub.Types.AwsKinesisStreamDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsKinesisStreamDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsKinesisStreamStreamEncryptionDetails

-- | Provides information about an Amazon Kinesis data stream.
--
-- /See:/ 'newAwsKinesisStreamDetails' smart constructor.
data AwsKinesisStreamDetails = AwsKinesisStreamDetails'
  { -- | The Amazon Resource Name (ARN) of the Kinesis data stream.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Kinesis stream. If you don\'t specify a name, CloudFront
    -- generates a unique physical ID and uses that ID for the stream name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of hours for the data records that are stored in shards to
    -- remain accessible.
    retentionPeriodHours :: Prelude.Maybe Prelude.Int,
    -- | The number of shards that the stream uses.
    shardCount :: Prelude.Maybe Prelude.Int,
    -- | When specified, enables or updates server-side encryption using an KMS
    -- key for a specified stream. Removing this property from your stack
    -- template and updating your stack disables encryption.
    streamEncryption :: Prelude.Maybe AwsKinesisStreamStreamEncryptionDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsKinesisStreamDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'awsKinesisStreamDetails_arn' - The Amazon Resource Name (ARN) of the Kinesis data stream.
--
-- 'name', 'awsKinesisStreamDetails_name' - The name of the Kinesis stream. If you don\'t specify a name, CloudFront
-- generates a unique physical ID and uses that ID for the stream name.
--
-- 'retentionPeriodHours', 'awsKinesisStreamDetails_retentionPeriodHours' - The number of hours for the data records that are stored in shards to
-- remain accessible.
--
-- 'shardCount', 'awsKinesisStreamDetails_shardCount' - The number of shards that the stream uses.
--
-- 'streamEncryption', 'awsKinesisStreamDetails_streamEncryption' - When specified, enables or updates server-side encryption using an KMS
-- key for a specified stream. Removing this property from your stack
-- template and updating your stack disables encryption.
newAwsKinesisStreamDetails ::
  AwsKinesisStreamDetails
newAwsKinesisStreamDetails =
  AwsKinesisStreamDetails'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      retentionPeriodHours = Prelude.Nothing,
      shardCount = Prelude.Nothing,
      streamEncryption = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Kinesis data stream.
awsKinesisStreamDetails_arn :: Lens.Lens' AwsKinesisStreamDetails (Prelude.Maybe Prelude.Text)
awsKinesisStreamDetails_arn = Lens.lens (\AwsKinesisStreamDetails' {arn} -> arn) (\s@AwsKinesisStreamDetails' {} a -> s {arn = a} :: AwsKinesisStreamDetails)

-- | The name of the Kinesis stream. If you don\'t specify a name, CloudFront
-- generates a unique physical ID and uses that ID for the stream name.
awsKinesisStreamDetails_name :: Lens.Lens' AwsKinesisStreamDetails (Prelude.Maybe Prelude.Text)
awsKinesisStreamDetails_name = Lens.lens (\AwsKinesisStreamDetails' {name} -> name) (\s@AwsKinesisStreamDetails' {} a -> s {name = a} :: AwsKinesisStreamDetails)

-- | The number of hours for the data records that are stored in shards to
-- remain accessible.
awsKinesisStreamDetails_retentionPeriodHours :: Lens.Lens' AwsKinesisStreamDetails (Prelude.Maybe Prelude.Int)
awsKinesisStreamDetails_retentionPeriodHours = Lens.lens (\AwsKinesisStreamDetails' {retentionPeriodHours} -> retentionPeriodHours) (\s@AwsKinesisStreamDetails' {} a -> s {retentionPeriodHours = a} :: AwsKinesisStreamDetails)

-- | The number of shards that the stream uses.
awsKinesisStreamDetails_shardCount :: Lens.Lens' AwsKinesisStreamDetails (Prelude.Maybe Prelude.Int)
awsKinesisStreamDetails_shardCount = Lens.lens (\AwsKinesisStreamDetails' {shardCount} -> shardCount) (\s@AwsKinesisStreamDetails' {} a -> s {shardCount = a} :: AwsKinesisStreamDetails)

-- | When specified, enables or updates server-side encryption using an KMS
-- key for a specified stream. Removing this property from your stack
-- template and updating your stack disables encryption.
awsKinesisStreamDetails_streamEncryption :: Lens.Lens' AwsKinesisStreamDetails (Prelude.Maybe AwsKinesisStreamStreamEncryptionDetails)
awsKinesisStreamDetails_streamEncryption = Lens.lens (\AwsKinesisStreamDetails' {streamEncryption} -> streamEncryption) (\s@AwsKinesisStreamDetails' {} a -> s {streamEncryption = a} :: AwsKinesisStreamDetails)

instance Data.FromJSON AwsKinesisStreamDetails where
  parseJSON =
    Data.withObject
      "AwsKinesisStreamDetails"
      ( \x ->
          AwsKinesisStreamDetails'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RetentionPeriodHours")
            Prelude.<*> (x Data..:? "ShardCount")
            Prelude.<*> (x Data..:? "StreamEncryption")
      )

instance Prelude.Hashable AwsKinesisStreamDetails where
  hashWithSalt _salt AwsKinesisStreamDetails' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` retentionPeriodHours
      `Prelude.hashWithSalt` shardCount
      `Prelude.hashWithSalt` streamEncryption

instance Prelude.NFData AwsKinesisStreamDetails where
  rnf AwsKinesisStreamDetails' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf retentionPeriodHours
      `Prelude.seq` Prelude.rnf shardCount
      `Prelude.seq` Prelude.rnf streamEncryption

instance Data.ToJSON AwsKinesisStreamDetails where
  toJSON AwsKinesisStreamDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arn" Data..=) Prelude.<$> arn,
            ("Name" Data..=) Prelude.<$> name,
            ("RetentionPeriodHours" Data..=)
              Prelude.<$> retentionPeriodHours,
            ("ShardCount" Data..=) Prelude.<$> shardCount,
            ("StreamEncryption" Data..=)
              Prelude.<$> streamEncryption
          ]
      )
