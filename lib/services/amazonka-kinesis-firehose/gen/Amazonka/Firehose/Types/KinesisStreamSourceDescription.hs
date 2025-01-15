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
-- Module      : Amazonka.Firehose.Types.KinesisStreamSourceDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.KinesisStreamSourceDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a Kinesis data stream used as the source for a Kinesis
-- Data Firehose delivery stream.
--
-- /See:/ 'newKinesisStreamSourceDescription' smart constructor.
data KinesisStreamSourceDescription = KinesisStreamSourceDescription'
  { -- | Kinesis Data Firehose starts retrieving records from the Kinesis data
    -- stream starting with this timestamp.
    deliveryStartTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the source Kinesis data stream. For
    -- more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format>.
    kinesisStreamARN :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role used by the source Kinesis data stream. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM) ARN Format>.
    roleARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamSourceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStartTimestamp', 'kinesisStreamSourceDescription_deliveryStartTimestamp' - Kinesis Data Firehose starts retrieving records from the Kinesis data
-- stream starting with this timestamp.
--
-- 'kinesisStreamARN', 'kinesisStreamSourceDescription_kinesisStreamARN' - The Amazon Resource Name (ARN) of the source Kinesis data stream. For
-- more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format>.
--
-- 'roleARN', 'kinesisStreamSourceDescription_roleARN' - The ARN of the role used by the source Kinesis data stream. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM) ARN Format>.
newKinesisStreamSourceDescription ::
  KinesisStreamSourceDescription
newKinesisStreamSourceDescription =
  KinesisStreamSourceDescription'
    { deliveryStartTimestamp =
        Prelude.Nothing,
      kinesisStreamARN = Prelude.Nothing,
      roleARN = Prelude.Nothing
    }

-- | Kinesis Data Firehose starts retrieving records from the Kinesis data
-- stream starting with this timestamp.
kinesisStreamSourceDescription_deliveryStartTimestamp :: Lens.Lens' KinesisStreamSourceDescription (Prelude.Maybe Prelude.UTCTime)
kinesisStreamSourceDescription_deliveryStartTimestamp = Lens.lens (\KinesisStreamSourceDescription' {deliveryStartTimestamp} -> deliveryStartTimestamp) (\s@KinesisStreamSourceDescription' {} a -> s {deliveryStartTimestamp = a} :: KinesisStreamSourceDescription) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the source Kinesis data stream. For
-- more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format>.
kinesisStreamSourceDescription_kinesisStreamARN :: Lens.Lens' KinesisStreamSourceDescription (Prelude.Maybe Prelude.Text)
kinesisStreamSourceDescription_kinesisStreamARN = Lens.lens (\KinesisStreamSourceDescription' {kinesisStreamARN} -> kinesisStreamARN) (\s@KinesisStreamSourceDescription' {} a -> s {kinesisStreamARN = a} :: KinesisStreamSourceDescription)

-- | The ARN of the role used by the source Kinesis data stream. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM) ARN Format>.
kinesisStreamSourceDescription_roleARN :: Lens.Lens' KinesisStreamSourceDescription (Prelude.Maybe Prelude.Text)
kinesisStreamSourceDescription_roleARN = Lens.lens (\KinesisStreamSourceDescription' {roleARN} -> roleARN) (\s@KinesisStreamSourceDescription' {} a -> s {roleARN = a} :: KinesisStreamSourceDescription)

instance Data.FromJSON KinesisStreamSourceDescription where
  parseJSON =
    Data.withObject
      "KinesisStreamSourceDescription"
      ( \x ->
          KinesisStreamSourceDescription'
            Prelude.<$> (x Data..:? "DeliveryStartTimestamp")
            Prelude.<*> (x Data..:? "KinesisStreamARN")
            Prelude.<*> (x Data..:? "RoleARN")
      )

instance
  Prelude.Hashable
    KinesisStreamSourceDescription
  where
  hashWithSalt
    _salt
    KinesisStreamSourceDescription' {..} =
      _salt
        `Prelude.hashWithSalt` deliveryStartTimestamp
        `Prelude.hashWithSalt` kinesisStreamARN
        `Prelude.hashWithSalt` roleARN

instance
  Prelude.NFData
    KinesisStreamSourceDescription
  where
  rnf KinesisStreamSourceDescription' {..} =
    Prelude.rnf deliveryStartTimestamp `Prelude.seq`
      Prelude.rnf kinesisStreamARN `Prelude.seq`
        Prelude.rnf roleARN
