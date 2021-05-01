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
-- Module      : Network.AWS.Firehose.Types.KinesisStreamSourceDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KinesisStreamSourceDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about a Kinesis data stream used as the source for a Kinesis
-- Data Firehose delivery stream.
--
-- /See:/ 'newKinesisStreamSourceDescription' smart constructor.
data KinesisStreamSourceDescription = KinesisStreamSourceDescription'
  { -- | The ARN of the role used by the source Kinesis data stream. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format>.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Kinesis Data Firehose starts retrieving records from the Kinesis data
    -- stream starting with this timestamp.
    deliveryStartTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the source Kinesis data stream. For
    -- more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format>.
    kinesisStreamARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamSourceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'kinesisStreamSourceDescription_roleARN' - The ARN of the role used by the source Kinesis data stream. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format>.
--
-- 'deliveryStartTimestamp', 'kinesisStreamSourceDescription_deliveryStartTimestamp' - Kinesis Data Firehose starts retrieving records from the Kinesis data
-- stream starting with this timestamp.
--
-- 'kinesisStreamARN', 'kinesisStreamSourceDescription_kinesisStreamARN' - The Amazon Resource Name (ARN) of the source Kinesis data stream. For
-- more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format>.
newKinesisStreamSourceDescription ::
  KinesisStreamSourceDescription
newKinesisStreamSourceDescription =
  KinesisStreamSourceDescription'
    { roleARN =
        Prelude.Nothing,
      deliveryStartTimestamp = Prelude.Nothing,
      kinesisStreamARN = Prelude.Nothing
    }

-- | The ARN of the role used by the source Kinesis data stream. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format>.
kinesisStreamSourceDescription_roleARN :: Lens.Lens' KinesisStreamSourceDescription (Prelude.Maybe Prelude.Text)
kinesisStreamSourceDescription_roleARN = Lens.lens (\KinesisStreamSourceDescription' {roleARN} -> roleARN) (\s@KinesisStreamSourceDescription' {} a -> s {roleARN = a} :: KinesisStreamSourceDescription)

-- | Kinesis Data Firehose starts retrieving records from the Kinesis data
-- stream starting with this timestamp.
kinesisStreamSourceDescription_deliveryStartTimestamp :: Lens.Lens' KinesisStreamSourceDescription (Prelude.Maybe Prelude.UTCTime)
kinesisStreamSourceDescription_deliveryStartTimestamp = Lens.lens (\KinesisStreamSourceDescription' {deliveryStartTimestamp} -> deliveryStartTimestamp) (\s@KinesisStreamSourceDescription' {} a -> s {deliveryStartTimestamp = a} :: KinesisStreamSourceDescription) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the source Kinesis data stream. For
-- more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format>.
kinesisStreamSourceDescription_kinesisStreamARN :: Lens.Lens' KinesisStreamSourceDescription (Prelude.Maybe Prelude.Text)
kinesisStreamSourceDescription_kinesisStreamARN = Lens.lens (\KinesisStreamSourceDescription' {kinesisStreamARN} -> kinesisStreamARN) (\s@KinesisStreamSourceDescription' {} a -> s {kinesisStreamARN = a} :: KinesisStreamSourceDescription)

instance
  Prelude.FromJSON
    KinesisStreamSourceDescription
  where
  parseJSON =
    Prelude.withObject
      "KinesisStreamSourceDescription"
      ( \x ->
          KinesisStreamSourceDescription'
            Prelude.<$> (x Prelude..:? "RoleARN")
            Prelude.<*> (x Prelude..:? "DeliveryStartTimestamp")
            Prelude.<*> (x Prelude..:? "KinesisStreamARN")
      )

instance
  Prelude.Hashable
    KinesisStreamSourceDescription

instance
  Prelude.NFData
    KinesisStreamSourceDescription
