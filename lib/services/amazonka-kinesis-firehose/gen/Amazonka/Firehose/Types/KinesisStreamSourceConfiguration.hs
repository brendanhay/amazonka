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
-- Module      : Amazonka.Firehose.Types.KinesisStreamSourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.KinesisStreamSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The stream and role Amazon Resource Names (ARNs) for a Kinesis data
-- stream used as the source for a delivery stream.
--
-- /See:/ 'newKinesisStreamSourceConfiguration' smart constructor.
data KinesisStreamSourceConfiguration = KinesisStreamSourceConfiguration'
  { -- | The ARN of the source Kinesis data stream. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format>.
    kinesisStreamARN :: Prelude.Text,
    -- | The ARN of the role that provides access to the source Kinesis data
    -- stream. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM) ARN Format>.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisStreamARN', 'kinesisStreamSourceConfiguration_kinesisStreamARN' - The ARN of the source Kinesis data stream. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format>.
--
-- 'roleARN', 'kinesisStreamSourceConfiguration_roleARN' - The ARN of the role that provides access to the source Kinesis data
-- stream. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM) ARN Format>.
newKinesisStreamSourceConfiguration ::
  -- | 'kinesisStreamARN'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  KinesisStreamSourceConfiguration
newKinesisStreamSourceConfiguration
  pKinesisStreamARN_
  pRoleARN_ =
    KinesisStreamSourceConfiguration'
      { kinesisStreamARN =
          pKinesisStreamARN_,
        roleARN = pRoleARN_
      }

-- | The ARN of the source Kinesis data stream. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format>.
kinesisStreamSourceConfiguration_kinesisStreamARN :: Lens.Lens' KinesisStreamSourceConfiguration Prelude.Text
kinesisStreamSourceConfiguration_kinesisStreamARN = Lens.lens (\KinesisStreamSourceConfiguration' {kinesisStreamARN} -> kinesisStreamARN) (\s@KinesisStreamSourceConfiguration' {} a -> s {kinesisStreamARN = a} :: KinesisStreamSourceConfiguration)

-- | The ARN of the role that provides access to the source Kinesis data
-- stream. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM) ARN Format>.
kinesisStreamSourceConfiguration_roleARN :: Lens.Lens' KinesisStreamSourceConfiguration Prelude.Text
kinesisStreamSourceConfiguration_roleARN = Lens.lens (\KinesisStreamSourceConfiguration' {roleARN} -> roleARN) (\s@KinesisStreamSourceConfiguration' {} a -> s {roleARN = a} :: KinesisStreamSourceConfiguration)

instance
  Prelude.Hashable
    KinesisStreamSourceConfiguration
  where
  hashWithSalt
    _salt
    KinesisStreamSourceConfiguration' {..} =
      _salt `Prelude.hashWithSalt` kinesisStreamARN
        `Prelude.hashWithSalt` roleARN

instance
  Prelude.NFData
    KinesisStreamSourceConfiguration
  where
  rnf KinesisStreamSourceConfiguration' {..} =
    Prelude.rnf kinesisStreamARN
      `Prelude.seq` Prelude.rnf roleARN

instance Data.ToJSON KinesisStreamSourceConfiguration where
  toJSON KinesisStreamSourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KinesisStreamARN" Data..= kinesisStreamARN),
            Prelude.Just ("RoleARN" Data..= roleARN)
          ]
      )
