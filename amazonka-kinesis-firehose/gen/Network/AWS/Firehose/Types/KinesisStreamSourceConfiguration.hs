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
-- Module      : Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format>.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format>.
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
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format>.
kinesisStreamSourceConfiguration_roleARN :: Lens.Lens' KinesisStreamSourceConfiguration Prelude.Text
kinesisStreamSourceConfiguration_roleARN = Lens.lens (\KinesisStreamSourceConfiguration' {roleARN} -> roleARN) (\s@KinesisStreamSourceConfiguration' {} a -> s {roleARN = a} :: KinesisStreamSourceConfiguration)

instance
  Prelude.Hashable
    KinesisStreamSourceConfiguration

instance
  Prelude.NFData
    KinesisStreamSourceConfiguration

instance
  Prelude.ToJSON
    KinesisStreamSourceConfiguration
  where
  toJSON KinesisStreamSourceConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KinesisStreamARN" Prelude..= kinesisStreamARN),
            Prelude.Just ("RoleARN" Prelude..= roleARN)
          ]
      )
