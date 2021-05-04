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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | When configuring application output, identifies an Amazon Kinesis
-- Firehose delivery stream as the destination. You provide the stream
-- Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis
-- Analytics to write to the stream on your behalf.
--
-- /See:/ 'newKinesisFirehoseOutput' smart constructor.
data KinesisFirehoseOutput = KinesisFirehoseOutput'
  { -- | ARN of the destination Amazon Kinesis Firehose delivery stream to write
    -- to.
    resourceARN :: Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
    -- the destination stream on your behalf. You need to grant the necessary
    -- permissions to this role.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisFirehoseOutput_resourceARN' - ARN of the destination Amazon Kinesis Firehose delivery stream to write
-- to.
--
-- 'roleARN', 'kinesisFirehoseOutput_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination stream on your behalf. You need to grant the necessary
-- permissions to this role.
newKinesisFirehoseOutput ::
  -- | 'resourceARN'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  KinesisFirehoseOutput
newKinesisFirehoseOutput pResourceARN_ pRoleARN_ =
  KinesisFirehoseOutput'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the destination Amazon Kinesis Firehose delivery stream to write
-- to.
kinesisFirehoseOutput_resourceARN :: Lens.Lens' KinesisFirehoseOutput Prelude.Text
kinesisFirehoseOutput_resourceARN = Lens.lens (\KinesisFirehoseOutput' {resourceARN} -> resourceARN) (\s@KinesisFirehoseOutput' {} a -> s {resourceARN = a} :: KinesisFirehoseOutput)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination stream on your behalf. You need to grant the necessary
-- permissions to this role.
kinesisFirehoseOutput_roleARN :: Lens.Lens' KinesisFirehoseOutput Prelude.Text
kinesisFirehoseOutput_roleARN = Lens.lens (\KinesisFirehoseOutput' {roleARN} -> roleARN) (\s@KinesisFirehoseOutput' {} a -> s {roleARN = a} :: KinesisFirehoseOutput)

instance Prelude.Hashable KinesisFirehoseOutput

instance Prelude.NFData KinesisFirehoseOutput

instance Prelude.ToJSON KinesisFirehoseOutput where
  toJSON KinesisFirehoseOutput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Prelude..= resourceARN),
            Prelude.Just ("RoleARN" Prelude..= roleARN)
          ]
      )
