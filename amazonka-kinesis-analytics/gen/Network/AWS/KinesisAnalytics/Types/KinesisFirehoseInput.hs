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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifies an Amazon Kinesis Firehose delivery stream as the streaming
-- source. You provide the delivery stream\'s Amazon Resource Name (ARN)
-- and an IAM role ARN that enables Amazon Kinesis Analytics to access the
-- stream on your behalf.
--
-- /See:/ 'newKinesisFirehoseInput' smart constructor.
data KinesisFirehoseInput = KinesisFirehoseInput'
  { -- | ARN of the input delivery stream.
    resourceARN :: Core.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream on your behalf. You need to make sure that the role has the
    -- necessary permissions to access the stream.
    roleARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KinesisFirehoseInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisFirehoseInput_resourceARN' - ARN of the input delivery stream.
--
-- 'roleARN', 'kinesisFirehoseInput_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to make sure that the role has the
-- necessary permissions to access the stream.
newKinesisFirehoseInput ::
  -- | 'resourceARN'
  Core.Text ->
  -- | 'roleARN'
  Core.Text ->
  KinesisFirehoseInput
newKinesisFirehoseInput pResourceARN_ pRoleARN_ =
  KinesisFirehoseInput'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the input delivery stream.
kinesisFirehoseInput_resourceARN :: Lens.Lens' KinesisFirehoseInput Core.Text
kinesisFirehoseInput_resourceARN = Lens.lens (\KinesisFirehoseInput' {resourceARN} -> resourceARN) (\s@KinesisFirehoseInput' {} a -> s {resourceARN = a} :: KinesisFirehoseInput)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to make sure that the role has the
-- necessary permissions to access the stream.
kinesisFirehoseInput_roleARN :: Lens.Lens' KinesisFirehoseInput Core.Text
kinesisFirehoseInput_roleARN = Lens.lens (\KinesisFirehoseInput' {roleARN} -> roleARN) (\s@KinesisFirehoseInput' {} a -> s {roleARN = a} :: KinesisFirehoseInput)

instance Core.Hashable KinesisFirehoseInput

instance Core.NFData KinesisFirehoseInput

instance Core.ToJSON KinesisFirehoseInput where
  toJSON KinesisFirehoseInput' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARN" Core..= resourceARN),
            Core.Just ("RoleARN" Core..= roleARN)
          ]
      )
