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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the Amazon Kinesis Firehose delivery stream that is configured
-- as the streaming source in the application input configuration.
--
-- /See:/ 'newKinesisFirehoseInputDescription' smart constructor.
data KinesisFirehoseInputDescription = KinesisFirehoseInputDescription'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
    -- stream.
    resourceARN :: Core.Maybe Core.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics assumes to access the
    -- stream.
    roleARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KinesisFirehoseInputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisFirehoseInputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
--
-- 'roleARN', 'kinesisFirehoseInputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics assumes to access the
-- stream.
newKinesisFirehoseInputDescription ::
  KinesisFirehoseInputDescription
newKinesisFirehoseInputDescription =
  KinesisFirehoseInputDescription'
    { resourceARN =
        Core.Nothing,
      roleARN = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
kinesisFirehoseInputDescription_resourceARN :: Lens.Lens' KinesisFirehoseInputDescription (Core.Maybe Core.Text)
kinesisFirehoseInputDescription_resourceARN = Lens.lens (\KinesisFirehoseInputDescription' {resourceARN} -> resourceARN) (\s@KinesisFirehoseInputDescription' {} a -> s {resourceARN = a} :: KinesisFirehoseInputDescription)

-- | ARN of the IAM role that Amazon Kinesis Analytics assumes to access the
-- stream.
kinesisFirehoseInputDescription_roleARN :: Lens.Lens' KinesisFirehoseInputDescription (Core.Maybe Core.Text)
kinesisFirehoseInputDescription_roleARN = Lens.lens (\KinesisFirehoseInputDescription' {roleARN} -> roleARN) (\s@KinesisFirehoseInputDescription' {} a -> s {roleARN = a} :: KinesisFirehoseInputDescription)

instance
  Core.FromJSON
    KinesisFirehoseInputDescription
  where
  parseJSON =
    Core.withObject
      "KinesisFirehoseInputDescription"
      ( \x ->
          KinesisFirehoseInputDescription'
            Core.<$> (x Core..:? "ResourceARN")
            Core.<*> (x Core..:? "RoleARN")
      )

instance
  Core.Hashable
    KinesisFirehoseInputDescription

instance Core.NFData KinesisFirehoseInputDescription
