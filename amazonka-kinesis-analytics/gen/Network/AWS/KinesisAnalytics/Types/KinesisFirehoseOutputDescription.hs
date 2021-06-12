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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | For an application output, describes the Amazon Kinesis Firehose
-- delivery stream configured as its destination.
--
-- /See:/ 'newKinesisFirehoseOutputDescription' smart constructor.
data KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
    -- stream.
    resourceARN :: Core.Maybe Core.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream.
    roleARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KinesisFirehoseOutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisFirehoseOutputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
--
-- 'roleARN', 'kinesisFirehoseOutputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
newKinesisFirehoseOutputDescription ::
  KinesisFirehoseOutputDescription
newKinesisFirehoseOutputDescription =
  KinesisFirehoseOutputDescription'
    { resourceARN =
        Core.Nothing,
      roleARN = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
kinesisFirehoseOutputDescription_resourceARN :: Lens.Lens' KinesisFirehoseOutputDescription (Core.Maybe Core.Text)
kinesisFirehoseOutputDescription_resourceARN = Lens.lens (\KinesisFirehoseOutputDescription' {resourceARN} -> resourceARN) (\s@KinesisFirehoseOutputDescription' {} a -> s {resourceARN = a} :: KinesisFirehoseOutputDescription)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
kinesisFirehoseOutputDescription_roleARN :: Lens.Lens' KinesisFirehoseOutputDescription (Core.Maybe Core.Text)
kinesisFirehoseOutputDescription_roleARN = Lens.lens (\KinesisFirehoseOutputDescription' {roleARN} -> roleARN) (\s@KinesisFirehoseOutputDescription' {} a -> s {roleARN = a} :: KinesisFirehoseOutputDescription)

instance
  Core.FromJSON
    KinesisFirehoseOutputDescription
  where
  parseJSON =
    Core.withObject
      "KinesisFirehoseOutputDescription"
      ( \x ->
          KinesisFirehoseOutputDescription'
            Core.<$> (x Core..:? "ResourceARN")
            Core.<*> (x Core..:? "RoleARN")
      )

instance
  Core.Hashable
    KinesisFirehoseOutputDescription

instance Core.NFData KinesisFirehoseOutputDescription
