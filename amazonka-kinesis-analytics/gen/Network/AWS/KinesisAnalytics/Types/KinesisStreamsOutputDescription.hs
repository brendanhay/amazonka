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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | For an application output, describes the Amazon Kinesis stream
-- configured as its destination.
--
-- /See:/ 'newKinesisStreamsOutputDescription' smart constructor.
data KinesisStreamsOutputDescription = KinesisStreamsOutputDescription'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
    resourceARN :: Core.Maybe Core.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream.
    roleARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KinesisStreamsOutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisStreamsOutputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- 'roleARN', 'kinesisStreamsOutputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
newKinesisStreamsOutputDescription ::
  KinesisStreamsOutputDescription
newKinesisStreamsOutputDescription =
  KinesisStreamsOutputDescription'
    { resourceARN =
        Core.Nothing,
      roleARN = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
kinesisStreamsOutputDescription_resourceARN :: Lens.Lens' KinesisStreamsOutputDescription (Core.Maybe Core.Text)
kinesisStreamsOutputDescription_resourceARN = Lens.lens (\KinesisStreamsOutputDescription' {resourceARN} -> resourceARN) (\s@KinesisStreamsOutputDescription' {} a -> s {resourceARN = a} :: KinesisStreamsOutputDescription)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
kinesisStreamsOutputDescription_roleARN :: Lens.Lens' KinesisStreamsOutputDescription (Core.Maybe Core.Text)
kinesisStreamsOutputDescription_roleARN = Lens.lens (\KinesisStreamsOutputDescription' {roleARN} -> roleARN) (\s@KinesisStreamsOutputDescription' {} a -> s {roleARN = a} :: KinesisStreamsOutputDescription)

instance
  Core.FromJSON
    KinesisStreamsOutputDescription
  where
  parseJSON =
    Core.withObject
      "KinesisStreamsOutputDescription"
      ( \x ->
          KinesisStreamsOutputDescription'
            Core.<$> (x Core..:? "ResourceARN")
            Core.<*> (x Core..:? "RoleARN")
      )

instance
  Core.Hashable
    KinesisStreamsOutputDescription

instance Core.NFData KinesisStreamsOutputDescription
