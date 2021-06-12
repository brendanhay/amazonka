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
-- Module      : Network.AWS.Connect.Types.KinesisFirehoseConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisFirehoseConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration information of a Kinesis Data Firehose delivery stream.
--
-- /See:/ 'newKinesisFirehoseConfig' smart constructor.
data KinesisFirehoseConfig = KinesisFirehoseConfig'
  { -- | The Amazon Resource Name (ARN) of the delivery stream.
    firehoseArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KinesisFirehoseConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firehoseArn', 'kinesisFirehoseConfig_firehoseArn' - The Amazon Resource Name (ARN) of the delivery stream.
newKinesisFirehoseConfig ::
  -- | 'firehoseArn'
  Core.Text ->
  KinesisFirehoseConfig
newKinesisFirehoseConfig pFirehoseArn_ =
  KinesisFirehoseConfig' {firehoseArn = pFirehoseArn_}

-- | The Amazon Resource Name (ARN) of the delivery stream.
kinesisFirehoseConfig_firehoseArn :: Lens.Lens' KinesisFirehoseConfig Core.Text
kinesisFirehoseConfig_firehoseArn = Lens.lens (\KinesisFirehoseConfig' {firehoseArn} -> firehoseArn) (\s@KinesisFirehoseConfig' {} a -> s {firehoseArn = a} :: KinesisFirehoseConfig)

instance Core.FromJSON KinesisFirehoseConfig where
  parseJSON =
    Core.withObject
      "KinesisFirehoseConfig"
      ( \x ->
          KinesisFirehoseConfig'
            Core.<$> (x Core..: "FirehoseArn")
      )

instance Core.Hashable KinesisFirehoseConfig

instance Core.NFData KinesisFirehoseConfig

instance Core.ToJSON KinesisFirehoseConfig where
  toJSON KinesisFirehoseConfig' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("FirehoseArn" Core..= firehoseArn)]
      )
