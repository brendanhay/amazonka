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
-- Module      : Network.AWS.Connect.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisStreamConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration information of a Kinesis data stream.
--
-- /See:/ 'newKinesisStreamConfig' smart constructor.
data KinesisStreamConfig = KinesisStreamConfig'
  { -- | The Amazon Resource Name (ARN) of the data stream.
    streamArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KinesisStreamConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamArn', 'kinesisStreamConfig_streamArn' - The Amazon Resource Name (ARN) of the data stream.
newKinesisStreamConfig ::
  -- | 'streamArn'
  Core.Text ->
  KinesisStreamConfig
newKinesisStreamConfig pStreamArn_ =
  KinesisStreamConfig' {streamArn = pStreamArn_}

-- | The Amazon Resource Name (ARN) of the data stream.
kinesisStreamConfig_streamArn :: Lens.Lens' KinesisStreamConfig Core.Text
kinesisStreamConfig_streamArn = Lens.lens (\KinesisStreamConfig' {streamArn} -> streamArn) (\s@KinesisStreamConfig' {} a -> s {streamArn = a} :: KinesisStreamConfig)

instance Core.FromJSON KinesisStreamConfig where
  parseJSON =
    Core.withObject
      "KinesisStreamConfig"
      ( \x ->
          KinesisStreamConfig'
            Core.<$> (x Core..: "StreamArn")
      )

instance Core.Hashable KinesisStreamConfig

instance Core.NFData KinesisStreamConfig

instance Core.ToJSON KinesisStreamConfig where
  toJSON KinesisStreamConfig' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("StreamArn" Core..= streamArn)]
      )
