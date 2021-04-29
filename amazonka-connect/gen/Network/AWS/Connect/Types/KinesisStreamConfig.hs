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
-- Module      : Network.AWS.Connect.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisStreamConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information of a Kinesis data stream.
--
-- /See:/ 'newKinesisStreamConfig' smart constructor.
data KinesisStreamConfig = KinesisStreamConfig'
  { -- | The Amazon Resource Name (ARN) of the data stream.
    streamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  KinesisStreamConfig
newKinesisStreamConfig pStreamArn_ =
  KinesisStreamConfig' {streamArn = pStreamArn_}

-- | The Amazon Resource Name (ARN) of the data stream.
kinesisStreamConfig_streamArn :: Lens.Lens' KinesisStreamConfig Prelude.Text
kinesisStreamConfig_streamArn = Lens.lens (\KinesisStreamConfig' {streamArn} -> streamArn) (\s@KinesisStreamConfig' {} a -> s {streamArn = a} :: KinesisStreamConfig)

instance Prelude.FromJSON KinesisStreamConfig where
  parseJSON =
    Prelude.withObject
      "KinesisStreamConfig"
      ( \x ->
          KinesisStreamConfig'
            Prelude.<$> (x Prelude..: "StreamArn")
      )

instance Prelude.Hashable KinesisStreamConfig

instance Prelude.NFData KinesisStreamConfig

instance Prelude.ToJSON KinesisStreamConfig where
  toJSON KinesisStreamConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("StreamArn" Prelude..= streamArn)]
      )
