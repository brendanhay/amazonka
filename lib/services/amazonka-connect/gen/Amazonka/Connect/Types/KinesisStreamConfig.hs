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
-- Module      : Amazonka.Connect.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.KinesisStreamConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information of a Kinesis data stream.
--
-- /See:/ 'newKinesisStreamConfig' smart constructor.
data KinesisStreamConfig = KinesisStreamConfig'
  { -- | The Amazon Resource Name (ARN) of the data stream.
    streamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON KinesisStreamConfig where
  parseJSON =
    Data.withObject
      "KinesisStreamConfig"
      ( \x ->
          KinesisStreamConfig'
            Prelude.<$> (x Data..: "StreamArn")
      )

instance Prelude.Hashable KinesisStreamConfig where
  hashWithSalt _salt KinesisStreamConfig' {..} =
    _salt `Prelude.hashWithSalt` streamArn

instance Prelude.NFData KinesisStreamConfig where
  rnf KinesisStreamConfig' {..} = Prelude.rnf streamArn

instance Data.ToJSON KinesisStreamConfig where
  toJSON KinesisStreamConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("StreamArn" Data..= streamArn)]
      )
