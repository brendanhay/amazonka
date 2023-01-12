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
-- Module      : Amazonka.Rekognition.Types.KinesisDataStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.KinesisDataStream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Kinesis data stream Amazon Rekognition to which the analysis results
-- of a Amazon Rekognition stream processor are streamed. For more
-- information, see CreateStreamProcessor in the Amazon Rekognition
-- Developer Guide.
--
-- /See:/ 'newKinesisDataStream' smart constructor.
data KinesisDataStream = KinesisDataStream'
  { -- | ARN of the output Amazon Kinesis Data Streams stream.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisDataStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'kinesisDataStream_arn' - ARN of the output Amazon Kinesis Data Streams stream.
newKinesisDataStream ::
  KinesisDataStream
newKinesisDataStream =
  KinesisDataStream' {arn = Prelude.Nothing}

-- | ARN of the output Amazon Kinesis Data Streams stream.
kinesisDataStream_arn :: Lens.Lens' KinesisDataStream (Prelude.Maybe Prelude.Text)
kinesisDataStream_arn = Lens.lens (\KinesisDataStream' {arn} -> arn) (\s@KinesisDataStream' {} a -> s {arn = a} :: KinesisDataStream)

instance Data.FromJSON KinesisDataStream where
  parseJSON =
    Data.withObject
      "KinesisDataStream"
      ( \x ->
          KinesisDataStream' Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable KinesisDataStream where
  hashWithSalt _salt KinesisDataStream' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData KinesisDataStream where
  rnf KinesisDataStream' {..} = Prelude.rnf arn

instance Data.ToJSON KinesisDataStream where
  toJSON KinesisDataStream' {..} =
    Data.object
      (Prelude.catMaybes [("Arn" Data..=) Prelude.<$> arn])
