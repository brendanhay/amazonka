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
-- Module      : Network.AWS.Rekognition.Types.KinesisDataStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.KinesisDataStream where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON KinesisDataStream where
  parseJSON =
    Prelude.withObject
      "KinesisDataStream"
      ( \x ->
          KinesisDataStream' Prelude.<$> (x Prelude..:? "Arn")
      )

instance Prelude.Hashable KinesisDataStream

instance Prelude.NFData KinesisDataStream

instance Prelude.ToJSON KinesisDataStream where
  toJSON KinesisDataStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Arn" Prelude..=) Prelude.<$> arn]
      )
