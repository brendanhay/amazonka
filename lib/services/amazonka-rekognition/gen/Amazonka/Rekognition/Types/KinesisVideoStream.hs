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
-- Module      : Amazonka.Rekognition.Types.KinesisVideoStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.KinesisVideoStream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Kinesis video stream stream that provides the source streaming video for
-- a Amazon Rekognition Video stream processor. For more information, see
-- CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'newKinesisVideoStream' smart constructor.
data KinesisVideoStream = KinesisVideoStream'
  { -- | ARN of the Kinesis video stream stream that streams the source video.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisVideoStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'kinesisVideoStream_arn' - ARN of the Kinesis video stream stream that streams the source video.
newKinesisVideoStream ::
  KinesisVideoStream
newKinesisVideoStream =
  KinesisVideoStream' {arn = Prelude.Nothing}

-- | ARN of the Kinesis video stream stream that streams the source video.
kinesisVideoStream_arn :: Lens.Lens' KinesisVideoStream (Prelude.Maybe Prelude.Text)
kinesisVideoStream_arn = Lens.lens (\KinesisVideoStream' {arn} -> arn) (\s@KinesisVideoStream' {} a -> s {arn = a} :: KinesisVideoStream)

instance Data.FromJSON KinesisVideoStream where
  parseJSON =
    Data.withObject
      "KinesisVideoStream"
      ( \x ->
          KinesisVideoStream' Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable KinesisVideoStream where
  hashWithSalt _salt KinesisVideoStream' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData KinesisVideoStream where
  rnf KinesisVideoStream' {..} = Prelude.rnf arn

instance Data.ToJSON KinesisVideoStream where
  toJSON KinesisVideoStream' {..} =
    Data.object
      (Prelude.catMaybes [("Arn" Data..=) Prelude.<$> arn])
