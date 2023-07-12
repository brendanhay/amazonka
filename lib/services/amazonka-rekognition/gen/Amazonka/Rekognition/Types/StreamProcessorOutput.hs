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
-- Module      : Amazonka.Rekognition.Types.StreamProcessorOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessorOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.KinesisDataStream
import Amazonka.Rekognition.Types.S3Destination

-- | Information about the Amazon Kinesis Data Streams stream to which a
-- Amazon Rekognition Video stream processor streams the results of a video
-- analysis. For more information, see CreateStreamProcessor in the Amazon
-- Rekognition Developer Guide.
--
-- /See:/ 'newStreamProcessorOutput' smart constructor.
data StreamProcessorOutput = StreamProcessorOutput'
  { -- | The Amazon Kinesis Data Streams stream to which the Amazon Rekognition
    -- stream processor streams the analysis results.
    kinesisDataStream :: Prelude.Maybe KinesisDataStream,
    -- | The Amazon S3 bucket location to which Amazon Rekognition publishes the
    -- detailed inference results of a video analysis operation.
    s3Destination :: Prelude.Maybe S3Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamProcessorOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisDataStream', 'streamProcessorOutput_kinesisDataStream' - The Amazon Kinesis Data Streams stream to which the Amazon Rekognition
-- stream processor streams the analysis results.
--
-- 's3Destination', 'streamProcessorOutput_s3Destination' - The Amazon S3 bucket location to which Amazon Rekognition publishes the
-- detailed inference results of a video analysis operation.
newStreamProcessorOutput ::
  StreamProcessorOutput
newStreamProcessorOutput =
  StreamProcessorOutput'
    { kinesisDataStream =
        Prelude.Nothing,
      s3Destination = Prelude.Nothing
    }

-- | The Amazon Kinesis Data Streams stream to which the Amazon Rekognition
-- stream processor streams the analysis results.
streamProcessorOutput_kinesisDataStream :: Lens.Lens' StreamProcessorOutput (Prelude.Maybe KinesisDataStream)
streamProcessorOutput_kinesisDataStream = Lens.lens (\StreamProcessorOutput' {kinesisDataStream} -> kinesisDataStream) (\s@StreamProcessorOutput' {} a -> s {kinesisDataStream = a} :: StreamProcessorOutput)

-- | The Amazon S3 bucket location to which Amazon Rekognition publishes the
-- detailed inference results of a video analysis operation.
streamProcessorOutput_s3Destination :: Lens.Lens' StreamProcessorOutput (Prelude.Maybe S3Destination)
streamProcessorOutput_s3Destination = Lens.lens (\StreamProcessorOutput' {s3Destination} -> s3Destination) (\s@StreamProcessorOutput' {} a -> s {s3Destination = a} :: StreamProcessorOutput)

instance Data.FromJSON StreamProcessorOutput where
  parseJSON =
    Data.withObject
      "StreamProcessorOutput"
      ( \x ->
          StreamProcessorOutput'
            Prelude.<$> (x Data..:? "KinesisDataStream")
            Prelude.<*> (x Data..:? "S3Destination")
      )

instance Prelude.Hashable StreamProcessorOutput where
  hashWithSalt _salt StreamProcessorOutput' {..} =
    _salt
      `Prelude.hashWithSalt` kinesisDataStream
      `Prelude.hashWithSalt` s3Destination

instance Prelude.NFData StreamProcessorOutput where
  rnf StreamProcessorOutput' {..} =
    Prelude.rnf kinesisDataStream
      `Prelude.seq` Prelude.rnf s3Destination

instance Data.ToJSON StreamProcessorOutput where
  toJSON StreamProcessorOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KinesisDataStream" Data..=)
              Prelude.<$> kinesisDataStream,
            ("S3Destination" Data..=) Prelude.<$> s3Destination
          ]
      )
