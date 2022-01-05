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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessorOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.KinesisDataStream

-- | Information about the Amazon Kinesis Data Streams stream to which a
-- Amazon Rekognition Video stream processor streams the results of a video
-- analysis. For more information, see CreateStreamProcessor in the Amazon
-- Rekognition Developer Guide.
--
-- /See:/ 'newStreamProcessorOutput' smart constructor.
data StreamProcessorOutput = StreamProcessorOutput'
  { -- | The Amazon Kinesis Data Streams stream to which the Amazon Rekognition
    -- stream processor streams the analysis results.
    kinesisDataStream :: Prelude.Maybe KinesisDataStream
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
newStreamProcessorOutput ::
  StreamProcessorOutput
newStreamProcessorOutput =
  StreamProcessorOutput'
    { kinesisDataStream =
        Prelude.Nothing
    }

-- | The Amazon Kinesis Data Streams stream to which the Amazon Rekognition
-- stream processor streams the analysis results.
streamProcessorOutput_kinesisDataStream :: Lens.Lens' StreamProcessorOutput (Prelude.Maybe KinesisDataStream)
streamProcessorOutput_kinesisDataStream = Lens.lens (\StreamProcessorOutput' {kinesisDataStream} -> kinesisDataStream) (\s@StreamProcessorOutput' {} a -> s {kinesisDataStream = a} :: StreamProcessorOutput)

instance Core.FromJSON StreamProcessorOutput where
  parseJSON =
    Core.withObject
      "StreamProcessorOutput"
      ( \x ->
          StreamProcessorOutput'
            Prelude.<$> (x Core..:? "KinesisDataStream")
      )

instance Prelude.Hashable StreamProcessorOutput where
  hashWithSalt _salt StreamProcessorOutput' {..} =
    _salt `Prelude.hashWithSalt` kinesisDataStream

instance Prelude.NFData StreamProcessorOutput where
  rnf StreamProcessorOutput' {..} =
    Prelude.rnf kinesisDataStream

instance Core.ToJSON StreamProcessorOutput where
  toJSON StreamProcessorOutput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KinesisDataStream" Core..=)
              Prelude.<$> kinesisDataStream
          ]
      )
