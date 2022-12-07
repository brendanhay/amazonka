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
-- Module      : Amazonka.Rekognition.Types.StreamProcessorInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessorInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.KinesisVideoStream

-- | Information about the source streaming video.
--
-- /See:/ 'newStreamProcessorInput' smart constructor.
data StreamProcessorInput = StreamProcessorInput'
  { -- | The Kinesis video stream input stream for the source streaming video.
    kinesisVideoStream :: Prelude.Maybe KinesisVideoStream
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamProcessorInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisVideoStream', 'streamProcessorInput_kinesisVideoStream' - The Kinesis video stream input stream for the source streaming video.
newStreamProcessorInput ::
  StreamProcessorInput
newStreamProcessorInput =
  StreamProcessorInput'
    { kinesisVideoStream =
        Prelude.Nothing
    }

-- | The Kinesis video stream input stream for the source streaming video.
streamProcessorInput_kinesisVideoStream :: Lens.Lens' StreamProcessorInput (Prelude.Maybe KinesisVideoStream)
streamProcessorInput_kinesisVideoStream = Lens.lens (\StreamProcessorInput' {kinesisVideoStream} -> kinesisVideoStream) (\s@StreamProcessorInput' {} a -> s {kinesisVideoStream = a} :: StreamProcessorInput)

instance Data.FromJSON StreamProcessorInput where
  parseJSON =
    Data.withObject
      "StreamProcessorInput"
      ( \x ->
          StreamProcessorInput'
            Prelude.<$> (x Data..:? "KinesisVideoStream")
      )

instance Prelude.Hashable StreamProcessorInput where
  hashWithSalt _salt StreamProcessorInput' {..} =
    _salt `Prelude.hashWithSalt` kinesisVideoStream

instance Prelude.NFData StreamProcessorInput where
  rnf StreamProcessorInput' {..} =
    Prelude.rnf kinesisVideoStream

instance Data.ToJSON StreamProcessorInput where
  toJSON StreamProcessorInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KinesisVideoStream" Data..=)
              Prelude.<$> kinesisVideoStream
          ]
      )
