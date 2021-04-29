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
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorInput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.KinesisVideoStream

-- | Information about the source streaming video.
--
-- /See:/ 'newStreamProcessorInput' smart constructor.
data StreamProcessorInput = StreamProcessorInput'
  { -- | The Kinesis video stream input stream for the source streaming video.
    kinesisVideoStream :: Prelude.Maybe KinesisVideoStream
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON StreamProcessorInput where
  parseJSON =
    Prelude.withObject
      "StreamProcessorInput"
      ( \x ->
          StreamProcessorInput'
            Prelude.<$> (x Prelude..:? "KinesisVideoStream")
      )

instance Prelude.Hashable StreamProcessorInput

instance Prelude.NFData StreamProcessorInput

instance Prelude.ToJSON StreamProcessorInput where
  toJSON StreamProcessorInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KinesisVideoStream" Prelude..=)
              Prelude.<$> kinesisVideoStream
          ]
      )
