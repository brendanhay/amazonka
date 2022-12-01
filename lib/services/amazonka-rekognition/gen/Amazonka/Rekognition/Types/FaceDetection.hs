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
-- Module      : Amazonka.Rekognition.Types.FaceDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.FaceDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.FaceDetail

-- | Information about a face detected in a video analysis request and the
-- time the face was detected in the video.
--
-- /See:/ 'newFaceDetection' smart constructor.
data FaceDetection = FaceDetection'
  { -- | Time, in milliseconds from the start of the video, that the face was
    -- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
    -- individual frame where the face first appears.
    timestamp :: Prelude.Maybe Prelude.Integer,
    -- | The face properties for the detected face.
    face :: Prelude.Maybe FaceDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaceDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'faceDetection_timestamp' - Time, in milliseconds from the start of the video, that the face was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the face first appears.
--
-- 'face', 'faceDetection_face' - The face properties for the detected face.
newFaceDetection ::
  FaceDetection
newFaceDetection =
  FaceDetection'
    { timestamp = Prelude.Nothing,
      face = Prelude.Nothing
    }

-- | Time, in milliseconds from the start of the video, that the face was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the face first appears.
faceDetection_timestamp :: Lens.Lens' FaceDetection (Prelude.Maybe Prelude.Integer)
faceDetection_timestamp = Lens.lens (\FaceDetection' {timestamp} -> timestamp) (\s@FaceDetection' {} a -> s {timestamp = a} :: FaceDetection)

-- | The face properties for the detected face.
faceDetection_face :: Lens.Lens' FaceDetection (Prelude.Maybe FaceDetail)
faceDetection_face = Lens.lens (\FaceDetection' {face} -> face) (\s@FaceDetection' {} a -> s {face = a} :: FaceDetection)

instance Core.FromJSON FaceDetection where
  parseJSON =
    Core.withObject
      "FaceDetection"
      ( \x ->
          FaceDetection'
            Prelude.<$> (x Core..:? "Timestamp")
            Prelude.<*> (x Core..:? "Face")
      )

instance Prelude.Hashable FaceDetection where
  hashWithSalt _salt FaceDetection' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` face

instance Prelude.NFData FaceDetection where
  rnf FaceDetection' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf face
