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
-- Module      : Network.AWS.Rekognition.Types.FaceDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceDetection where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.FaceDetail

-- | Information about a face detected in a video analysis request and the
-- time the face was detected in the video.
--
-- /See:/ 'newFaceDetection' smart constructor.
data FaceDetection = FaceDetection'
  { -- | The face properties for the detected face.
    face :: Prelude.Maybe FaceDetail,
    -- | Time, in milliseconds from the start of the video, that the face was
    -- detected.
    timestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FaceDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'face', 'faceDetection_face' - The face properties for the detected face.
--
-- 'timestamp', 'faceDetection_timestamp' - Time, in milliseconds from the start of the video, that the face was
-- detected.
newFaceDetection ::
  FaceDetection
newFaceDetection =
  FaceDetection'
    { face = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The face properties for the detected face.
faceDetection_face :: Lens.Lens' FaceDetection (Prelude.Maybe FaceDetail)
faceDetection_face = Lens.lens (\FaceDetection' {face} -> face) (\s@FaceDetection' {} a -> s {face = a} :: FaceDetection)

-- | Time, in milliseconds from the start of the video, that the face was
-- detected.
faceDetection_timestamp :: Lens.Lens' FaceDetection (Prelude.Maybe Prelude.Integer)
faceDetection_timestamp = Lens.lens (\FaceDetection' {timestamp} -> timestamp) (\s@FaceDetection' {} a -> s {timestamp = a} :: FaceDetection)

instance Prelude.FromJSON FaceDetection where
  parseJSON =
    Prelude.withObject
      "FaceDetection"
      ( \x ->
          FaceDetection'
            Prelude.<$> (x Prelude..:? "Face")
            Prelude.<*> (x Prelude..:? "Timestamp")
      )

instance Prelude.Hashable FaceDetection

instance Prelude.NFData FaceDetection
