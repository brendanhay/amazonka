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
-- Module      : Network.AWS.Rekognition.Types.UnindexedFace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.UnindexedFace where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.FaceDetail
import Network.AWS.Rekognition.Types.Reason

-- | A face that IndexFaces detected, but didn\'t index. Use the @Reasons@
-- response attribute to determine why a face wasn\'t indexed.
--
-- /See:/ 'newUnindexedFace' smart constructor.
data UnindexedFace = UnindexedFace'
  { -- | The structure that contains attributes of a face that
    -- @IndexFaces@detected, but didn\'t index.
    faceDetail :: Prelude.Maybe FaceDetail,
    -- | An array of reasons that specify why a face wasn\'t indexed.
    --
    -- -   EXTREME_POSE - The face is at a pose that can\'t be detected. For
    --     example, the head is turned too far away from the camera.
    --
    -- -   EXCEEDS_MAX_FACES - The number of faces detected is already higher
    --     than that specified by the @MaxFaces@ input parameter for
    --     @IndexFaces@.
    --
    -- -   LOW_BRIGHTNESS - The image is too dark.
    --
    -- -   LOW_SHARPNESS - The image is too blurry.
    --
    -- -   LOW_CONFIDENCE - The face was detected with a low confidence.
    --
    -- -   SMALL_BOUNDING_BOX - The bounding box around the face is too small.
    reasons :: Prelude.Maybe [Reason]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnindexedFace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceDetail', 'unindexedFace_faceDetail' - The structure that contains attributes of a face that
-- @IndexFaces@detected, but didn\'t index.
--
-- 'reasons', 'unindexedFace_reasons' - An array of reasons that specify why a face wasn\'t indexed.
--
-- -   EXTREME_POSE - The face is at a pose that can\'t be detected. For
--     example, the head is turned too far away from the camera.
--
-- -   EXCEEDS_MAX_FACES - The number of faces detected is already higher
--     than that specified by the @MaxFaces@ input parameter for
--     @IndexFaces@.
--
-- -   LOW_BRIGHTNESS - The image is too dark.
--
-- -   LOW_SHARPNESS - The image is too blurry.
--
-- -   LOW_CONFIDENCE - The face was detected with a low confidence.
--
-- -   SMALL_BOUNDING_BOX - The bounding box around the face is too small.
newUnindexedFace ::
  UnindexedFace
newUnindexedFace =
  UnindexedFace'
    { faceDetail = Prelude.Nothing,
      reasons = Prelude.Nothing
    }

-- | The structure that contains attributes of a face that
-- @IndexFaces@detected, but didn\'t index.
unindexedFace_faceDetail :: Lens.Lens' UnindexedFace (Prelude.Maybe FaceDetail)
unindexedFace_faceDetail = Lens.lens (\UnindexedFace' {faceDetail} -> faceDetail) (\s@UnindexedFace' {} a -> s {faceDetail = a} :: UnindexedFace)

-- | An array of reasons that specify why a face wasn\'t indexed.
--
-- -   EXTREME_POSE - The face is at a pose that can\'t be detected. For
--     example, the head is turned too far away from the camera.
--
-- -   EXCEEDS_MAX_FACES - The number of faces detected is already higher
--     than that specified by the @MaxFaces@ input parameter for
--     @IndexFaces@.
--
-- -   LOW_BRIGHTNESS - The image is too dark.
--
-- -   LOW_SHARPNESS - The image is too blurry.
--
-- -   LOW_CONFIDENCE - The face was detected with a low confidence.
--
-- -   SMALL_BOUNDING_BOX - The bounding box around the face is too small.
unindexedFace_reasons :: Lens.Lens' UnindexedFace (Prelude.Maybe [Reason])
unindexedFace_reasons = Lens.lens (\UnindexedFace' {reasons} -> reasons) (\s@UnindexedFace' {} a -> s {reasons = a} :: UnindexedFace) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON UnindexedFace where
  parseJSON =
    Prelude.withObject
      "UnindexedFace"
      ( \x ->
          UnindexedFace'
            Prelude.<$> (x Prelude..:? "FaceDetail")
            Prelude.<*> (x Prelude..:? "Reasons" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable UnindexedFace

instance Prelude.NFData UnindexedFace
