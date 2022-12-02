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
-- Module      : Amazonka.Rekognition.Types.UnindexedFace
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UnindexedFace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.FaceDetail
import Amazonka.Rekognition.Types.Reason

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
unindexedFace_reasons = Lens.lens (\UnindexedFace' {reasons} -> reasons) (\s@UnindexedFace' {} a -> s {reasons = a} :: UnindexedFace) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UnindexedFace where
  parseJSON =
    Data.withObject
      "UnindexedFace"
      ( \x ->
          UnindexedFace'
            Prelude.<$> (x Data..:? "FaceDetail")
            Prelude.<*> (x Data..:? "Reasons" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UnindexedFace where
  hashWithSalt _salt UnindexedFace' {..} =
    _salt `Prelude.hashWithSalt` faceDetail
      `Prelude.hashWithSalt` reasons

instance Prelude.NFData UnindexedFace where
  rnf UnindexedFace' {..} =
    Prelude.rnf faceDetail
      `Prelude.seq` Prelude.rnf reasons
