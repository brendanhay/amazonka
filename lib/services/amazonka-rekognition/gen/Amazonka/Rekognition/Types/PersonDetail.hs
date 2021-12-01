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
-- Module      : Amazonka.Rekognition.Types.PersonDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.PersonDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.FaceDetail

-- | Details about a person detected in a video analysis request.
--
-- /See:/ 'newPersonDetail' smart constructor.
data PersonDetail = PersonDetail'
  { -- | Bounding box around the detected person.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | Identifier for the person detected person within a video. Use to keep
    -- track of the person throughout the video. The identifier is not stored
    -- by Amazon Rekognition.
    index :: Prelude.Maybe Prelude.Integer,
    -- | Face details for the detected person.
    face :: Prelude.Maybe FaceDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PersonDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBox', 'personDetail_boundingBox' - Bounding box around the detected person.
--
-- 'index', 'personDetail_index' - Identifier for the person detected person within a video. Use to keep
-- track of the person throughout the video. The identifier is not stored
-- by Amazon Rekognition.
--
-- 'face', 'personDetail_face' - Face details for the detected person.
newPersonDetail ::
  PersonDetail
newPersonDetail =
  PersonDetail'
    { boundingBox = Prelude.Nothing,
      index = Prelude.Nothing,
      face = Prelude.Nothing
    }

-- | Bounding box around the detected person.
personDetail_boundingBox :: Lens.Lens' PersonDetail (Prelude.Maybe BoundingBox)
personDetail_boundingBox = Lens.lens (\PersonDetail' {boundingBox} -> boundingBox) (\s@PersonDetail' {} a -> s {boundingBox = a} :: PersonDetail)

-- | Identifier for the person detected person within a video. Use to keep
-- track of the person throughout the video. The identifier is not stored
-- by Amazon Rekognition.
personDetail_index :: Lens.Lens' PersonDetail (Prelude.Maybe Prelude.Integer)
personDetail_index = Lens.lens (\PersonDetail' {index} -> index) (\s@PersonDetail' {} a -> s {index = a} :: PersonDetail)

-- | Face details for the detected person.
personDetail_face :: Lens.Lens' PersonDetail (Prelude.Maybe FaceDetail)
personDetail_face = Lens.lens (\PersonDetail' {face} -> face) (\s@PersonDetail' {} a -> s {face = a} :: PersonDetail)

instance Core.FromJSON PersonDetail where
  parseJSON =
    Core.withObject
      "PersonDetail"
      ( \x ->
          PersonDetail'
            Prelude.<$> (x Core..:? "BoundingBox")
            Prelude.<*> (x Core..:? "Index")
            Prelude.<*> (x Core..:? "Face")
      )

instance Prelude.Hashable PersonDetail where
  hashWithSalt salt' PersonDetail' {..} =
    salt' `Prelude.hashWithSalt` face
      `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` boundingBox

instance Prelude.NFData PersonDetail where
  rnf PersonDetail' {..} =
    Prelude.rnf boundingBox
      `Prelude.seq` Prelude.rnf face
      `Prelude.seq` Prelude.rnf index
