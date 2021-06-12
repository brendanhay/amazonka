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
-- Module      : Network.AWS.Rekognition.Types.PersonDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.FaceDetail

-- | Details about a person detected in a video analysis request.
--
-- /See:/ 'newPersonDetail' smart constructor.
data PersonDetail = PersonDetail'
  { -- | Bounding box around the detected person.
    boundingBox :: Core.Maybe BoundingBox,
    -- | Face details for the detected person.
    face :: Core.Maybe FaceDetail,
    -- | Identifier for the person detected person within a video. Use to keep
    -- track of the person throughout the video. The identifier is not stored
    -- by Amazon Rekognition.
    index :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'face', 'personDetail_face' - Face details for the detected person.
--
-- 'index', 'personDetail_index' - Identifier for the person detected person within a video. Use to keep
-- track of the person throughout the video. The identifier is not stored
-- by Amazon Rekognition.
newPersonDetail ::
  PersonDetail
newPersonDetail =
  PersonDetail'
    { boundingBox = Core.Nothing,
      face = Core.Nothing,
      index = Core.Nothing
    }

-- | Bounding box around the detected person.
personDetail_boundingBox :: Lens.Lens' PersonDetail (Core.Maybe BoundingBox)
personDetail_boundingBox = Lens.lens (\PersonDetail' {boundingBox} -> boundingBox) (\s@PersonDetail' {} a -> s {boundingBox = a} :: PersonDetail)

-- | Face details for the detected person.
personDetail_face :: Lens.Lens' PersonDetail (Core.Maybe FaceDetail)
personDetail_face = Lens.lens (\PersonDetail' {face} -> face) (\s@PersonDetail' {} a -> s {face = a} :: PersonDetail)

-- | Identifier for the person detected person within a video. Use to keep
-- track of the person throughout the video. The identifier is not stored
-- by Amazon Rekognition.
personDetail_index :: Lens.Lens' PersonDetail (Core.Maybe Core.Integer)
personDetail_index = Lens.lens (\PersonDetail' {index} -> index) (\s@PersonDetail' {} a -> s {index = a} :: PersonDetail)

instance Core.FromJSON PersonDetail where
  parseJSON =
    Core.withObject
      "PersonDetail"
      ( \x ->
          PersonDetail'
            Core.<$> (x Core..:? "BoundingBox")
            Core.<*> (x Core..:? "Face")
            Core.<*> (x Core..:? "Index")
      )

instance Core.Hashable PersonDetail

instance Core.NFData PersonDetail
