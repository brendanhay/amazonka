{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.PersonDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonDetail
  ( PersonDetail (..),

    -- * Smart constructor
    mkPersonDetail,

    -- * Lenses
    pdBoundingBox,
    pdFace,
    pdIndex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.BoundingBox as Types
import qualified Network.AWS.Rekognition.Types.FaceDetail as Types

-- | Details about a person detected in a video analysis request.
--
-- /See:/ 'mkPersonDetail' smart constructor.
data PersonDetail = PersonDetail'
  { -- | Bounding box around the detected person.
    boundingBox :: Core.Maybe Types.BoundingBox,
    -- | Face details for the detected person.
    face :: Core.Maybe Types.FaceDetail,
    -- | Identifier for the person detected person within a video. Use to keep track of the person throughout the video. The identifier is not stored by Amazon Rekognition.
    index :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PersonDetail' value with any optional fields omitted.
mkPersonDetail ::
  PersonDetail
mkPersonDetail =
  PersonDetail'
    { boundingBox = Core.Nothing,
      face = Core.Nothing,
      index = Core.Nothing
    }

-- | Bounding box around the detected person.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdBoundingBox :: Lens.Lens' PersonDetail (Core.Maybe Types.BoundingBox)
pdBoundingBox = Lens.field @"boundingBox"
{-# DEPRECATED pdBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | Face details for the detected person.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdFace :: Lens.Lens' PersonDetail (Core.Maybe Types.FaceDetail)
pdFace = Lens.field @"face"
{-# DEPRECATED pdFace "Use generic-lens or generic-optics with 'face' instead." #-}

-- | Identifier for the person detected person within a video. Use to keep track of the person throughout the video. The identifier is not stored by Amazon Rekognition.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdIndex :: Lens.Lens' PersonDetail (Core.Maybe Core.Integer)
pdIndex = Lens.field @"index"
{-# DEPRECATED pdIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Core.FromJSON PersonDetail where
  parseJSON =
    Core.withObject "PersonDetail" Core.$
      \x ->
        PersonDetail'
          Core.<$> (x Core..:? "BoundingBox")
          Core.<*> (x Core..:? "Face")
          Core.<*> (x Core..:? "Index")
