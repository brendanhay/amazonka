{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.SegmentTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.SegmentTypeInfo
  ( SegmentTypeInfo (..),

    -- * Smart constructor
    mkSegmentTypeInfo,

    -- * Lenses
    stiModelVersion,
    stiType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.SegmentType as Types
import qualified Network.AWS.Rekognition.Types.String as Types

-- | Information about the type of a segment requested in a call to 'StartSegmentDetection' . An array of @SegmentTypeInfo@ objects is returned by the response from 'GetSegmentDetection' .
--
-- /See:/ 'mkSegmentTypeInfo' smart constructor.
data SegmentTypeInfo = SegmentTypeInfo'
  { -- | The version of the model used to detect segments.
    modelVersion :: Core.Maybe Types.String,
    -- | The type of a segment (technical cue or shot detection).
    type' :: Core.Maybe Types.SegmentType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentTypeInfo' value with any optional fields omitted.
mkSegmentTypeInfo ::
  SegmentTypeInfo
mkSegmentTypeInfo =
  SegmentTypeInfo'
    { modelVersion = Core.Nothing,
      type' = Core.Nothing
    }

-- | The version of the model used to detect segments.
--
-- /Note:/ Consider using 'modelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stiModelVersion :: Lens.Lens' SegmentTypeInfo (Core.Maybe Types.String)
stiModelVersion = Lens.field @"modelVersion"
{-# DEPRECATED stiModelVersion "Use generic-lens or generic-optics with 'modelVersion' instead." #-}

-- | The type of a segment (technical cue or shot detection).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stiType :: Lens.Lens' SegmentTypeInfo (Core.Maybe Types.SegmentType)
stiType = Lens.field @"type'"
{-# DEPRECATED stiType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON SegmentTypeInfo where
  parseJSON =
    Core.withObject "SegmentTypeInfo" Core.$
      \x ->
        SegmentTypeInfo'
          Core.<$> (x Core..:? "ModelVersion") Core.<*> (x Core..:? "Type")
