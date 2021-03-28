{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.SegmentTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.SegmentTypeInfo
  ( SegmentTypeInfo (..)
  -- * Smart constructor
  , mkSegmentTypeInfo
  -- * Lenses
  , stiModelVersion
  , stiType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.SegmentType as Types

-- | Information about the type of a segment requested in a call to 'StartSegmentDetection' . An array of @SegmentTypeInfo@ objects is returned by the response from 'GetSegmentDetection' .
--
-- /See:/ 'mkSegmentTypeInfo' smart constructor.
data SegmentTypeInfo = SegmentTypeInfo'
  { modelVersion :: Core.Maybe Core.Text
    -- ^ The version of the model used to detect segments.
  , type' :: Core.Maybe Types.SegmentType
    -- ^ The type of a segment (technical cue or shot detection).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentTypeInfo' value with any optional fields omitted.
mkSegmentTypeInfo
    :: SegmentTypeInfo
mkSegmentTypeInfo
  = SegmentTypeInfo'{modelVersion = Core.Nothing,
                     type' = Core.Nothing}

-- | The version of the model used to detect segments.
--
-- /Note:/ Consider using 'modelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stiModelVersion :: Lens.Lens' SegmentTypeInfo (Core.Maybe Core.Text)
stiModelVersion = Lens.field @"modelVersion"
{-# INLINEABLE stiModelVersion #-}
{-# DEPRECATED modelVersion "Use generic-lens or generic-optics with 'modelVersion' instead"  #-}

-- | The type of a segment (technical cue or shot detection).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stiType :: Lens.Lens' SegmentTypeInfo (Core.Maybe Types.SegmentType)
stiType = Lens.field @"type'"
{-# INLINEABLE stiType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON SegmentTypeInfo where
        parseJSON
          = Core.withObject "SegmentTypeInfo" Core.$
              \ x ->
                SegmentTypeInfo' Core.<$>
                  (x Core..:? "ModelVersion") Core.<*> x Core..:? "Type"
