{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.LabelDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.LabelDetection
  ( LabelDetection (..)
  -- * Smart constructor
  , mkLabelDetection
  -- * Lenses
  , ldLabel
  , ldTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Label as Types

-- | Information about a label detected in a video analysis request and the time the label was detected in the video. 
--
-- /See:/ 'mkLabelDetection' smart constructor.
data LabelDetection = LabelDetection'
  { label :: Core.Maybe Types.Label
    -- ^ Details about the detected label.
  , timestamp :: Core.Maybe Core.Integer
    -- ^ Time, in milliseconds from the start of the video, that the label was detected.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LabelDetection' value with any optional fields omitted.
mkLabelDetection
    :: LabelDetection
mkLabelDetection
  = LabelDetection'{label = Core.Nothing, timestamp = Core.Nothing}

-- | Details about the detected label.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldLabel :: Lens.Lens' LabelDetection (Core.Maybe Types.Label)
ldLabel = Lens.field @"label"
{-# INLINEABLE ldLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

-- | Time, in milliseconds from the start of the video, that the label was detected.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldTimestamp :: Lens.Lens' LabelDetection (Core.Maybe Core.Integer)
ldTimestamp = Lens.field @"timestamp"
{-# INLINEABLE ldTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromJSON LabelDetection where
        parseJSON
          = Core.withObject "LabelDetection" Core.$
              \ x ->
                LabelDetection' Core.<$>
                  (x Core..:? "Label") Core.<*> x Core..:? "Timestamp"
