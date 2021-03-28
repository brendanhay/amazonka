{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Label
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Label
  ( Label (..)
  -- * Smart constructor
  , mkLabel
  -- * Lenses
  , lConfidence
  , lInstances
  , lName
  , lParents
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Instance as Types
import qualified Network.AWS.Rekognition.Types.Parent as Types

-- | Structure containing details about the detected label, including the name, detected instances, parent labels, and level of confidence.
--
--
--
-- /See:/ 'mkLabel' smart constructor.
data Label = Label'
  { confidence :: Core.Maybe Core.Double
    -- ^ Level of confidence.
  , instances :: Core.Maybe [Types.Instance]
    -- ^ If @Label@ represents an object, @Instances@ contains the bounding boxes for each instance of the detected object. Bounding boxes are returned for common object labels such as people, cars, furniture, apparel or pets.
  , name :: Core.Maybe Core.Text
    -- ^ The name (label) of the object or scene.
  , parents :: Core.Maybe [Types.Parent]
    -- ^ The parent labels for a label. The response includes all ancestor labels.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Label' value with any optional fields omitted.
mkLabel
    :: Label
mkLabel
  = Label'{confidence = Core.Nothing, instances = Core.Nothing,
           name = Core.Nothing, parents = Core.Nothing}

-- | Level of confidence.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lConfidence :: Lens.Lens' Label (Core.Maybe Core.Double)
lConfidence = Lens.field @"confidence"
{-# INLINEABLE lConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | If @Label@ represents an object, @Instances@ contains the bounding boxes for each instance of the detected object. Bounding boxes are returned for common object labels such as people, cars, furniture, apparel or pets.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lInstances :: Lens.Lens' Label (Core.Maybe [Types.Instance])
lInstances = Lens.field @"instances"
{-# INLINEABLE lInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | The name (label) of the object or scene.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Label (Core.Maybe Core.Text)
lName = Lens.field @"name"
{-# INLINEABLE lName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The parent labels for a label. The response includes all ancestor labels.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lParents :: Lens.Lens' Label (Core.Maybe [Types.Parent])
lParents = Lens.field @"parents"
{-# INLINEABLE lParents #-}
{-# DEPRECATED parents "Use generic-lens or generic-optics with 'parents' instead"  #-}

instance Core.FromJSON Label where
        parseJSON
          = Core.withObject "Label" Core.$
              \ x ->
                Label' Core.<$>
                  (x Core..:? "Confidence") Core.<*> x Core..:? "Instances" Core.<*>
                    x Core..:? "Name"
                    Core.<*> x Core..:? "Parents"
