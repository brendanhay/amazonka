{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CoversBodyPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.CoversBodyPart
  ( CoversBodyPart (..)
  -- * Smart constructor
  , mkCoversBodyPart
  -- * Lenses
  , cbpConfidence
  , cbpValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an item of Personal Protective Equipment covering a corresponding body part. For more information, see 'DetectProtectiveEquipment' .
--
-- /See:/ 'mkCoversBodyPart' smart constructor.
data CoversBodyPart = CoversBodyPart'
  { confidence :: Core.Maybe Core.Double
    -- ^ The confidence that Amazon Rekognition has in the value of @Value@ .
  , value :: Core.Maybe Core.Bool
    -- ^ True if the PPE covers the corresponding body part, otherwise false.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CoversBodyPart' value with any optional fields omitted.
mkCoversBodyPart
    :: CoversBodyPart
mkCoversBodyPart
  = CoversBodyPart'{confidence = Core.Nothing, value = Core.Nothing}

-- | The confidence that Amazon Rekognition has in the value of @Value@ .
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpConfidence :: Lens.Lens' CoversBodyPart (Core.Maybe Core.Double)
cbpConfidence = Lens.field @"confidence"
{-# INLINEABLE cbpConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | True if the PPE covers the corresponding body part, otherwise false.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpValue :: Lens.Lens' CoversBodyPart (Core.Maybe Core.Bool)
cbpValue = Lens.field @"value"
{-# INLINEABLE cbpValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON CoversBodyPart where
        parseJSON
          = Core.withObject "CoversBodyPart" Core.$
              \ x ->
                CoversBodyPart' Core.<$>
                  (x Core..:? "Confidence") Core.<*> x Core..:? "Value"
