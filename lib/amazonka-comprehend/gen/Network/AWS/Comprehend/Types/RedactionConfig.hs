{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.RedactionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.RedactionConfig
  ( RedactionConfig (..)
  -- * Smart constructor
  , mkRedactionConfig
  -- * Lenses
  , rcMaskCharacter
  , rcMaskMode
  , rcPiiEntityTypes
  ) where

import qualified Network.AWS.Comprehend.Types.MaskCharacter as Types
import qualified Network.AWS.Comprehend.Types.PiiEntitiesDetectionMaskMode as Types
import qualified Network.AWS.Comprehend.Types.PiiEntityType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides configuration parameters for PII entity redaction.
--
-- /See:/ 'mkRedactionConfig' smart constructor.
data RedactionConfig = RedactionConfig'
  { maskCharacter :: Core.Maybe Types.MaskCharacter
    -- ^ A character that replaces each character in the redacted PII entity.
  , maskMode :: Core.Maybe Types.PiiEntitiesDetectionMaskMode
    -- ^ Specifies whether the PII entity is redacted with the mask character or the entity type.
  , piiEntityTypes :: Core.Maybe [Types.PiiEntityType]
    -- ^ An array of the types of PII entities that Amazon Comprehend detects in the input text for your request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedactionConfig' value with any optional fields omitted.
mkRedactionConfig
    :: RedactionConfig
mkRedactionConfig
  = RedactionConfig'{maskCharacter = Core.Nothing,
                     maskMode = Core.Nothing, piiEntityTypes = Core.Nothing}

-- | A character that replaces each character in the redacted PII entity.
--
-- /Note:/ Consider using 'maskCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMaskCharacter :: Lens.Lens' RedactionConfig (Core.Maybe Types.MaskCharacter)
rcMaskCharacter = Lens.field @"maskCharacter"
{-# INLINEABLE rcMaskCharacter #-}
{-# DEPRECATED maskCharacter "Use generic-lens or generic-optics with 'maskCharacter' instead"  #-}

-- | Specifies whether the PII entity is redacted with the mask character or the entity type.
--
-- /Note:/ Consider using 'maskMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMaskMode :: Lens.Lens' RedactionConfig (Core.Maybe Types.PiiEntitiesDetectionMaskMode)
rcMaskMode = Lens.field @"maskMode"
{-# INLINEABLE rcMaskMode #-}
{-# DEPRECATED maskMode "Use generic-lens or generic-optics with 'maskMode' instead"  #-}

-- | An array of the types of PII entities that Amazon Comprehend detects in the input text for your request.
--
-- /Note:/ Consider using 'piiEntityTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcPiiEntityTypes :: Lens.Lens' RedactionConfig (Core.Maybe [Types.PiiEntityType])
rcPiiEntityTypes = Lens.field @"piiEntityTypes"
{-# INLINEABLE rcPiiEntityTypes #-}
{-# DEPRECATED piiEntityTypes "Use generic-lens or generic-optics with 'piiEntityTypes' instead"  #-}

instance Core.FromJSON RedactionConfig where
        toJSON RedactionConfig{..}
          = Core.object
              (Core.catMaybes
                 [("MaskCharacter" Core..=) Core.<$> maskCharacter,
                  ("MaskMode" Core..=) Core.<$> maskMode,
                  ("PiiEntityTypes" Core..=) Core.<$> piiEntityTypes])

instance Core.FromJSON RedactionConfig where
        parseJSON
          = Core.withObject "RedactionConfig" Core.$
              \ x ->
                RedactionConfig' Core.<$>
                  (x Core..:? "MaskCharacter") Core.<*> x Core..:? "MaskMode"
                    Core.<*> x Core..:? "PiiEntityTypes"
