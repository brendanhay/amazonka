{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata
  ( BuiltinSlotTypeMetadata (..),

    -- * Smart constructor
    mkBuiltinSlotTypeMetadata,

    -- * Lenses
    bstmSignature,
    bstmSupportedLocales,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Locale as Types
import qualified Network.AWS.LexModels.Types.Signature as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about a built in slot type.
--
-- /See:/ 'mkBuiltinSlotTypeMetadata' smart constructor.
data BuiltinSlotTypeMetadata = BuiltinSlotTypeMetadata'
  { -- | A unique identifier for the built-in slot type. To find the signature for a slot type, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
    signature :: Core.Maybe Types.Signature,
    -- | A list of target locales for the slot.
    supportedLocales :: Core.Maybe [Types.Locale]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BuiltinSlotTypeMetadata' value with any optional fields omitted.
mkBuiltinSlotTypeMetadata ::
  BuiltinSlotTypeMetadata
mkBuiltinSlotTypeMetadata =
  BuiltinSlotTypeMetadata'
    { signature = Core.Nothing,
      supportedLocales = Core.Nothing
    }

-- | A unique identifier for the built-in slot type. To find the signature for a slot type, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bstmSignature :: Lens.Lens' BuiltinSlotTypeMetadata (Core.Maybe Types.Signature)
bstmSignature = Lens.field @"signature"
{-# DEPRECATED bstmSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | A list of target locales for the slot.
--
-- /Note:/ Consider using 'supportedLocales' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bstmSupportedLocales :: Lens.Lens' BuiltinSlotTypeMetadata (Core.Maybe [Types.Locale])
bstmSupportedLocales = Lens.field @"supportedLocales"
{-# DEPRECATED bstmSupportedLocales "Use generic-lens or generic-optics with 'supportedLocales' instead." #-}

instance Core.FromJSON BuiltinSlotTypeMetadata where
  parseJSON =
    Core.withObject "BuiltinSlotTypeMetadata" Core.$
      \x ->
        BuiltinSlotTypeMetadata'
          Core.<$> (x Core..:? "signature") Core.<*> (x Core..:? "supportedLocales")
