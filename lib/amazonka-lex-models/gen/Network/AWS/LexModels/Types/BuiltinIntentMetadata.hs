{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BuiltinIntentMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.BuiltinIntentMetadata
  ( BuiltinIntentMetadata (..)
  -- * Smart constructor
  , mkBuiltinIntentMetadata
  -- * Lenses
  , bimSignature
  , bimSupportedLocales
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Locale as Types
import qualified Network.AWS.LexModels.Types.Signature as Types
import qualified Network.AWS.Prelude as Core

-- | Provides metadata for a built-in intent.
--
-- /See:/ 'mkBuiltinIntentMetadata' smart constructor.
data BuiltinIntentMetadata = BuiltinIntentMetadata'
  { signature :: Core.Maybe Types.Signature
    -- ^ A unique identifier for the built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
  , supportedLocales :: Core.Maybe [Types.Locale]
    -- ^ A list of identifiers for the locales that the intent supports.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BuiltinIntentMetadata' value with any optional fields omitted.
mkBuiltinIntentMetadata
    :: BuiltinIntentMetadata
mkBuiltinIntentMetadata
  = BuiltinIntentMetadata'{signature = Core.Nothing,
                           supportedLocales = Core.Nothing}

-- | A unique identifier for the built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bimSignature :: Lens.Lens' BuiltinIntentMetadata (Core.Maybe Types.Signature)
bimSignature = Lens.field @"signature"
{-# INLINEABLE bimSignature #-}
{-# DEPRECATED signature "Use generic-lens or generic-optics with 'signature' instead"  #-}

-- | A list of identifiers for the locales that the intent supports.
--
-- /Note:/ Consider using 'supportedLocales' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bimSupportedLocales :: Lens.Lens' BuiltinIntentMetadata (Core.Maybe [Types.Locale])
bimSupportedLocales = Lens.field @"supportedLocales"
{-# INLINEABLE bimSupportedLocales #-}
{-# DEPRECATED supportedLocales "Use generic-lens or generic-optics with 'supportedLocales' instead"  #-}

instance Core.FromJSON BuiltinIntentMetadata where
        parseJSON
          = Core.withObject "BuiltinIntentMetadata" Core.$
              \ x ->
                BuiltinIntentMetadata' Core.<$>
                  (x Core..:? "signature") Core.<*> x Core..:? "supportedLocales"
