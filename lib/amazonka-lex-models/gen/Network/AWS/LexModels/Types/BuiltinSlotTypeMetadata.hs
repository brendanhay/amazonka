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
import Network.AWS.LexModels.Types.Locale
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a built in slot type.
--
-- /See:/ 'mkBuiltinSlotTypeMetadata' smart constructor.
data BuiltinSlotTypeMetadata = BuiltinSlotTypeMetadata'
  { -- | A unique identifier for the built-in slot type. To find the signature for a slot type, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
    signature :: Lude.Maybe Lude.Text,
    -- | A list of target locales for the slot.
    supportedLocales :: Lude.Maybe [Locale]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuiltinSlotTypeMetadata' with the minimum fields required to make a request.
--
-- * 'signature' - A unique identifier for the built-in slot type. To find the signature for a slot type, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
-- * 'supportedLocales' - A list of target locales for the slot.
mkBuiltinSlotTypeMetadata ::
  BuiltinSlotTypeMetadata
mkBuiltinSlotTypeMetadata =
  BuiltinSlotTypeMetadata'
    { signature = Lude.Nothing,
      supportedLocales = Lude.Nothing
    }

-- | A unique identifier for the built-in slot type. To find the signature for a slot type, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bstmSignature :: Lens.Lens' BuiltinSlotTypeMetadata (Lude.Maybe Lude.Text)
bstmSignature = Lens.lens (signature :: BuiltinSlotTypeMetadata -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: BuiltinSlotTypeMetadata)
{-# DEPRECATED bstmSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | A list of target locales for the slot.
--
-- /Note:/ Consider using 'supportedLocales' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bstmSupportedLocales :: Lens.Lens' BuiltinSlotTypeMetadata (Lude.Maybe [Locale])
bstmSupportedLocales = Lens.lens (supportedLocales :: BuiltinSlotTypeMetadata -> Lude.Maybe [Locale]) (\s a -> s {supportedLocales = a} :: BuiltinSlotTypeMetadata)
{-# DEPRECATED bstmSupportedLocales "Use generic-lens or generic-optics with 'supportedLocales' instead." #-}

instance Lude.FromJSON BuiltinSlotTypeMetadata where
  parseJSON =
    Lude.withObject
      "BuiltinSlotTypeMetadata"
      ( \x ->
          BuiltinSlotTypeMetadata'
            Lude.<$> (x Lude..:? "signature")
            Lude.<*> (x Lude..:? "supportedLocales" Lude..!= Lude.mempty)
      )
