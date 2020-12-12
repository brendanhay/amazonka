{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BuiltinIntentMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BuiltinIntentMetadata
  ( BuiltinIntentMetadata (..),

    -- * Smart constructor
    mkBuiltinIntentMetadata,

    -- * Lenses
    bimSignature,
    bimSupportedLocales,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Locale
import qualified Network.AWS.Prelude as Lude

-- | Provides metadata for a built-in intent.
--
-- /See:/ 'mkBuiltinIntentMetadata' smart constructor.
data BuiltinIntentMetadata = BuiltinIntentMetadata'
  { signature ::
      Lude.Maybe Lude.Text,
    supportedLocales :: Lude.Maybe [Locale]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuiltinIntentMetadata' with the minimum fields required to make a request.
--
-- * 'signature' - A unique identifier for the built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
-- * 'supportedLocales' - A list of identifiers for the locales that the intent supports.
mkBuiltinIntentMetadata ::
  BuiltinIntentMetadata
mkBuiltinIntentMetadata =
  BuiltinIntentMetadata'
    { signature = Lude.Nothing,
      supportedLocales = Lude.Nothing
    }

-- | A unique identifier for the built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bimSignature :: Lens.Lens' BuiltinIntentMetadata (Lude.Maybe Lude.Text)
bimSignature = Lens.lens (signature :: BuiltinIntentMetadata -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: BuiltinIntentMetadata)
{-# DEPRECATED bimSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | A list of identifiers for the locales that the intent supports.
--
-- /Note:/ Consider using 'supportedLocales' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bimSupportedLocales :: Lens.Lens' BuiltinIntentMetadata (Lude.Maybe [Locale])
bimSupportedLocales = Lens.lens (supportedLocales :: BuiltinIntentMetadata -> Lude.Maybe [Locale]) (\s a -> s {supportedLocales = a} :: BuiltinIntentMetadata)
{-# DEPRECATED bimSupportedLocales "Use generic-lens or generic-optics with 'supportedLocales' instead." #-}

instance Lude.FromJSON BuiltinIntentMetadata where
  parseJSON =
    Lude.withObject
      "BuiltinIntentMetadata"
      ( \x ->
          BuiltinIntentMetadata'
            Lude.<$> (x Lude..:? "signature")
            Lude.<*> (x Lude..:? "supportedLocales" Lude..!= Lude.mempty)
      )
