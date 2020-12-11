-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Ssml
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Ssml
  ( Ssml (..),

    -- * Smart constructor
    mkSsml,

    -- * Lenses
    ssmLocale,
    ssmValue,
  )
where

import Network.AWS.AlexaBusiness.Types.Locale
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The SSML message. For more information, see <https://developer.amazon.com/docs/custom-skills/speech-synthesis-markup-language-ssml-reference.html SSML Reference> .
--
-- /See:/ 'mkSsml' smart constructor.
data Ssml = Ssml' {locale :: Locale, value :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Ssml' with the minimum fields required to make a request.
--
-- * 'locale' - The locale of the SSML message. Currently, en-US is supported.
-- * 'value' - The value of the SSML message in the correct SSML format. The audio tag is not supported.
mkSsml ::
  -- | 'locale'
  Locale ->
  -- | 'value'
  Lude.Text ->
  Ssml
mkSsml pLocale_ pValue_ = Ssml' {locale = pLocale_, value = pValue_}

-- | The locale of the SSML message. Currently, en-US is supported.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmLocale :: Lens.Lens' Ssml Locale
ssmLocale = Lens.lens (locale :: Ssml -> Locale) (\s a -> s {locale = a} :: Ssml)
{-# DEPRECATED ssmLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The value of the SSML message in the correct SSML format. The audio tag is not supported.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmValue :: Lens.Lens' Ssml Lude.Text
ssmValue = Lens.lens (value :: Ssml -> Lude.Text) (\s a -> s {value = a} :: Ssml)
{-# DEPRECATED ssmValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToJSON Ssml where
  toJSON Ssml' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Locale" Lude..= locale),
            Lude.Just ("Value" Lude..= value)
          ]
      )
