{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.TextMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.TextMessage
  ( TextMessage (..),

    -- * Smart constructor
    mkTextMessage,

    -- * Lenses
    tmLocale,
    tmValue,
  )
where

import Network.AWS.AlexaBusiness.Types.Locale
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The text message.
--
-- /See:/ 'mkTextMessage' smart constructor.
data TextMessage = TextMessage'
  { -- | The locale of the text message. Currently, en-US is supported.
    locale :: Locale,
    -- | The value of the text message.
    value :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TextMessage' with the minimum fields required to make a request.
--
-- * 'locale' - The locale of the text message. Currently, en-US is supported.
-- * 'value' - The value of the text message.
mkTextMessage ::
  -- | 'locale'
  Locale ->
  -- | 'value'
  Lude.Text ->
  TextMessage
mkTextMessage pLocale_ pValue_ =
  TextMessage' {locale = pLocale_, value = pValue_}

-- | The locale of the text message. Currently, en-US is supported.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmLocale :: Lens.Lens' TextMessage Locale
tmLocale = Lens.lens (locale :: TextMessage -> Locale) (\s a -> s {locale = a} :: TextMessage)
{-# DEPRECATED tmLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The value of the text message.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmValue :: Lens.Lens' TextMessage Lude.Text
tmValue = Lens.lens (value :: TextMessage -> Lude.Text) (\s a -> s {value = a} :: TextMessage)
{-# DEPRECATED tmValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToJSON TextMessage where
  toJSON TextMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Locale" Lude..= locale),
            Lude.Just ("Value" Lude..= value)
          ]
      )
