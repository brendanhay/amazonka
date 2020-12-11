-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.KeyProviderSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.KeyProviderSettings
  ( KeyProviderSettings (..),

    -- * Smart constructor
    mkKeyProviderSettings,

    -- * Lenses
    kpsStaticKeySettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.StaticKeySettings
import qualified Network.AWS.Prelude as Lude

-- | Key Provider Settings
--
-- /See:/ 'mkKeyProviderSettings' smart constructor.
newtype KeyProviderSettings = KeyProviderSettings'
  { staticKeySettings ::
      Lude.Maybe StaticKeySettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyProviderSettings' with the minimum fields required to make a request.
--
-- * 'staticKeySettings' - Undocumented field.
mkKeyProviderSettings ::
  KeyProviderSettings
mkKeyProviderSettings =
  KeyProviderSettings' {staticKeySettings = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'staticKeySettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpsStaticKeySettings :: Lens.Lens' KeyProviderSettings (Lude.Maybe StaticKeySettings)
kpsStaticKeySettings = Lens.lens (staticKeySettings :: KeyProviderSettings -> Lude.Maybe StaticKeySettings) (\s a -> s {staticKeySettings = a} :: KeyProviderSettings)
{-# DEPRECATED kpsStaticKeySettings "Use generic-lens or generic-optics with 'staticKeySettings' instead." #-}

instance Lude.FromJSON KeyProviderSettings where
  parseJSON =
    Lude.withObject
      "KeyProviderSettings"
      ( \x ->
          KeyProviderSettings' Lude.<$> (x Lude..:? "staticKeySettings")
      )

instance Lude.ToJSON KeyProviderSettings where
  toJSON KeyProviderSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [("staticKeySettings" Lude..=) Lude.<$> staticKeySettings]
      )
