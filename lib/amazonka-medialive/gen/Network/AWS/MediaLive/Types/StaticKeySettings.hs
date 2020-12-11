-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticKeySettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticKeySettings
  ( StaticKeySettings (..),

    -- * Smart constructor
    mkStaticKeySettings,

    -- * Lenses
    sksKeyProviderServer,
    sksStaticKeyValue,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Lude

-- | Static Key Settings
--
-- /See:/ 'mkStaticKeySettings' smart constructor.
data StaticKeySettings = StaticKeySettings'
  { keyProviderServer ::
      Lude.Maybe InputLocation,
    staticKeyValue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StaticKeySettings' with the minimum fields required to make a request.
--
-- * 'keyProviderServer' - The URL of the license server used for protecting content.
-- * 'staticKeyValue' - Static key value as a 32 character hexadecimal string.
mkStaticKeySettings ::
  -- | 'staticKeyValue'
  Lude.Text ->
  StaticKeySettings
mkStaticKeySettings pStaticKeyValue_ =
  StaticKeySettings'
    { keyProviderServer = Lude.Nothing,
      staticKeyValue = pStaticKeyValue_
    }

-- | The URL of the license server used for protecting content.
--
-- /Note:/ Consider using 'keyProviderServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sksKeyProviderServer :: Lens.Lens' StaticKeySettings (Lude.Maybe InputLocation)
sksKeyProviderServer = Lens.lens (keyProviderServer :: StaticKeySettings -> Lude.Maybe InputLocation) (\s a -> s {keyProviderServer = a} :: StaticKeySettings)
{-# DEPRECATED sksKeyProviderServer "Use generic-lens or generic-optics with 'keyProviderServer' instead." #-}

-- | Static key value as a 32 character hexadecimal string.
--
-- /Note:/ Consider using 'staticKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sksStaticKeyValue :: Lens.Lens' StaticKeySettings Lude.Text
sksStaticKeyValue = Lens.lens (staticKeyValue :: StaticKeySettings -> Lude.Text) (\s a -> s {staticKeyValue = a} :: StaticKeySettings)
{-# DEPRECATED sksStaticKeyValue "Use generic-lens or generic-optics with 'staticKeyValue' instead." #-}

instance Lude.FromJSON StaticKeySettings where
  parseJSON =
    Lude.withObject
      "StaticKeySettings"
      ( \x ->
          StaticKeySettings'
            Lude.<$> (x Lude..:? "keyProviderServer")
            Lude.<*> (x Lude..: "staticKeyValue")
      )

instance Lude.ToJSON StaticKeySettings where
  toJSON StaticKeySettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("keyProviderServer" Lude..=) Lude.<$> keyProviderServer,
            Lude.Just ("staticKeyValue" Lude..= staticKeyValue)
          ]
      )
