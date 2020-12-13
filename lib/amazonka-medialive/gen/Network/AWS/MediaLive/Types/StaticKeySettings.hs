{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    sksStaticKeyValue,
    sksKeyProviderServer,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Lude

-- | Static Key Settings
--
-- /See:/ 'mkStaticKeySettings' smart constructor.
data StaticKeySettings = StaticKeySettings'
  { -- | Static key value as a 32 character hexadecimal string.
    staticKeyValue :: Lude.Text,
    -- | The URL of the license server used for protecting content.
    keyProviderServer :: Lude.Maybe InputLocation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StaticKeySettings' with the minimum fields required to make a request.
--
-- * 'staticKeyValue' - Static key value as a 32 character hexadecimal string.
-- * 'keyProviderServer' - The URL of the license server used for protecting content.
mkStaticKeySettings ::
  -- | 'staticKeyValue'
  Lude.Text ->
  StaticKeySettings
mkStaticKeySettings pStaticKeyValue_ =
  StaticKeySettings'
    { staticKeyValue = pStaticKeyValue_,
      keyProviderServer = Lude.Nothing
    }

-- | Static key value as a 32 character hexadecimal string.
--
-- /Note:/ Consider using 'staticKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sksStaticKeyValue :: Lens.Lens' StaticKeySettings Lude.Text
sksStaticKeyValue = Lens.lens (staticKeyValue :: StaticKeySettings -> Lude.Text) (\s a -> s {staticKeyValue = a} :: StaticKeySettings)
{-# DEPRECATED sksStaticKeyValue "Use generic-lens or generic-optics with 'staticKeyValue' instead." #-}

-- | The URL of the license server used for protecting content.
--
-- /Note:/ Consider using 'keyProviderServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sksKeyProviderServer :: Lens.Lens' StaticKeySettings (Lude.Maybe InputLocation)
sksKeyProviderServer = Lens.lens (keyProviderServer :: StaticKeySettings -> Lude.Maybe InputLocation) (\s a -> s {keyProviderServer = a} :: StaticKeySettings)
{-# DEPRECATED sksKeyProviderServer "Use generic-lens or generic-optics with 'keyProviderServer' instead." #-}

instance Lude.FromJSON StaticKeySettings where
  parseJSON =
    Lude.withObject
      "StaticKeySettings"
      ( \x ->
          StaticKeySettings'
            Lude.<$> (x Lude..: "staticKeyValue")
            Lude.<*> (x Lude..:? "keyProviderServer")
      )

instance Lude.ToJSON StaticKeySettings where
  toJSON StaticKeySettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("staticKeyValue" Lude..= staticKeyValue),
            ("keyProviderServer" Lude..=) Lude.<$> keyProviderServer
          ]
      )
