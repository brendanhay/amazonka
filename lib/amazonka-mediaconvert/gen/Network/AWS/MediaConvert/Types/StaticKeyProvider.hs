{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.StaticKeyProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.StaticKeyProvider
  ( StaticKeyProvider (..),

    -- * Smart constructor
    mkStaticKeyProvider,

    -- * Lenses
    skpStaticKeyValue,
    skpURL,
    skpKeyFormat,
    skpKeyFormatVersions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use these settings to set up encryption with a static key provider.
--
-- /See:/ 'mkStaticKeyProvider' smart constructor.
data StaticKeyProvider = StaticKeyProvider'
  { -- | Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
    staticKeyValue :: Lude.Maybe Lude.Text,
    -- | Relates to DRM implementation. The location of the license server used for protecting content.
    url :: Lude.Maybe Lude.Text,
    -- | Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
    keyFormat :: Lude.Maybe Lude.Text,
    -- | Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
    keyFormatVersions :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StaticKeyProvider' with the minimum fields required to make a request.
--
-- * 'staticKeyValue' - Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
-- * 'url' - Relates to DRM implementation. The location of the license server used for protecting content.
-- * 'keyFormat' - Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
-- * 'keyFormatVersions' - Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
mkStaticKeyProvider ::
  StaticKeyProvider
mkStaticKeyProvider =
  StaticKeyProvider'
    { staticKeyValue = Lude.Nothing,
      url = Lude.Nothing,
      keyFormat = Lude.Nothing,
      keyFormatVersions = Lude.Nothing
    }

-- | Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
--
-- /Note:/ Consider using 'staticKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpStaticKeyValue :: Lens.Lens' StaticKeyProvider (Lude.Maybe Lude.Text)
skpStaticKeyValue = Lens.lens (staticKeyValue :: StaticKeyProvider -> Lude.Maybe Lude.Text) (\s a -> s {staticKeyValue = a} :: StaticKeyProvider)
{-# DEPRECATED skpStaticKeyValue "Use generic-lens or generic-optics with 'staticKeyValue' instead." #-}

-- | Relates to DRM implementation. The location of the license server used for protecting content.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpURL :: Lens.Lens' StaticKeyProvider (Lude.Maybe Lude.Text)
skpURL = Lens.lens (url :: StaticKeyProvider -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: StaticKeyProvider)
{-# DEPRECATED skpURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
--
-- /Note:/ Consider using 'keyFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpKeyFormat :: Lens.Lens' StaticKeyProvider (Lude.Maybe Lude.Text)
skpKeyFormat = Lens.lens (keyFormat :: StaticKeyProvider -> Lude.Maybe Lude.Text) (\s a -> s {keyFormat = a} :: StaticKeyProvider)
{-# DEPRECATED skpKeyFormat "Use generic-lens or generic-optics with 'keyFormat' instead." #-}

-- | Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
--
-- /Note:/ Consider using 'keyFormatVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpKeyFormatVersions :: Lens.Lens' StaticKeyProvider (Lude.Maybe Lude.Text)
skpKeyFormatVersions = Lens.lens (keyFormatVersions :: StaticKeyProvider -> Lude.Maybe Lude.Text) (\s a -> s {keyFormatVersions = a} :: StaticKeyProvider)
{-# DEPRECATED skpKeyFormatVersions "Use generic-lens or generic-optics with 'keyFormatVersions' instead." #-}

instance Lude.FromJSON StaticKeyProvider where
  parseJSON =
    Lude.withObject
      "StaticKeyProvider"
      ( \x ->
          StaticKeyProvider'
            Lude.<$> (x Lude..:? "staticKeyValue")
            Lude.<*> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "keyFormat")
            Lude.<*> (x Lude..:? "keyFormatVersions")
      )

instance Lude.ToJSON StaticKeyProvider where
  toJSON StaticKeyProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("staticKeyValue" Lude..=) Lude.<$> staticKeyValue,
            ("url" Lude..=) Lude.<$> url,
            ("keyFormat" Lude..=) Lude.<$> keyFormat,
            ("keyFormatVersions" Lude..=) Lude.<$> keyFormatVersions
          ]
      )
