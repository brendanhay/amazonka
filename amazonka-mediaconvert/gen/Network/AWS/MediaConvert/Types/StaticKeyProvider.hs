{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.StaticKeyProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.StaticKeyProvider where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Use these settings to set up encryption with a static key provider.
--
-- /See:/ 'newStaticKeyProvider' smart constructor.
data StaticKeyProvider = StaticKeyProvider'
  { -- | Relates to DRM implementation. Sets the value of the KEYFORMAT
    -- attribute. Must be \'identity\' or a reverse DNS string. May be omitted
    -- to indicate an implicit value of \'identity\'.
    keyFormat :: Prelude.Maybe Prelude.Text,
    -- | Relates to DRM implementation. Use a 32-character hexidecimal string to
    -- specify Key Value (StaticKeyValue).
    staticKeyValue :: Prelude.Maybe Prelude.Text,
    -- | Relates to DRM implementation. The location of the license server used
    -- for protecting content.
    url :: Prelude.Maybe Prelude.Text,
    -- | Relates to DRM implementation. Either a single positive integer version
    -- value or a slash delimited list of version values (1\/2\/3).
    keyFormatVersions :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StaticKeyProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyFormat', 'staticKeyProvider_keyFormat' - Relates to DRM implementation. Sets the value of the KEYFORMAT
-- attribute. Must be \'identity\' or a reverse DNS string. May be omitted
-- to indicate an implicit value of \'identity\'.
--
-- 'staticKeyValue', 'staticKeyProvider_staticKeyValue' - Relates to DRM implementation. Use a 32-character hexidecimal string to
-- specify Key Value (StaticKeyValue).
--
-- 'url', 'staticKeyProvider_url' - Relates to DRM implementation. The location of the license server used
-- for protecting content.
--
-- 'keyFormatVersions', 'staticKeyProvider_keyFormatVersions' - Relates to DRM implementation. Either a single positive integer version
-- value or a slash delimited list of version values (1\/2\/3).
newStaticKeyProvider ::
  StaticKeyProvider
newStaticKeyProvider =
  StaticKeyProvider'
    { keyFormat = Prelude.Nothing,
      staticKeyValue = Prelude.Nothing,
      url = Prelude.Nothing,
      keyFormatVersions = Prelude.Nothing
    }

-- | Relates to DRM implementation. Sets the value of the KEYFORMAT
-- attribute. Must be \'identity\' or a reverse DNS string. May be omitted
-- to indicate an implicit value of \'identity\'.
staticKeyProvider_keyFormat :: Lens.Lens' StaticKeyProvider (Prelude.Maybe Prelude.Text)
staticKeyProvider_keyFormat = Lens.lens (\StaticKeyProvider' {keyFormat} -> keyFormat) (\s@StaticKeyProvider' {} a -> s {keyFormat = a} :: StaticKeyProvider)

-- | Relates to DRM implementation. Use a 32-character hexidecimal string to
-- specify Key Value (StaticKeyValue).
staticKeyProvider_staticKeyValue :: Lens.Lens' StaticKeyProvider (Prelude.Maybe Prelude.Text)
staticKeyProvider_staticKeyValue = Lens.lens (\StaticKeyProvider' {staticKeyValue} -> staticKeyValue) (\s@StaticKeyProvider' {} a -> s {staticKeyValue = a} :: StaticKeyProvider)

-- | Relates to DRM implementation. The location of the license server used
-- for protecting content.
staticKeyProvider_url :: Lens.Lens' StaticKeyProvider (Prelude.Maybe Prelude.Text)
staticKeyProvider_url = Lens.lens (\StaticKeyProvider' {url} -> url) (\s@StaticKeyProvider' {} a -> s {url = a} :: StaticKeyProvider)

-- | Relates to DRM implementation. Either a single positive integer version
-- value or a slash delimited list of version values (1\/2\/3).
staticKeyProvider_keyFormatVersions :: Lens.Lens' StaticKeyProvider (Prelude.Maybe Prelude.Text)
staticKeyProvider_keyFormatVersions = Lens.lens (\StaticKeyProvider' {keyFormatVersions} -> keyFormatVersions) (\s@StaticKeyProvider' {} a -> s {keyFormatVersions = a} :: StaticKeyProvider)

instance Prelude.FromJSON StaticKeyProvider where
  parseJSON =
    Prelude.withObject
      "StaticKeyProvider"
      ( \x ->
          StaticKeyProvider'
            Prelude.<$> (x Prelude..:? "keyFormat")
            Prelude.<*> (x Prelude..:? "staticKeyValue")
            Prelude.<*> (x Prelude..:? "url")
            Prelude.<*> (x Prelude..:? "keyFormatVersions")
      )

instance Prelude.Hashable StaticKeyProvider

instance Prelude.NFData StaticKeyProvider

instance Prelude.ToJSON StaticKeyProvider where
  toJSON StaticKeyProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("keyFormat" Prelude..=) Prelude.<$> keyFormat,
            ("staticKeyValue" Prelude..=)
              Prelude.<$> staticKeyValue,
            ("url" Prelude..=) Prelude.<$> url,
            ("keyFormatVersions" Prelude..=)
              Prelude.<$> keyFormatVersions
          ]
      )
