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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Use these settings to set up encryption with a static key provider.
--
-- /See:/ 'newStaticKeyProvider' smart constructor.
data StaticKeyProvider = StaticKeyProvider'
  { -- | Relates to DRM implementation. Sets the value of the KEYFORMAT
    -- attribute. Must be \'identity\' or a reverse DNS string. May be omitted
    -- to indicate an implicit value of \'identity\'.
    keyFormat :: Core.Maybe Core.Text,
    -- | Relates to DRM implementation. Use a 32-character hexidecimal string to
    -- specify Key Value (StaticKeyValue).
    staticKeyValue :: Core.Maybe Core.Text,
    -- | Relates to DRM implementation. The location of the license server used
    -- for protecting content.
    url :: Core.Maybe Core.Text,
    -- | Relates to DRM implementation. Either a single positive integer version
    -- value or a slash delimited list of version values (1\/2\/3).
    keyFormatVersions :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { keyFormat = Core.Nothing,
      staticKeyValue = Core.Nothing,
      url = Core.Nothing,
      keyFormatVersions = Core.Nothing
    }

-- | Relates to DRM implementation. Sets the value of the KEYFORMAT
-- attribute. Must be \'identity\' or a reverse DNS string. May be omitted
-- to indicate an implicit value of \'identity\'.
staticKeyProvider_keyFormat :: Lens.Lens' StaticKeyProvider (Core.Maybe Core.Text)
staticKeyProvider_keyFormat = Lens.lens (\StaticKeyProvider' {keyFormat} -> keyFormat) (\s@StaticKeyProvider' {} a -> s {keyFormat = a} :: StaticKeyProvider)

-- | Relates to DRM implementation. Use a 32-character hexidecimal string to
-- specify Key Value (StaticKeyValue).
staticKeyProvider_staticKeyValue :: Lens.Lens' StaticKeyProvider (Core.Maybe Core.Text)
staticKeyProvider_staticKeyValue = Lens.lens (\StaticKeyProvider' {staticKeyValue} -> staticKeyValue) (\s@StaticKeyProvider' {} a -> s {staticKeyValue = a} :: StaticKeyProvider)

-- | Relates to DRM implementation. The location of the license server used
-- for protecting content.
staticKeyProvider_url :: Lens.Lens' StaticKeyProvider (Core.Maybe Core.Text)
staticKeyProvider_url = Lens.lens (\StaticKeyProvider' {url} -> url) (\s@StaticKeyProvider' {} a -> s {url = a} :: StaticKeyProvider)

-- | Relates to DRM implementation. Either a single positive integer version
-- value or a slash delimited list of version values (1\/2\/3).
staticKeyProvider_keyFormatVersions :: Lens.Lens' StaticKeyProvider (Core.Maybe Core.Text)
staticKeyProvider_keyFormatVersions = Lens.lens (\StaticKeyProvider' {keyFormatVersions} -> keyFormatVersions) (\s@StaticKeyProvider' {} a -> s {keyFormatVersions = a} :: StaticKeyProvider)

instance Core.FromJSON StaticKeyProvider where
  parseJSON =
    Core.withObject
      "StaticKeyProvider"
      ( \x ->
          StaticKeyProvider'
            Core.<$> (x Core..:? "keyFormat")
            Core.<*> (x Core..:? "staticKeyValue")
            Core.<*> (x Core..:? "url")
            Core.<*> (x Core..:? "keyFormatVersions")
      )

instance Core.Hashable StaticKeyProvider

instance Core.NFData StaticKeyProvider

instance Core.ToJSON StaticKeyProvider where
  toJSON StaticKeyProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ ("keyFormat" Core..=) Core.<$> keyFormat,
            ("staticKeyValue" Core..=) Core.<$> staticKeyValue,
            ("url" Core..=) Core.<$> url,
            ("keyFormatVersions" Core..=)
              Core.<$> keyFormatVersions
          ]
      )
