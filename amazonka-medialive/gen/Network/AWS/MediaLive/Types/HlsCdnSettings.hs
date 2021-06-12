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
-- Module      : Network.AWS.MediaLive.Types.HlsCdnSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsCdnSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsAkamaiSettings
import Network.AWS.MediaLive.Types.HlsBasicPutSettings
import Network.AWS.MediaLive.Types.HlsMediaStoreSettings
import Network.AWS.MediaLive.Types.HlsWebdavSettings

-- | Hls Cdn Settings
--
-- /See:/ 'newHlsCdnSettings' smart constructor.
data HlsCdnSettings = HlsCdnSettings'
  { hlsBasicPutSettings :: Core.Maybe HlsBasicPutSettings,
    hlsWebdavSettings :: Core.Maybe HlsWebdavSettings,
    hlsAkamaiSettings :: Core.Maybe HlsAkamaiSettings,
    hlsMediaStoreSettings :: Core.Maybe HlsMediaStoreSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HlsCdnSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hlsBasicPutSettings', 'hlsCdnSettings_hlsBasicPutSettings' - Undocumented member.
--
-- 'hlsWebdavSettings', 'hlsCdnSettings_hlsWebdavSettings' - Undocumented member.
--
-- 'hlsAkamaiSettings', 'hlsCdnSettings_hlsAkamaiSettings' - Undocumented member.
--
-- 'hlsMediaStoreSettings', 'hlsCdnSettings_hlsMediaStoreSettings' - Undocumented member.
newHlsCdnSettings ::
  HlsCdnSettings
newHlsCdnSettings =
  HlsCdnSettings'
    { hlsBasicPutSettings = Core.Nothing,
      hlsWebdavSettings = Core.Nothing,
      hlsAkamaiSettings = Core.Nothing,
      hlsMediaStoreSettings = Core.Nothing
    }

-- | Undocumented member.
hlsCdnSettings_hlsBasicPutSettings :: Lens.Lens' HlsCdnSettings (Core.Maybe HlsBasicPutSettings)
hlsCdnSettings_hlsBasicPutSettings = Lens.lens (\HlsCdnSettings' {hlsBasicPutSettings} -> hlsBasicPutSettings) (\s@HlsCdnSettings' {} a -> s {hlsBasicPutSettings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsWebdavSettings :: Lens.Lens' HlsCdnSettings (Core.Maybe HlsWebdavSettings)
hlsCdnSettings_hlsWebdavSettings = Lens.lens (\HlsCdnSettings' {hlsWebdavSettings} -> hlsWebdavSettings) (\s@HlsCdnSettings' {} a -> s {hlsWebdavSettings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsAkamaiSettings :: Lens.Lens' HlsCdnSettings (Core.Maybe HlsAkamaiSettings)
hlsCdnSettings_hlsAkamaiSettings = Lens.lens (\HlsCdnSettings' {hlsAkamaiSettings} -> hlsAkamaiSettings) (\s@HlsCdnSettings' {} a -> s {hlsAkamaiSettings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsMediaStoreSettings :: Lens.Lens' HlsCdnSettings (Core.Maybe HlsMediaStoreSettings)
hlsCdnSettings_hlsMediaStoreSettings = Lens.lens (\HlsCdnSettings' {hlsMediaStoreSettings} -> hlsMediaStoreSettings) (\s@HlsCdnSettings' {} a -> s {hlsMediaStoreSettings = a} :: HlsCdnSettings)

instance Core.FromJSON HlsCdnSettings where
  parseJSON =
    Core.withObject
      "HlsCdnSettings"
      ( \x ->
          HlsCdnSettings'
            Core.<$> (x Core..:? "hlsBasicPutSettings")
            Core.<*> (x Core..:? "hlsWebdavSettings")
            Core.<*> (x Core..:? "hlsAkamaiSettings")
            Core.<*> (x Core..:? "hlsMediaStoreSettings")
      )

instance Core.Hashable HlsCdnSettings

instance Core.NFData HlsCdnSettings

instance Core.ToJSON HlsCdnSettings where
  toJSON HlsCdnSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("hlsBasicPutSettings" Core..=)
              Core.<$> hlsBasicPutSettings,
            ("hlsWebdavSettings" Core..=)
              Core.<$> hlsWebdavSettings,
            ("hlsAkamaiSettings" Core..=)
              Core.<$> hlsAkamaiSettings,
            ("hlsMediaStoreSettings" Core..=)
              Core.<$> hlsMediaStoreSettings
          ]
      )
