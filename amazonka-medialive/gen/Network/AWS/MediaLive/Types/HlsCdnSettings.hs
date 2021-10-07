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
import Network.AWS.MediaLive.Types.HlsS3Settings
import Network.AWS.MediaLive.Types.HlsWebdavSettings
import qualified Network.AWS.Prelude as Prelude

-- | Hls Cdn Settings
--
-- /See:/ 'newHlsCdnSettings' smart constructor.
data HlsCdnSettings = HlsCdnSettings'
  { hlsWebdavSettings :: Prelude.Maybe HlsWebdavSettings,
    hlsBasicPutSettings :: Prelude.Maybe HlsBasicPutSettings,
    hlsS3Settings :: Prelude.Maybe HlsS3Settings,
    hlsAkamaiSettings :: Prelude.Maybe HlsAkamaiSettings,
    hlsMediaStoreSettings :: Prelude.Maybe HlsMediaStoreSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsCdnSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hlsWebdavSettings', 'hlsCdnSettings_hlsWebdavSettings' - Undocumented member.
--
-- 'hlsBasicPutSettings', 'hlsCdnSettings_hlsBasicPutSettings' - Undocumented member.
--
-- 'hlsS3Settings', 'hlsCdnSettings_hlsS3Settings' - Undocumented member.
--
-- 'hlsAkamaiSettings', 'hlsCdnSettings_hlsAkamaiSettings' - Undocumented member.
--
-- 'hlsMediaStoreSettings', 'hlsCdnSettings_hlsMediaStoreSettings' - Undocumented member.
newHlsCdnSettings ::
  HlsCdnSettings
newHlsCdnSettings =
  HlsCdnSettings'
    { hlsWebdavSettings =
        Prelude.Nothing,
      hlsBasicPutSettings = Prelude.Nothing,
      hlsS3Settings = Prelude.Nothing,
      hlsAkamaiSettings = Prelude.Nothing,
      hlsMediaStoreSettings = Prelude.Nothing
    }

-- | Undocumented member.
hlsCdnSettings_hlsWebdavSettings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsWebdavSettings)
hlsCdnSettings_hlsWebdavSettings = Lens.lens (\HlsCdnSettings' {hlsWebdavSettings} -> hlsWebdavSettings) (\s@HlsCdnSettings' {} a -> s {hlsWebdavSettings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsBasicPutSettings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsBasicPutSettings)
hlsCdnSettings_hlsBasicPutSettings = Lens.lens (\HlsCdnSettings' {hlsBasicPutSettings} -> hlsBasicPutSettings) (\s@HlsCdnSettings' {} a -> s {hlsBasicPutSettings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsS3Settings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsS3Settings)
hlsCdnSettings_hlsS3Settings = Lens.lens (\HlsCdnSettings' {hlsS3Settings} -> hlsS3Settings) (\s@HlsCdnSettings' {} a -> s {hlsS3Settings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsAkamaiSettings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsAkamaiSettings)
hlsCdnSettings_hlsAkamaiSettings = Lens.lens (\HlsCdnSettings' {hlsAkamaiSettings} -> hlsAkamaiSettings) (\s@HlsCdnSettings' {} a -> s {hlsAkamaiSettings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsMediaStoreSettings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsMediaStoreSettings)
hlsCdnSettings_hlsMediaStoreSettings = Lens.lens (\HlsCdnSettings' {hlsMediaStoreSettings} -> hlsMediaStoreSettings) (\s@HlsCdnSettings' {} a -> s {hlsMediaStoreSettings = a} :: HlsCdnSettings)

instance Core.FromJSON HlsCdnSettings where
  parseJSON =
    Core.withObject
      "HlsCdnSettings"
      ( \x ->
          HlsCdnSettings'
            Prelude.<$> (x Core..:? "hlsWebdavSettings")
            Prelude.<*> (x Core..:? "hlsBasicPutSettings")
            Prelude.<*> (x Core..:? "hlsS3Settings")
            Prelude.<*> (x Core..:? "hlsAkamaiSettings")
            Prelude.<*> (x Core..:? "hlsMediaStoreSettings")
      )

instance Prelude.Hashable HlsCdnSettings

instance Prelude.NFData HlsCdnSettings

instance Core.ToJSON HlsCdnSettings where
  toJSON HlsCdnSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hlsWebdavSettings" Core..=)
              Prelude.<$> hlsWebdavSettings,
            ("hlsBasicPutSettings" Core..=)
              Prelude.<$> hlsBasicPutSettings,
            ("hlsS3Settings" Core..=) Prelude.<$> hlsS3Settings,
            ("hlsAkamaiSettings" Core..=)
              Prelude.<$> hlsAkamaiSettings,
            ("hlsMediaStoreSettings" Core..=)
              Prelude.<$> hlsMediaStoreSettings
          ]
      )
