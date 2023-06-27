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
-- Module      : Amazonka.MediaLive.Types.HlsCdnSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsCdnSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.HlsAkamaiSettings
import Amazonka.MediaLive.Types.HlsBasicPutSettings
import Amazonka.MediaLive.Types.HlsMediaStoreSettings
import Amazonka.MediaLive.Types.HlsS3Settings
import Amazonka.MediaLive.Types.HlsWebdavSettings
import qualified Amazonka.Prelude as Prelude

-- | Hls Cdn Settings
--
-- /See:/ 'newHlsCdnSettings' smart constructor.
data HlsCdnSettings = HlsCdnSettings'
  { hlsAkamaiSettings :: Prelude.Maybe HlsAkamaiSettings,
    hlsBasicPutSettings :: Prelude.Maybe HlsBasicPutSettings,
    hlsMediaStoreSettings :: Prelude.Maybe HlsMediaStoreSettings,
    hlsS3Settings :: Prelude.Maybe HlsS3Settings,
    hlsWebdavSettings :: Prelude.Maybe HlsWebdavSettings
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
-- 'hlsAkamaiSettings', 'hlsCdnSettings_hlsAkamaiSettings' - Undocumented member.
--
-- 'hlsBasicPutSettings', 'hlsCdnSettings_hlsBasicPutSettings' - Undocumented member.
--
-- 'hlsMediaStoreSettings', 'hlsCdnSettings_hlsMediaStoreSettings' - Undocumented member.
--
-- 'hlsS3Settings', 'hlsCdnSettings_hlsS3Settings' - Undocumented member.
--
-- 'hlsWebdavSettings', 'hlsCdnSettings_hlsWebdavSettings' - Undocumented member.
newHlsCdnSettings ::
  HlsCdnSettings
newHlsCdnSettings =
  HlsCdnSettings'
    { hlsAkamaiSettings =
        Prelude.Nothing,
      hlsBasicPutSettings = Prelude.Nothing,
      hlsMediaStoreSettings = Prelude.Nothing,
      hlsS3Settings = Prelude.Nothing,
      hlsWebdavSettings = Prelude.Nothing
    }

-- | Undocumented member.
hlsCdnSettings_hlsAkamaiSettings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsAkamaiSettings)
hlsCdnSettings_hlsAkamaiSettings = Lens.lens (\HlsCdnSettings' {hlsAkamaiSettings} -> hlsAkamaiSettings) (\s@HlsCdnSettings' {} a -> s {hlsAkamaiSettings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsBasicPutSettings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsBasicPutSettings)
hlsCdnSettings_hlsBasicPutSettings = Lens.lens (\HlsCdnSettings' {hlsBasicPutSettings} -> hlsBasicPutSettings) (\s@HlsCdnSettings' {} a -> s {hlsBasicPutSettings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsMediaStoreSettings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsMediaStoreSettings)
hlsCdnSettings_hlsMediaStoreSettings = Lens.lens (\HlsCdnSettings' {hlsMediaStoreSettings} -> hlsMediaStoreSettings) (\s@HlsCdnSettings' {} a -> s {hlsMediaStoreSettings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsS3Settings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsS3Settings)
hlsCdnSettings_hlsS3Settings = Lens.lens (\HlsCdnSettings' {hlsS3Settings} -> hlsS3Settings) (\s@HlsCdnSettings' {} a -> s {hlsS3Settings = a} :: HlsCdnSettings)

-- | Undocumented member.
hlsCdnSettings_hlsWebdavSettings :: Lens.Lens' HlsCdnSettings (Prelude.Maybe HlsWebdavSettings)
hlsCdnSettings_hlsWebdavSettings = Lens.lens (\HlsCdnSettings' {hlsWebdavSettings} -> hlsWebdavSettings) (\s@HlsCdnSettings' {} a -> s {hlsWebdavSettings = a} :: HlsCdnSettings)

instance Data.FromJSON HlsCdnSettings where
  parseJSON =
    Data.withObject
      "HlsCdnSettings"
      ( \x ->
          HlsCdnSettings'
            Prelude.<$> (x Data..:? "hlsAkamaiSettings")
            Prelude.<*> (x Data..:? "hlsBasicPutSettings")
            Prelude.<*> (x Data..:? "hlsMediaStoreSettings")
            Prelude.<*> (x Data..:? "hlsS3Settings")
            Prelude.<*> (x Data..:? "hlsWebdavSettings")
      )

instance Prelude.Hashable HlsCdnSettings where
  hashWithSalt _salt HlsCdnSettings' {..} =
    _salt
      `Prelude.hashWithSalt` hlsAkamaiSettings
      `Prelude.hashWithSalt` hlsBasicPutSettings
      `Prelude.hashWithSalt` hlsMediaStoreSettings
      `Prelude.hashWithSalt` hlsS3Settings
      `Prelude.hashWithSalt` hlsWebdavSettings

instance Prelude.NFData HlsCdnSettings where
  rnf HlsCdnSettings' {..} =
    Prelude.rnf hlsAkamaiSettings
      `Prelude.seq` Prelude.rnf hlsBasicPutSettings
      `Prelude.seq` Prelude.rnf hlsMediaStoreSettings
      `Prelude.seq` Prelude.rnf hlsS3Settings
      `Prelude.seq` Prelude.rnf hlsWebdavSettings

instance Data.ToJSON HlsCdnSettings where
  toJSON HlsCdnSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("hlsAkamaiSettings" Data..=)
              Prelude.<$> hlsAkamaiSettings,
            ("hlsBasicPutSettings" Data..=)
              Prelude.<$> hlsBasicPutSettings,
            ("hlsMediaStoreSettings" Data..=)
              Prelude.<$> hlsMediaStoreSettings,
            ("hlsS3Settings" Data..=) Prelude.<$> hlsS3Settings,
            ("hlsWebdavSettings" Data..=)
              Prelude.<$> hlsWebdavSettings
          ]
      )
