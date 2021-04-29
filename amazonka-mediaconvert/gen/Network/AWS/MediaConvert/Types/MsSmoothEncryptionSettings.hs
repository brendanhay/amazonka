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
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Prelude

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to
-- specify the value SpekeKeyProvider.
--
-- /See:/ 'newMsSmoothEncryptionSettings' smart constructor.
data MsSmoothEncryptionSettings = MsSmoothEncryptionSettings'
  { -- | If your output group type is HLS, DASH, or Microsoft Smooth, use these
    -- settings when doing DRM encryption with a SPEKE-compliant key provider.
    -- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
    -- instead.
    spekeKeyProvider :: Prelude.Maybe SpekeKeyProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MsSmoothEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spekeKeyProvider', 'msSmoothEncryptionSettings_spekeKeyProvider' - If your output group type is HLS, DASH, or Microsoft Smooth, use these
-- settings when doing DRM encryption with a SPEKE-compliant key provider.
-- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
-- instead.
newMsSmoothEncryptionSettings ::
  MsSmoothEncryptionSettings
newMsSmoothEncryptionSettings =
  MsSmoothEncryptionSettings'
    { spekeKeyProvider =
        Prelude.Nothing
    }

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these
-- settings when doing DRM encryption with a SPEKE-compliant key provider.
-- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
-- instead.
msSmoothEncryptionSettings_spekeKeyProvider :: Lens.Lens' MsSmoothEncryptionSettings (Prelude.Maybe SpekeKeyProvider)
msSmoothEncryptionSettings_spekeKeyProvider = Lens.lens (\MsSmoothEncryptionSettings' {spekeKeyProvider} -> spekeKeyProvider) (\s@MsSmoothEncryptionSettings' {} a -> s {spekeKeyProvider = a} :: MsSmoothEncryptionSettings)

instance Prelude.FromJSON MsSmoothEncryptionSettings where
  parseJSON =
    Prelude.withObject
      "MsSmoothEncryptionSettings"
      ( \x ->
          MsSmoothEncryptionSettings'
            Prelude.<$> (x Prelude..:? "spekeKeyProvider")
      )

instance Prelude.Hashable MsSmoothEncryptionSettings

instance Prelude.NFData MsSmoothEncryptionSettings

instance Prelude.ToJSON MsSmoothEncryptionSettings where
  toJSON MsSmoothEncryptionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("spekeKeyProvider" Prelude..=)
              Prelude.<$> spekeKeyProvider
          ]
      )
