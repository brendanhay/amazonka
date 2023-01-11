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
-- Module      : Amazonka.MediaConvert.Types.MsSmoothEncryptionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MsSmoothEncryptionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.SpekeKeyProvider
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON MsSmoothEncryptionSettings where
  parseJSON =
    Data.withObject
      "MsSmoothEncryptionSettings"
      ( \x ->
          MsSmoothEncryptionSettings'
            Prelude.<$> (x Data..:? "spekeKeyProvider")
      )

instance Prelude.Hashable MsSmoothEncryptionSettings where
  hashWithSalt _salt MsSmoothEncryptionSettings' {..} =
    _salt `Prelude.hashWithSalt` spekeKeyProvider

instance Prelude.NFData MsSmoothEncryptionSettings where
  rnf MsSmoothEncryptionSettings' {..} =
    Prelude.rnf spekeKeyProvider

instance Data.ToJSON MsSmoothEncryptionSettings where
  toJSON MsSmoothEncryptionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("spekeKeyProvider" Data..=)
              Prelude.<$> spekeKeyProvider
          ]
      )
