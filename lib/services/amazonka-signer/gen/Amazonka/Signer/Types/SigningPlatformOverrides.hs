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
-- Module      : Amazonka.Signer.Types.SigningPlatformOverrides
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningPlatformOverrides where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.ImageFormat
import Amazonka.Signer.Types.SigningConfigurationOverrides

-- | Any overrides that are applied to the signing configuration of a code
-- signing platform.
--
-- /See:/ 'newSigningPlatformOverrides' smart constructor.
data SigningPlatformOverrides = SigningPlatformOverrides'
  { -- | A signing configuration that overrides the default encryption or hash
    -- algorithm of a signing job.
    signingConfiguration :: Prelude.Maybe SigningConfigurationOverrides,
    -- | A signed image is a JSON object. When overriding the default signing
    -- platform configuration, a customer can select either of two signing
    -- formats, @JSONEmbedded@ or @JSONDetached@. (A third format value,
    -- @JSON@, is reserved for future use.) With @JSONEmbedded@, the signing
    -- image has the payload embedded in it. With @JSONDetached@, the payload
    -- is not be embedded in the signing image.
    signingImageFormat :: Prelude.Maybe ImageFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningPlatformOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingConfiguration', 'signingPlatformOverrides_signingConfiguration' - A signing configuration that overrides the default encryption or hash
-- algorithm of a signing job.
--
-- 'signingImageFormat', 'signingPlatformOverrides_signingImageFormat' - A signed image is a JSON object. When overriding the default signing
-- platform configuration, a customer can select either of two signing
-- formats, @JSONEmbedded@ or @JSONDetached@. (A third format value,
-- @JSON@, is reserved for future use.) With @JSONEmbedded@, the signing
-- image has the payload embedded in it. With @JSONDetached@, the payload
-- is not be embedded in the signing image.
newSigningPlatformOverrides ::
  SigningPlatformOverrides
newSigningPlatformOverrides =
  SigningPlatformOverrides'
    { signingConfiguration =
        Prelude.Nothing,
      signingImageFormat = Prelude.Nothing
    }

-- | A signing configuration that overrides the default encryption or hash
-- algorithm of a signing job.
signingPlatformOverrides_signingConfiguration :: Lens.Lens' SigningPlatformOverrides (Prelude.Maybe SigningConfigurationOverrides)
signingPlatformOverrides_signingConfiguration = Lens.lens (\SigningPlatformOverrides' {signingConfiguration} -> signingConfiguration) (\s@SigningPlatformOverrides' {} a -> s {signingConfiguration = a} :: SigningPlatformOverrides)

-- | A signed image is a JSON object. When overriding the default signing
-- platform configuration, a customer can select either of two signing
-- formats, @JSONEmbedded@ or @JSONDetached@. (A third format value,
-- @JSON@, is reserved for future use.) With @JSONEmbedded@, the signing
-- image has the payload embedded in it. With @JSONDetached@, the payload
-- is not be embedded in the signing image.
signingPlatformOverrides_signingImageFormat :: Lens.Lens' SigningPlatformOverrides (Prelude.Maybe ImageFormat)
signingPlatformOverrides_signingImageFormat = Lens.lens (\SigningPlatformOverrides' {signingImageFormat} -> signingImageFormat) (\s@SigningPlatformOverrides' {} a -> s {signingImageFormat = a} :: SigningPlatformOverrides)

instance Data.FromJSON SigningPlatformOverrides where
  parseJSON =
    Data.withObject
      "SigningPlatformOverrides"
      ( \x ->
          SigningPlatformOverrides'
            Prelude.<$> (x Data..:? "signingConfiguration")
            Prelude.<*> (x Data..:? "signingImageFormat")
      )

instance Prelude.Hashable SigningPlatformOverrides where
  hashWithSalt _salt SigningPlatformOverrides' {..} =
    _salt `Prelude.hashWithSalt` signingConfiguration
      `Prelude.hashWithSalt` signingImageFormat

instance Prelude.NFData SigningPlatformOverrides where
  rnf SigningPlatformOverrides' {..} =
    Prelude.rnf signingConfiguration
      `Prelude.seq` Prelude.rnf signingImageFormat

instance Data.ToJSON SigningPlatformOverrides where
  toJSON SigningPlatformOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("signingConfiguration" Data..=)
              Prelude.<$> signingConfiguration,
            ("signingImageFormat" Data..=)
              Prelude.<$> signingImageFormat
          ]
      )
