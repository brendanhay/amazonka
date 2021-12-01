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
-- Module      : Amazonka.MediaConvert.Types.CmafEncryptionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmafEncryptionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.CmafEncryptionType
import Amazonka.MediaConvert.Types.CmafInitializationVectorInManifest
import Amazonka.MediaConvert.Types.CmafKeyProviderType
import Amazonka.MediaConvert.Types.SpekeKeyProviderCmaf
import Amazonka.MediaConvert.Types.StaticKeyProvider
import qualified Amazonka.Prelude as Prelude

-- | Settings for CMAF encryption
--
-- /See:/ 'newCmafEncryptionSettings' smart constructor.
data CmafEncryptionSettings = CmafEncryptionSettings'
  { -- | Specify the encryption scheme that you want the service to use when
    -- encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or
    -- AES_CTR (AES-CTR).
    encryptionMethod :: Prelude.Maybe CmafEncryptionType,
    -- | This is a 128-bit, 16-byte hex value represented by a 32-character text
    -- string. If this parameter is not set then the Initialization Vector will
    -- follow the segment number by default.
    constantInitializationVector :: Prelude.Maybe Prelude.Text,
    -- | Specify whether your DRM encryption key is static or from a key provider
    -- that follows the SPEKE standard. For more information about SPEKE, see
    -- https:\/\/docs.aws.amazon.com\/speke\/latest\/documentation\/what-is-speke.html.
    type' :: Prelude.Maybe CmafKeyProviderType,
    -- | Use these settings to set up encryption with a static key provider.
    staticKeyProvider :: Prelude.Maybe StaticKeyProvider,
    -- | If your output group type is CMAF, use these settings when doing DRM
    -- encryption with a SPEKE-compliant key provider. If your output group
    -- type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider
    -- settings instead.
    spekeKeyProvider :: Prelude.Maybe SpekeKeyProviderCmaf,
    -- | When you use DRM with CMAF outputs, choose whether the service writes
    -- the 128-bit encryption initialization vector in the HLS and DASH
    -- manifests.
    initializationVectorInManifest :: Prelude.Maybe CmafInitializationVectorInManifest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CmafEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionMethod', 'cmafEncryptionSettings_encryptionMethod' - Specify the encryption scheme that you want the service to use when
-- encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or
-- AES_CTR (AES-CTR).
--
-- 'constantInitializationVector', 'cmafEncryptionSettings_constantInitializationVector' - This is a 128-bit, 16-byte hex value represented by a 32-character text
-- string. If this parameter is not set then the Initialization Vector will
-- follow the segment number by default.
--
-- 'type'', 'cmafEncryptionSettings_type' - Specify whether your DRM encryption key is static or from a key provider
-- that follows the SPEKE standard. For more information about SPEKE, see
-- https:\/\/docs.aws.amazon.com\/speke\/latest\/documentation\/what-is-speke.html.
--
-- 'staticKeyProvider', 'cmafEncryptionSettings_staticKeyProvider' - Use these settings to set up encryption with a static key provider.
--
-- 'spekeKeyProvider', 'cmafEncryptionSettings_spekeKeyProvider' - If your output group type is CMAF, use these settings when doing DRM
-- encryption with a SPEKE-compliant key provider. If your output group
-- type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider
-- settings instead.
--
-- 'initializationVectorInManifest', 'cmafEncryptionSettings_initializationVectorInManifest' - When you use DRM with CMAF outputs, choose whether the service writes
-- the 128-bit encryption initialization vector in the HLS and DASH
-- manifests.
newCmafEncryptionSettings ::
  CmafEncryptionSettings
newCmafEncryptionSettings =
  CmafEncryptionSettings'
    { encryptionMethod =
        Prelude.Nothing,
      constantInitializationVector = Prelude.Nothing,
      type' = Prelude.Nothing,
      staticKeyProvider = Prelude.Nothing,
      spekeKeyProvider = Prelude.Nothing,
      initializationVectorInManifest = Prelude.Nothing
    }

-- | Specify the encryption scheme that you want the service to use when
-- encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or
-- AES_CTR (AES-CTR).
cmafEncryptionSettings_encryptionMethod :: Lens.Lens' CmafEncryptionSettings (Prelude.Maybe CmafEncryptionType)
cmafEncryptionSettings_encryptionMethod = Lens.lens (\CmafEncryptionSettings' {encryptionMethod} -> encryptionMethod) (\s@CmafEncryptionSettings' {} a -> s {encryptionMethod = a} :: CmafEncryptionSettings)

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text
-- string. If this parameter is not set then the Initialization Vector will
-- follow the segment number by default.
cmafEncryptionSettings_constantInitializationVector :: Lens.Lens' CmafEncryptionSettings (Prelude.Maybe Prelude.Text)
cmafEncryptionSettings_constantInitializationVector = Lens.lens (\CmafEncryptionSettings' {constantInitializationVector} -> constantInitializationVector) (\s@CmafEncryptionSettings' {} a -> s {constantInitializationVector = a} :: CmafEncryptionSettings)

-- | Specify whether your DRM encryption key is static or from a key provider
-- that follows the SPEKE standard. For more information about SPEKE, see
-- https:\/\/docs.aws.amazon.com\/speke\/latest\/documentation\/what-is-speke.html.
cmafEncryptionSettings_type :: Lens.Lens' CmafEncryptionSettings (Prelude.Maybe CmafKeyProviderType)
cmafEncryptionSettings_type = Lens.lens (\CmafEncryptionSettings' {type'} -> type') (\s@CmafEncryptionSettings' {} a -> s {type' = a} :: CmafEncryptionSettings)

-- | Use these settings to set up encryption with a static key provider.
cmafEncryptionSettings_staticKeyProvider :: Lens.Lens' CmafEncryptionSettings (Prelude.Maybe StaticKeyProvider)
cmafEncryptionSettings_staticKeyProvider = Lens.lens (\CmafEncryptionSettings' {staticKeyProvider} -> staticKeyProvider) (\s@CmafEncryptionSettings' {} a -> s {staticKeyProvider = a} :: CmafEncryptionSettings)

-- | If your output group type is CMAF, use these settings when doing DRM
-- encryption with a SPEKE-compliant key provider. If your output group
-- type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider
-- settings instead.
cmafEncryptionSettings_spekeKeyProvider :: Lens.Lens' CmafEncryptionSettings (Prelude.Maybe SpekeKeyProviderCmaf)
cmafEncryptionSettings_spekeKeyProvider = Lens.lens (\CmafEncryptionSettings' {spekeKeyProvider} -> spekeKeyProvider) (\s@CmafEncryptionSettings' {} a -> s {spekeKeyProvider = a} :: CmafEncryptionSettings)

-- | When you use DRM with CMAF outputs, choose whether the service writes
-- the 128-bit encryption initialization vector in the HLS and DASH
-- manifests.
cmafEncryptionSettings_initializationVectorInManifest :: Lens.Lens' CmafEncryptionSettings (Prelude.Maybe CmafInitializationVectorInManifest)
cmafEncryptionSettings_initializationVectorInManifest = Lens.lens (\CmafEncryptionSettings' {initializationVectorInManifest} -> initializationVectorInManifest) (\s@CmafEncryptionSettings' {} a -> s {initializationVectorInManifest = a} :: CmafEncryptionSettings)

instance Core.FromJSON CmafEncryptionSettings where
  parseJSON =
    Core.withObject
      "CmafEncryptionSettings"
      ( \x ->
          CmafEncryptionSettings'
            Prelude.<$> (x Core..:? "encryptionMethod")
            Prelude.<*> (x Core..:? "constantInitializationVector")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "staticKeyProvider")
            Prelude.<*> (x Core..:? "spekeKeyProvider")
            Prelude.<*> (x Core..:? "initializationVectorInManifest")
      )

instance Prelude.Hashable CmafEncryptionSettings where
  hashWithSalt salt' CmafEncryptionSettings' {..} =
    salt'
      `Prelude.hashWithSalt` initializationVectorInManifest
      `Prelude.hashWithSalt` spekeKeyProvider
      `Prelude.hashWithSalt` staticKeyProvider
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` constantInitializationVector
      `Prelude.hashWithSalt` encryptionMethod

instance Prelude.NFData CmafEncryptionSettings where
  rnf CmafEncryptionSettings' {..} =
    Prelude.rnf encryptionMethod
      `Prelude.seq` Prelude.rnf initializationVectorInManifest
      `Prelude.seq` Prelude.rnf spekeKeyProvider
      `Prelude.seq` Prelude.rnf staticKeyProvider
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf constantInitializationVector

instance Core.ToJSON CmafEncryptionSettings where
  toJSON CmafEncryptionSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("encryptionMethod" Core..=)
              Prelude.<$> encryptionMethod,
            ("constantInitializationVector" Core..=)
              Prelude.<$> constantInitializationVector,
            ("type" Core..=) Prelude.<$> type',
            ("staticKeyProvider" Core..=)
              Prelude.<$> staticKeyProvider,
            ("spekeKeyProvider" Core..=)
              Prelude.<$> spekeKeyProvider,
            ("initializationVectorInManifest" Core..=)
              Prelude.<$> initializationVectorInManifest
          ]
      )
