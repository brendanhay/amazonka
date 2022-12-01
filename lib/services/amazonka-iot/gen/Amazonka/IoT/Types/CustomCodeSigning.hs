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
-- Module      : Amazonka.IoT.Types.CustomCodeSigning
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CustomCodeSigning where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.CodeSigningCertificateChain
import Amazonka.IoT.Types.CodeSigningSignature
import qualified Amazonka.Prelude as Prelude

-- | Describes a custom method used to code sign a file.
--
-- /See:/ 'newCustomCodeSigning' smart constructor.
data CustomCodeSigning = CustomCodeSigning'
  { -- | The hash algorithm used to code sign the file. You can use a string as
    -- the algorithm name if the target over-the-air (OTA) update devices are
    -- able to verify the signature that was generated using the same signature
    -- algorithm. For example, FreeRTOS uses @SHA256@ or @SHA1@, so you can
    -- pass either of them based on which was used for generating the
    -- signature.
    hashAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The signature algorithm used to code sign the file. You can use a string
    -- as the algorithm name if the target over-the-air (OTA) update devices
    -- are able to verify the signature that was generated using the same
    -- signature algorithm. For example, FreeRTOS uses @ECDSA@ or @RSA@, so you
    -- can pass either of them based on which was used for generating the
    -- signature.
    signatureAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The certificate chain.
    certificateChain :: Prelude.Maybe CodeSigningCertificateChain,
    -- | The signature for the file.
    signature :: Prelude.Maybe CodeSigningSignature
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomCodeSigning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hashAlgorithm', 'customCodeSigning_hashAlgorithm' - The hash algorithm used to code sign the file. You can use a string as
-- the algorithm name if the target over-the-air (OTA) update devices are
-- able to verify the signature that was generated using the same signature
-- algorithm. For example, FreeRTOS uses @SHA256@ or @SHA1@, so you can
-- pass either of them based on which was used for generating the
-- signature.
--
-- 'signatureAlgorithm', 'customCodeSigning_signatureAlgorithm' - The signature algorithm used to code sign the file. You can use a string
-- as the algorithm name if the target over-the-air (OTA) update devices
-- are able to verify the signature that was generated using the same
-- signature algorithm. For example, FreeRTOS uses @ECDSA@ or @RSA@, so you
-- can pass either of them based on which was used for generating the
-- signature.
--
-- 'certificateChain', 'customCodeSigning_certificateChain' - The certificate chain.
--
-- 'signature', 'customCodeSigning_signature' - The signature for the file.
newCustomCodeSigning ::
  CustomCodeSigning
newCustomCodeSigning =
  CustomCodeSigning'
    { hashAlgorithm = Prelude.Nothing,
      signatureAlgorithm = Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      signature = Prelude.Nothing
    }

-- | The hash algorithm used to code sign the file. You can use a string as
-- the algorithm name if the target over-the-air (OTA) update devices are
-- able to verify the signature that was generated using the same signature
-- algorithm. For example, FreeRTOS uses @SHA256@ or @SHA1@, so you can
-- pass either of them based on which was used for generating the
-- signature.
customCodeSigning_hashAlgorithm :: Lens.Lens' CustomCodeSigning (Prelude.Maybe Prelude.Text)
customCodeSigning_hashAlgorithm = Lens.lens (\CustomCodeSigning' {hashAlgorithm} -> hashAlgorithm) (\s@CustomCodeSigning' {} a -> s {hashAlgorithm = a} :: CustomCodeSigning)

-- | The signature algorithm used to code sign the file. You can use a string
-- as the algorithm name if the target over-the-air (OTA) update devices
-- are able to verify the signature that was generated using the same
-- signature algorithm. For example, FreeRTOS uses @ECDSA@ or @RSA@, so you
-- can pass either of them based on which was used for generating the
-- signature.
customCodeSigning_signatureAlgorithm :: Lens.Lens' CustomCodeSigning (Prelude.Maybe Prelude.Text)
customCodeSigning_signatureAlgorithm = Lens.lens (\CustomCodeSigning' {signatureAlgorithm} -> signatureAlgorithm) (\s@CustomCodeSigning' {} a -> s {signatureAlgorithm = a} :: CustomCodeSigning)

-- | The certificate chain.
customCodeSigning_certificateChain :: Lens.Lens' CustomCodeSigning (Prelude.Maybe CodeSigningCertificateChain)
customCodeSigning_certificateChain = Lens.lens (\CustomCodeSigning' {certificateChain} -> certificateChain) (\s@CustomCodeSigning' {} a -> s {certificateChain = a} :: CustomCodeSigning)

-- | The signature for the file.
customCodeSigning_signature :: Lens.Lens' CustomCodeSigning (Prelude.Maybe CodeSigningSignature)
customCodeSigning_signature = Lens.lens (\CustomCodeSigning' {signature} -> signature) (\s@CustomCodeSigning' {} a -> s {signature = a} :: CustomCodeSigning)

instance Core.FromJSON CustomCodeSigning where
  parseJSON =
    Core.withObject
      "CustomCodeSigning"
      ( \x ->
          CustomCodeSigning'
            Prelude.<$> (x Core..:? "hashAlgorithm")
            Prelude.<*> (x Core..:? "signatureAlgorithm")
            Prelude.<*> (x Core..:? "certificateChain")
            Prelude.<*> (x Core..:? "signature")
      )

instance Prelude.Hashable CustomCodeSigning where
  hashWithSalt _salt CustomCodeSigning' {..} =
    _salt `Prelude.hashWithSalt` hashAlgorithm
      `Prelude.hashWithSalt` signatureAlgorithm
      `Prelude.hashWithSalt` certificateChain
      `Prelude.hashWithSalt` signature

instance Prelude.NFData CustomCodeSigning where
  rnf CustomCodeSigning' {..} =
    Prelude.rnf hashAlgorithm
      `Prelude.seq` Prelude.rnf signatureAlgorithm
      `Prelude.seq` Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf signature

instance Core.ToJSON CustomCodeSigning where
  toJSON CustomCodeSigning' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hashAlgorithm" Core..=) Prelude.<$> hashAlgorithm,
            ("signatureAlgorithm" Core..=)
              Prelude.<$> signatureAlgorithm,
            ("certificateChain" Core..=)
              Prelude.<$> certificateChain,
            ("signature" Core..=) Prelude.<$> signature
          ]
      )
