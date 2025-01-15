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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CustomCodeSigning where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.CodeSigningCertificateChain
import Amazonka.IoT.Types.CodeSigningSignature
import qualified Amazonka.Prelude as Prelude

-- | Describes a custom method used to code sign a file.
--
-- /See:/ 'newCustomCodeSigning' smart constructor.
data CustomCodeSigning = CustomCodeSigning'
  { -- | The certificate chain.
    certificateChain :: Prelude.Maybe CodeSigningCertificateChain,
    -- | The hash algorithm used to code sign the file. You can use a string as
    -- the algorithm name if the target over-the-air (OTA) update devices are
    -- able to verify the signature that was generated using the same signature
    -- algorithm. For example, FreeRTOS uses @SHA256@ or @SHA1@, so you can
    -- pass either of them based on which was used for generating the
    -- signature.
    hashAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The signature for the file.
    signature :: Prelude.Maybe CodeSigningSignature,
    -- | The signature algorithm used to code sign the file. You can use a string
    -- as the algorithm name if the target over-the-air (OTA) update devices
    -- are able to verify the signature that was generated using the same
    -- signature algorithm. For example, FreeRTOS uses @ECDSA@ or @RSA@, so you
    -- can pass either of them based on which was used for generating the
    -- signature.
    signatureAlgorithm :: Prelude.Maybe Prelude.Text
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
-- 'certificateChain', 'customCodeSigning_certificateChain' - The certificate chain.
--
-- 'hashAlgorithm', 'customCodeSigning_hashAlgorithm' - The hash algorithm used to code sign the file. You can use a string as
-- the algorithm name if the target over-the-air (OTA) update devices are
-- able to verify the signature that was generated using the same signature
-- algorithm. For example, FreeRTOS uses @SHA256@ or @SHA1@, so you can
-- pass either of them based on which was used for generating the
-- signature.
--
-- 'signature', 'customCodeSigning_signature' - The signature for the file.
--
-- 'signatureAlgorithm', 'customCodeSigning_signatureAlgorithm' - The signature algorithm used to code sign the file. You can use a string
-- as the algorithm name if the target over-the-air (OTA) update devices
-- are able to verify the signature that was generated using the same
-- signature algorithm. For example, FreeRTOS uses @ECDSA@ or @RSA@, so you
-- can pass either of them based on which was used for generating the
-- signature.
newCustomCodeSigning ::
  CustomCodeSigning
newCustomCodeSigning =
  CustomCodeSigning'
    { certificateChain =
        Prelude.Nothing,
      hashAlgorithm = Prelude.Nothing,
      signature = Prelude.Nothing,
      signatureAlgorithm = Prelude.Nothing
    }

-- | The certificate chain.
customCodeSigning_certificateChain :: Lens.Lens' CustomCodeSigning (Prelude.Maybe CodeSigningCertificateChain)
customCodeSigning_certificateChain = Lens.lens (\CustomCodeSigning' {certificateChain} -> certificateChain) (\s@CustomCodeSigning' {} a -> s {certificateChain = a} :: CustomCodeSigning)

-- | The hash algorithm used to code sign the file. You can use a string as
-- the algorithm name if the target over-the-air (OTA) update devices are
-- able to verify the signature that was generated using the same signature
-- algorithm. For example, FreeRTOS uses @SHA256@ or @SHA1@, so you can
-- pass either of them based on which was used for generating the
-- signature.
customCodeSigning_hashAlgorithm :: Lens.Lens' CustomCodeSigning (Prelude.Maybe Prelude.Text)
customCodeSigning_hashAlgorithm = Lens.lens (\CustomCodeSigning' {hashAlgorithm} -> hashAlgorithm) (\s@CustomCodeSigning' {} a -> s {hashAlgorithm = a} :: CustomCodeSigning)

-- | The signature for the file.
customCodeSigning_signature :: Lens.Lens' CustomCodeSigning (Prelude.Maybe CodeSigningSignature)
customCodeSigning_signature = Lens.lens (\CustomCodeSigning' {signature} -> signature) (\s@CustomCodeSigning' {} a -> s {signature = a} :: CustomCodeSigning)

-- | The signature algorithm used to code sign the file. You can use a string
-- as the algorithm name if the target over-the-air (OTA) update devices
-- are able to verify the signature that was generated using the same
-- signature algorithm. For example, FreeRTOS uses @ECDSA@ or @RSA@, so you
-- can pass either of them based on which was used for generating the
-- signature.
customCodeSigning_signatureAlgorithm :: Lens.Lens' CustomCodeSigning (Prelude.Maybe Prelude.Text)
customCodeSigning_signatureAlgorithm = Lens.lens (\CustomCodeSigning' {signatureAlgorithm} -> signatureAlgorithm) (\s@CustomCodeSigning' {} a -> s {signatureAlgorithm = a} :: CustomCodeSigning)

instance Data.FromJSON CustomCodeSigning where
  parseJSON =
    Data.withObject
      "CustomCodeSigning"
      ( \x ->
          CustomCodeSigning'
            Prelude.<$> (x Data..:? "certificateChain")
            Prelude.<*> (x Data..:? "hashAlgorithm")
            Prelude.<*> (x Data..:? "signature")
            Prelude.<*> (x Data..:? "signatureAlgorithm")
      )

instance Prelude.Hashable CustomCodeSigning where
  hashWithSalt _salt CustomCodeSigning' {..} =
    _salt
      `Prelude.hashWithSalt` certificateChain
      `Prelude.hashWithSalt` hashAlgorithm
      `Prelude.hashWithSalt` signature
      `Prelude.hashWithSalt` signatureAlgorithm

instance Prelude.NFData CustomCodeSigning where
  rnf CustomCodeSigning' {..} =
    Prelude.rnf certificateChain `Prelude.seq`
      Prelude.rnf hashAlgorithm `Prelude.seq`
        Prelude.rnf signature `Prelude.seq`
          Prelude.rnf signatureAlgorithm

instance Data.ToJSON CustomCodeSigning where
  toJSON CustomCodeSigning' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateChain" Data..=)
              Prelude.<$> certificateChain,
            ("hashAlgorithm" Data..=) Prelude.<$> hashAlgorithm,
            ("signature" Data..=) Prelude.<$> signature,
            ("signatureAlgorithm" Data..=)
              Prelude.<$> signatureAlgorithm
          ]
      )
