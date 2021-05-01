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
-- Module      : Network.AWS.IoT.Types.CustomCodeSigning
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CustomCodeSigning where

import Network.AWS.IoT.Types.CodeSigningCertificateChain
import Network.AWS.IoT.Types.CodeSigningSignature
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a custom method used to code sign a file.
--
-- /See:/ 'newCustomCodeSigning' smart constructor.
data CustomCodeSigning = CustomCodeSigning'
  { -- | The signature for the file.
    signature :: Prelude.Maybe CodeSigningSignature,
    -- | The signature algorithm used to code sign the file.
    signatureAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The certificate chain.
    certificateChain :: Prelude.Maybe CodeSigningCertificateChain,
    -- | The hash algorithm used to code sign the file.
    hashAlgorithm :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomCodeSigning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signature', 'customCodeSigning_signature' - The signature for the file.
--
-- 'signatureAlgorithm', 'customCodeSigning_signatureAlgorithm' - The signature algorithm used to code sign the file.
--
-- 'certificateChain', 'customCodeSigning_certificateChain' - The certificate chain.
--
-- 'hashAlgorithm', 'customCodeSigning_hashAlgorithm' - The hash algorithm used to code sign the file.
newCustomCodeSigning ::
  CustomCodeSigning
newCustomCodeSigning =
  CustomCodeSigning'
    { signature = Prelude.Nothing,
      signatureAlgorithm = Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      hashAlgorithm = Prelude.Nothing
    }

-- | The signature for the file.
customCodeSigning_signature :: Lens.Lens' CustomCodeSigning (Prelude.Maybe CodeSigningSignature)
customCodeSigning_signature = Lens.lens (\CustomCodeSigning' {signature} -> signature) (\s@CustomCodeSigning' {} a -> s {signature = a} :: CustomCodeSigning)

-- | The signature algorithm used to code sign the file.
customCodeSigning_signatureAlgorithm :: Lens.Lens' CustomCodeSigning (Prelude.Maybe Prelude.Text)
customCodeSigning_signatureAlgorithm = Lens.lens (\CustomCodeSigning' {signatureAlgorithm} -> signatureAlgorithm) (\s@CustomCodeSigning' {} a -> s {signatureAlgorithm = a} :: CustomCodeSigning)

-- | The certificate chain.
customCodeSigning_certificateChain :: Lens.Lens' CustomCodeSigning (Prelude.Maybe CodeSigningCertificateChain)
customCodeSigning_certificateChain = Lens.lens (\CustomCodeSigning' {certificateChain} -> certificateChain) (\s@CustomCodeSigning' {} a -> s {certificateChain = a} :: CustomCodeSigning)

-- | The hash algorithm used to code sign the file.
customCodeSigning_hashAlgorithm :: Lens.Lens' CustomCodeSigning (Prelude.Maybe Prelude.Text)
customCodeSigning_hashAlgorithm = Lens.lens (\CustomCodeSigning' {hashAlgorithm} -> hashAlgorithm) (\s@CustomCodeSigning' {} a -> s {hashAlgorithm = a} :: CustomCodeSigning)

instance Prelude.FromJSON CustomCodeSigning where
  parseJSON =
    Prelude.withObject
      "CustomCodeSigning"
      ( \x ->
          CustomCodeSigning'
            Prelude.<$> (x Prelude..:? "signature")
            Prelude.<*> (x Prelude..:? "signatureAlgorithm")
            Prelude.<*> (x Prelude..:? "certificateChain")
            Prelude.<*> (x Prelude..:? "hashAlgorithm")
      )

instance Prelude.Hashable CustomCodeSigning

instance Prelude.NFData CustomCodeSigning

instance Prelude.ToJSON CustomCodeSigning where
  toJSON CustomCodeSigning' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("signature" Prelude..=) Prelude.<$> signature,
            ("signatureAlgorithm" Prelude..=)
              Prelude.<$> signatureAlgorithm,
            ("certificateChain" Prelude..=)
              Prelude.<$> certificateChain,
            ("hashAlgorithm" Prelude..=)
              Prelude.<$> hashAlgorithm
          ]
      )
