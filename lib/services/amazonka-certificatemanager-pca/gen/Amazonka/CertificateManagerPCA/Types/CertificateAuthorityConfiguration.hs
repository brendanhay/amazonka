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
-- Module      : Amazonka.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.CertificateAuthorityConfiguration where

import Amazonka.CertificateManagerPCA.Types.ASN1Subject
import Amazonka.CertificateManagerPCA.Types.CsrExtensions
import Amazonka.CertificateManagerPCA.Types.KeyAlgorithm
import Amazonka.CertificateManagerPCA.Types.SigningAlgorithm
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration information for your private certificate
-- authority (CA). This includes information about the class of public key
-- algorithm and the key pair that your private CA creates when it issues a
-- certificate. It also includes the signature algorithm that it uses when
-- issuing certificates, and its X.500 distinguished name. You must specify
-- this information when you call the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action.
--
-- /See:/ 'newCertificateAuthorityConfiguration' smart constructor.
data CertificateAuthorityConfiguration = CertificateAuthorityConfiguration'
  { -- | Specifies information to be added to the extension section of the
    -- certificate signing request (CSR).
    csrExtensions :: Prelude.Maybe CsrExtensions,
    -- | Type of the public key algorithm and size, in bits, of the key pair that
    -- your CA creates when it issues a certificate. When you create a
    -- subordinate CA, you must use a key algorithm supported by the parent CA.
    keyAlgorithm :: KeyAlgorithm,
    -- | Name of the algorithm your private CA uses to sign certificate requests.
    --
    -- This parameter should not be confused with the @SigningAlgorithm@
    -- parameter used to sign certificates when they are issued.
    signingAlgorithm :: SigningAlgorithm,
    -- | Structure that contains X.500 distinguished name information for your
    -- private CA.
    subject :: ASN1Subject
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateAuthorityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csrExtensions', 'certificateAuthorityConfiguration_csrExtensions' - Specifies information to be added to the extension section of the
-- certificate signing request (CSR).
--
-- 'keyAlgorithm', 'certificateAuthorityConfiguration_keyAlgorithm' - Type of the public key algorithm and size, in bits, of the key pair that
-- your CA creates when it issues a certificate. When you create a
-- subordinate CA, you must use a key algorithm supported by the parent CA.
--
-- 'signingAlgorithm', 'certificateAuthorityConfiguration_signingAlgorithm' - Name of the algorithm your private CA uses to sign certificate requests.
--
-- This parameter should not be confused with the @SigningAlgorithm@
-- parameter used to sign certificates when they are issued.
--
-- 'subject', 'certificateAuthorityConfiguration_subject' - Structure that contains X.500 distinguished name information for your
-- private CA.
newCertificateAuthorityConfiguration ::
  -- | 'keyAlgorithm'
  KeyAlgorithm ->
  -- | 'signingAlgorithm'
  SigningAlgorithm ->
  -- | 'subject'
  ASN1Subject ->
  CertificateAuthorityConfiguration
newCertificateAuthorityConfiguration
  pKeyAlgorithm_
  pSigningAlgorithm_
  pSubject_ =
    CertificateAuthorityConfiguration'
      { csrExtensions =
          Prelude.Nothing,
        keyAlgorithm = pKeyAlgorithm_,
        signingAlgorithm = pSigningAlgorithm_,
        subject = pSubject_
      }

-- | Specifies information to be added to the extension section of the
-- certificate signing request (CSR).
certificateAuthorityConfiguration_csrExtensions :: Lens.Lens' CertificateAuthorityConfiguration (Prelude.Maybe CsrExtensions)
certificateAuthorityConfiguration_csrExtensions = Lens.lens (\CertificateAuthorityConfiguration' {csrExtensions} -> csrExtensions) (\s@CertificateAuthorityConfiguration' {} a -> s {csrExtensions = a} :: CertificateAuthorityConfiguration)

-- | Type of the public key algorithm and size, in bits, of the key pair that
-- your CA creates when it issues a certificate. When you create a
-- subordinate CA, you must use a key algorithm supported by the parent CA.
certificateAuthorityConfiguration_keyAlgorithm :: Lens.Lens' CertificateAuthorityConfiguration KeyAlgorithm
certificateAuthorityConfiguration_keyAlgorithm = Lens.lens (\CertificateAuthorityConfiguration' {keyAlgorithm} -> keyAlgorithm) (\s@CertificateAuthorityConfiguration' {} a -> s {keyAlgorithm = a} :: CertificateAuthorityConfiguration)

-- | Name of the algorithm your private CA uses to sign certificate requests.
--
-- This parameter should not be confused with the @SigningAlgorithm@
-- parameter used to sign certificates when they are issued.
certificateAuthorityConfiguration_signingAlgorithm :: Lens.Lens' CertificateAuthorityConfiguration SigningAlgorithm
certificateAuthorityConfiguration_signingAlgorithm = Lens.lens (\CertificateAuthorityConfiguration' {signingAlgorithm} -> signingAlgorithm) (\s@CertificateAuthorityConfiguration' {} a -> s {signingAlgorithm = a} :: CertificateAuthorityConfiguration)

-- | Structure that contains X.500 distinguished name information for your
-- private CA.
certificateAuthorityConfiguration_subject :: Lens.Lens' CertificateAuthorityConfiguration ASN1Subject
certificateAuthorityConfiguration_subject = Lens.lens (\CertificateAuthorityConfiguration' {subject} -> subject) (\s@CertificateAuthorityConfiguration' {} a -> s {subject = a} :: CertificateAuthorityConfiguration)

instance
  Data.FromJSON
    CertificateAuthorityConfiguration
  where
  parseJSON =
    Data.withObject
      "CertificateAuthorityConfiguration"
      ( \x ->
          CertificateAuthorityConfiguration'
            Prelude.<$> (x Data..:? "CsrExtensions")
            Prelude.<*> (x Data..: "KeyAlgorithm")
            Prelude.<*> (x Data..: "SigningAlgorithm")
            Prelude.<*> (x Data..: "Subject")
      )

instance
  Prelude.Hashable
    CertificateAuthorityConfiguration
  where
  hashWithSalt
    _salt
    CertificateAuthorityConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` csrExtensions
        `Prelude.hashWithSalt` keyAlgorithm
        `Prelude.hashWithSalt` signingAlgorithm
        `Prelude.hashWithSalt` subject

instance
  Prelude.NFData
    CertificateAuthorityConfiguration
  where
  rnf CertificateAuthorityConfiguration' {..} =
    Prelude.rnf csrExtensions
      `Prelude.seq` Prelude.rnf keyAlgorithm
      `Prelude.seq` Prelude.rnf signingAlgorithm
      `Prelude.seq` Prelude.rnf subject

instance
  Data.ToJSON
    CertificateAuthorityConfiguration
  where
  toJSON CertificateAuthorityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CsrExtensions" Data..=) Prelude.<$> csrExtensions,
            Prelude.Just ("KeyAlgorithm" Data..= keyAlgorithm),
            Prelude.Just
              ("SigningAlgorithm" Data..= signingAlgorithm),
            Prelude.Just ("Subject" Data..= subject)
          ]
      )
