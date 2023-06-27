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
-- Module      : Amazonka.PaymentCryptography.Types.TrustedCertificatePublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.TrustedCertificatePublicKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.KeyAttributes
import qualified Amazonka.Prelude as Prelude

-- | Parameter information for trusted public key certificate import.
--
-- /See:/ 'newTrustedCertificatePublicKey' smart constructor.
data TrustedCertificatePublicKey = TrustedCertificatePublicKey'
  { -- | The @KeyARN@ of the root public key certificate or certificate chain
    -- that signs the trusted public key certificate import.
    certificateAuthorityPublicKeyIdentifier :: Prelude.Text,
    -- | The role of the key, the algorithm it supports, and the cryptographic
    -- operations allowed with the key. This data is immutable after a trusted
    -- public key is imported.
    keyAttributes :: KeyAttributes,
    -- | Parameter information for trusted public key certificate import.
    publicKeyCertificate :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustedCertificatePublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityPublicKeyIdentifier', 'trustedCertificatePublicKey_certificateAuthorityPublicKeyIdentifier' - The @KeyARN@ of the root public key certificate or certificate chain
-- that signs the trusted public key certificate import.
--
-- 'keyAttributes', 'trustedCertificatePublicKey_keyAttributes' - The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after a trusted
-- public key is imported.
--
-- 'publicKeyCertificate', 'trustedCertificatePublicKey_publicKeyCertificate' - Parameter information for trusted public key certificate import.
newTrustedCertificatePublicKey ::
  -- | 'certificateAuthorityPublicKeyIdentifier'
  Prelude.Text ->
  -- | 'keyAttributes'
  KeyAttributes ->
  -- | 'publicKeyCertificate'
  Prelude.Text ->
  TrustedCertificatePublicKey
newTrustedCertificatePublicKey
  pCertificateAuthorityPublicKeyIdentifier_
  pKeyAttributes_
  pPublicKeyCertificate_ =
    TrustedCertificatePublicKey'
      { certificateAuthorityPublicKeyIdentifier =
          pCertificateAuthorityPublicKeyIdentifier_,
        keyAttributes = pKeyAttributes_,
        publicKeyCertificate =
          Data._Sensitive
            Lens.# pPublicKeyCertificate_
      }

-- | The @KeyARN@ of the root public key certificate or certificate chain
-- that signs the trusted public key certificate import.
trustedCertificatePublicKey_certificateAuthorityPublicKeyIdentifier :: Lens.Lens' TrustedCertificatePublicKey Prelude.Text
trustedCertificatePublicKey_certificateAuthorityPublicKeyIdentifier = Lens.lens (\TrustedCertificatePublicKey' {certificateAuthorityPublicKeyIdentifier} -> certificateAuthorityPublicKeyIdentifier) (\s@TrustedCertificatePublicKey' {} a -> s {certificateAuthorityPublicKeyIdentifier = a} :: TrustedCertificatePublicKey)

-- | The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after a trusted
-- public key is imported.
trustedCertificatePublicKey_keyAttributes :: Lens.Lens' TrustedCertificatePublicKey KeyAttributes
trustedCertificatePublicKey_keyAttributes = Lens.lens (\TrustedCertificatePublicKey' {keyAttributes} -> keyAttributes) (\s@TrustedCertificatePublicKey' {} a -> s {keyAttributes = a} :: TrustedCertificatePublicKey)

-- | Parameter information for trusted public key certificate import.
trustedCertificatePublicKey_publicKeyCertificate :: Lens.Lens' TrustedCertificatePublicKey Prelude.Text
trustedCertificatePublicKey_publicKeyCertificate = Lens.lens (\TrustedCertificatePublicKey' {publicKeyCertificate} -> publicKeyCertificate) (\s@TrustedCertificatePublicKey' {} a -> s {publicKeyCertificate = a} :: TrustedCertificatePublicKey) Prelude.. Data._Sensitive

instance Prelude.Hashable TrustedCertificatePublicKey where
  hashWithSalt _salt TrustedCertificatePublicKey' {..} =
    _salt
      `Prelude.hashWithSalt` certificateAuthorityPublicKeyIdentifier
      `Prelude.hashWithSalt` keyAttributes
      `Prelude.hashWithSalt` publicKeyCertificate

instance Prelude.NFData TrustedCertificatePublicKey where
  rnf TrustedCertificatePublicKey' {..} =
    Prelude.rnf certificateAuthorityPublicKeyIdentifier
      `Prelude.seq` Prelude.rnf keyAttributes
      `Prelude.seq` Prelude.rnf publicKeyCertificate

instance Data.ToJSON TrustedCertificatePublicKey where
  toJSON TrustedCertificatePublicKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityPublicKeyIdentifier"
                  Data..= certificateAuthorityPublicKeyIdentifier
              ),
            Prelude.Just ("KeyAttributes" Data..= keyAttributes),
            Prelude.Just
              ( "PublicKeyCertificate"
                  Data..= publicKeyCertificate
              )
          ]
      )
