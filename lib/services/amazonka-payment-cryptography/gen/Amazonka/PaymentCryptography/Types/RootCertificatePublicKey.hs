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
-- Module      : Amazonka.PaymentCryptography.Types.RootCertificatePublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.RootCertificatePublicKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.KeyAttributes
import qualified Amazonka.Prelude as Prelude

-- | Parameter information for root public key certificate import.
--
-- /See:/ 'newRootCertificatePublicKey' smart constructor.
data RootCertificatePublicKey = RootCertificatePublicKey'
  { -- | The role of the key, the algorithm it supports, and the cryptographic
    -- operations allowed with the key. This data is immutable after the root
    -- public key is imported.
    keyAttributes :: KeyAttributes,
    -- | Parameter information for root public key certificate import.
    publicKeyCertificate :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RootCertificatePublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyAttributes', 'rootCertificatePublicKey_keyAttributes' - The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after the root
-- public key is imported.
--
-- 'publicKeyCertificate', 'rootCertificatePublicKey_publicKeyCertificate' - Parameter information for root public key certificate import.
newRootCertificatePublicKey ::
  -- | 'keyAttributes'
  KeyAttributes ->
  -- | 'publicKeyCertificate'
  Prelude.Text ->
  RootCertificatePublicKey
newRootCertificatePublicKey
  pKeyAttributes_
  pPublicKeyCertificate_ =
    RootCertificatePublicKey'
      { keyAttributes =
          pKeyAttributes_,
        publicKeyCertificate =
          Data._Sensitive Lens.# pPublicKeyCertificate_
      }

-- | The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after the root
-- public key is imported.
rootCertificatePublicKey_keyAttributes :: Lens.Lens' RootCertificatePublicKey KeyAttributes
rootCertificatePublicKey_keyAttributes = Lens.lens (\RootCertificatePublicKey' {keyAttributes} -> keyAttributes) (\s@RootCertificatePublicKey' {} a -> s {keyAttributes = a} :: RootCertificatePublicKey)

-- | Parameter information for root public key certificate import.
rootCertificatePublicKey_publicKeyCertificate :: Lens.Lens' RootCertificatePublicKey Prelude.Text
rootCertificatePublicKey_publicKeyCertificate = Lens.lens (\RootCertificatePublicKey' {publicKeyCertificate} -> publicKeyCertificate) (\s@RootCertificatePublicKey' {} a -> s {publicKeyCertificate = a} :: RootCertificatePublicKey) Prelude.. Data._Sensitive

instance Prelude.Hashable RootCertificatePublicKey where
  hashWithSalt _salt RootCertificatePublicKey' {..} =
    _salt
      `Prelude.hashWithSalt` keyAttributes
      `Prelude.hashWithSalt` publicKeyCertificate

instance Prelude.NFData RootCertificatePublicKey where
  rnf RootCertificatePublicKey' {..} =
    Prelude.rnf keyAttributes
      `Prelude.seq` Prelude.rnf publicKeyCertificate

instance Data.ToJSON RootCertificatePublicKey where
  toJSON RootCertificatePublicKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KeyAttributes" Data..= keyAttributes),
            Prelude.Just
              ( "PublicKeyCertificate"
                  Data..= publicKeyCertificate
              )
          ]
      )
