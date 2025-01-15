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
-- Module      : Amazonka.AccessAnalyzer.Types.KmsGrantConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.KmsGrantConfiguration where

import Amazonka.AccessAnalyzer.Types.KmsGrantConstraints
import Amazonka.AccessAnalyzer.Types.KmsGrantOperation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A proposed grant configuration for a KMS key. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html CreateGrant>.
--
-- /See:/ 'newKmsGrantConfiguration' smart constructor.
data KmsGrantConfiguration = KmsGrantConfiguration'
  { -- | Use this structure to propose allowing
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
    -- in the grant only when the operation request includes the specified
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context>.
    constraints :: Prelude.Maybe KmsGrantConstraints,
    -- | The principal that is given permission to retire the grant by using
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_RetireGrant.html RetireGrant>
    -- operation.
    retiringPrincipal :: Prelude.Maybe Prelude.Text,
    -- | A list of operations that the grant permits.
    operations :: [KmsGrantOperation],
    -- | The principal that is given permission to perform the operations that
    -- the grant permits.
    granteePrincipal :: Prelude.Text,
    -- | The Amazon Web Services account under which the grant was issued. The
    -- account is used to propose KMS grants issued by accounts other than the
    -- owner of the key.
    issuingAccount :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KmsGrantConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraints', 'kmsGrantConfiguration_constraints' - Use this structure to propose allowing
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- in the grant only when the operation request includes the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context>.
--
-- 'retiringPrincipal', 'kmsGrantConfiguration_retiringPrincipal' - The principal that is given permission to retire the grant by using
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_RetireGrant.html RetireGrant>
-- operation.
--
-- 'operations', 'kmsGrantConfiguration_operations' - A list of operations that the grant permits.
--
-- 'granteePrincipal', 'kmsGrantConfiguration_granteePrincipal' - The principal that is given permission to perform the operations that
-- the grant permits.
--
-- 'issuingAccount', 'kmsGrantConfiguration_issuingAccount' - The Amazon Web Services account under which the grant was issued. The
-- account is used to propose KMS grants issued by accounts other than the
-- owner of the key.
newKmsGrantConfiguration ::
  -- | 'granteePrincipal'
  Prelude.Text ->
  -- | 'issuingAccount'
  Prelude.Text ->
  KmsGrantConfiguration
newKmsGrantConfiguration
  pGranteePrincipal_
  pIssuingAccount_ =
    KmsGrantConfiguration'
      { constraints =
          Prelude.Nothing,
        retiringPrincipal = Prelude.Nothing,
        operations = Prelude.mempty,
        granteePrincipal = pGranteePrincipal_,
        issuingAccount = pIssuingAccount_
      }

-- | Use this structure to propose allowing
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- in the grant only when the operation request includes the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context>.
kmsGrantConfiguration_constraints :: Lens.Lens' KmsGrantConfiguration (Prelude.Maybe KmsGrantConstraints)
kmsGrantConfiguration_constraints = Lens.lens (\KmsGrantConfiguration' {constraints} -> constraints) (\s@KmsGrantConfiguration' {} a -> s {constraints = a} :: KmsGrantConfiguration)

-- | The principal that is given permission to retire the grant by using
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_RetireGrant.html RetireGrant>
-- operation.
kmsGrantConfiguration_retiringPrincipal :: Lens.Lens' KmsGrantConfiguration (Prelude.Maybe Prelude.Text)
kmsGrantConfiguration_retiringPrincipal = Lens.lens (\KmsGrantConfiguration' {retiringPrincipal} -> retiringPrincipal) (\s@KmsGrantConfiguration' {} a -> s {retiringPrincipal = a} :: KmsGrantConfiguration)

-- | A list of operations that the grant permits.
kmsGrantConfiguration_operations :: Lens.Lens' KmsGrantConfiguration [KmsGrantOperation]
kmsGrantConfiguration_operations = Lens.lens (\KmsGrantConfiguration' {operations} -> operations) (\s@KmsGrantConfiguration' {} a -> s {operations = a} :: KmsGrantConfiguration) Prelude.. Lens.coerced

-- | The principal that is given permission to perform the operations that
-- the grant permits.
kmsGrantConfiguration_granteePrincipal :: Lens.Lens' KmsGrantConfiguration Prelude.Text
kmsGrantConfiguration_granteePrincipal = Lens.lens (\KmsGrantConfiguration' {granteePrincipal} -> granteePrincipal) (\s@KmsGrantConfiguration' {} a -> s {granteePrincipal = a} :: KmsGrantConfiguration)

-- | The Amazon Web Services account under which the grant was issued. The
-- account is used to propose KMS grants issued by accounts other than the
-- owner of the key.
kmsGrantConfiguration_issuingAccount :: Lens.Lens' KmsGrantConfiguration Prelude.Text
kmsGrantConfiguration_issuingAccount = Lens.lens (\KmsGrantConfiguration' {issuingAccount} -> issuingAccount) (\s@KmsGrantConfiguration' {} a -> s {issuingAccount = a} :: KmsGrantConfiguration)

instance Data.FromJSON KmsGrantConfiguration where
  parseJSON =
    Data.withObject
      "KmsGrantConfiguration"
      ( \x ->
          KmsGrantConfiguration'
            Prelude.<$> (x Data..:? "constraints")
            Prelude.<*> (x Data..:? "retiringPrincipal")
            Prelude.<*> (x Data..:? "operations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "granteePrincipal")
            Prelude.<*> (x Data..: "issuingAccount")
      )

instance Prelude.Hashable KmsGrantConfiguration where
  hashWithSalt _salt KmsGrantConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` constraints
      `Prelude.hashWithSalt` retiringPrincipal
      `Prelude.hashWithSalt` operations
      `Prelude.hashWithSalt` granteePrincipal
      `Prelude.hashWithSalt` issuingAccount

instance Prelude.NFData KmsGrantConfiguration where
  rnf KmsGrantConfiguration' {..} =
    Prelude.rnf constraints `Prelude.seq`
      Prelude.rnf retiringPrincipal `Prelude.seq`
        Prelude.rnf operations `Prelude.seq`
          Prelude.rnf granteePrincipal `Prelude.seq`
            Prelude.rnf issuingAccount

instance Data.ToJSON KmsGrantConfiguration where
  toJSON KmsGrantConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("constraints" Data..=) Prelude.<$> constraints,
            ("retiringPrincipal" Data..=)
              Prelude.<$> retiringPrincipal,
            Prelude.Just ("operations" Data..= operations),
            Prelude.Just
              ("granteePrincipal" Data..= granteePrincipal),
            Prelude.Just
              ("issuingAccount" Data..= issuingAccount)
          ]
      )
