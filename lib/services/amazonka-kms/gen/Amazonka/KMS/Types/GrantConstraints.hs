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
-- Module      : Amazonka.KMS.Types.GrantConstraints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.GrantConstraints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this structure to allow
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- in the grant only when the operation request includes the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context>.
--
-- KMS applies the grant constraints only to cryptographic operations that
-- support an encryption context, that is, all cryptographic operations
-- with a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html#symmetric-cmks symmetric encryption KMS key>.
-- Grant constraints are not applied to operations that do not support an
-- encryption context, such as cryptographic operations with HMAC KMS keys
-- or asymmetric KMS keys, and management operations, such as DescribeKey
-- or RetireGrant.
--
-- In a cryptographic operation, the encryption context in the decryption
-- operation must be an exact, case-sensitive match for the keys and values
-- in the encryption context of the encryption operation. Only the order of
-- the pairs can vary.
--
-- However, in a grant constraint, the key in each key-value pair is not
-- case sensitive, but the value is case sensitive.
--
-- To avoid confusion, do not use multiple encryption context pairs that
-- differ only by case. To require a fully case-sensitive encryption
-- context, use the @kms:EncryptionContext:@ and
-- @kms:EncryptionContextKeys@ conditions in an IAM or key policy. For
-- details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-encryption-context kms:EncryptionContext:>
-- in the //Key Management Service Developer Guide// .
--
-- /See:/ 'newGrantConstraints' smart constructor.
data GrantConstraints = GrantConstraints'
  { -- | A list of key-value pairs that must be included in the encryption
    -- context of the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
    -- request. The grant allows the cryptographic operation only when the
    -- encryption context in the request includes the key-value pairs specified
    -- in this constraint, although it can include additional key-value pairs.
    encryptionContextSubset :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of key-value pairs that must match the encryption context in the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
    -- request. The grant allows the operation only when the encryption context
    -- in the request is the same as the encryption context specified in this
    -- constraint.
    encryptionContextEquals :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrantConstraints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionContextSubset', 'grantConstraints_encryptionContextSubset' - A list of key-value pairs that must be included in the encryption
-- context of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the cryptographic operation only when the
-- encryption context in the request includes the key-value pairs specified
-- in this constraint, although it can include additional key-value pairs.
--
-- 'encryptionContextEquals', 'grantConstraints_encryptionContextEquals' - A list of key-value pairs that must match the encryption context in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the operation only when the encryption context
-- in the request is the same as the encryption context specified in this
-- constraint.
newGrantConstraints ::
  GrantConstraints
newGrantConstraints =
  GrantConstraints'
    { encryptionContextSubset =
        Prelude.Nothing,
      encryptionContextEquals = Prelude.Nothing
    }

-- | A list of key-value pairs that must be included in the encryption
-- context of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the cryptographic operation only when the
-- encryption context in the request includes the key-value pairs specified
-- in this constraint, although it can include additional key-value pairs.
grantConstraints_encryptionContextSubset :: Lens.Lens' GrantConstraints (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
grantConstraints_encryptionContextSubset = Lens.lens (\GrantConstraints' {encryptionContextSubset} -> encryptionContextSubset) (\s@GrantConstraints' {} a -> s {encryptionContextSubset = a} :: GrantConstraints) Prelude.. Lens.mapping Lens.coerced

-- | A list of key-value pairs that must match the encryption context in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the operation only when the encryption context
-- in the request is the same as the encryption context specified in this
-- constraint.
grantConstraints_encryptionContextEquals :: Lens.Lens' GrantConstraints (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
grantConstraints_encryptionContextEquals = Lens.lens (\GrantConstraints' {encryptionContextEquals} -> encryptionContextEquals) (\s@GrantConstraints' {} a -> s {encryptionContextEquals = a} :: GrantConstraints) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GrantConstraints where
  parseJSON =
    Data.withObject
      "GrantConstraints"
      ( \x ->
          GrantConstraints'
            Prelude.<$> ( x Data..:? "EncryptionContextSubset"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "EncryptionContextEquals"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable GrantConstraints where
  hashWithSalt _salt GrantConstraints' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionContextSubset
      `Prelude.hashWithSalt` encryptionContextEquals

instance Prelude.NFData GrantConstraints where
  rnf GrantConstraints' {..} =
    Prelude.rnf encryptionContextSubset
      `Prelude.seq` Prelude.rnf encryptionContextEquals

instance Data.ToJSON GrantConstraints where
  toJSON GrantConstraints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionContextSubset" Data..=)
              Prelude.<$> encryptionContextSubset,
            ("EncryptionContextEquals" Data..=)
              Prelude.<$> encryptionContextEquals
          ]
      )
