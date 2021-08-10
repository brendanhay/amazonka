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
-- Module      : Network.AWS.KMS.Types.GrantConstraints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.GrantConstraints where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Use this structure to allow
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- in the grant only when the operation request includes the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context>.
--
-- AWS KMS applies the grant constraints only to cryptographic operations
-- that support an encryption context, that is, all cryptographic
-- operations with a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html#symmetric-cmks symmetric CMK>.
-- Grant constraints are not applied to operations that do not support an
-- encryption context, such as cryptographic operations with asymmetric
-- CMKs and management operations, such as DescribeKey or RetireGrant.
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
-- in the //AWS Key Management Service Developer Guide// .
--
-- /See:/ 'newGrantConstraints' smart constructor.
data GrantConstraints = GrantConstraints'
  { -- | A list of key-value pairs that must match the encryption context in the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
    -- request. The grant allows the operation only when the encryption context
    -- in the request is the same as the encryption context specified in this
    -- constraint.
    encryptionContextEquals :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of key-value pairs that must be included in the encryption
    -- context of the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
    -- request. The grant allows the cryptographic operation only when the
    -- encryption context in the request includes the key-value pairs specified
    -- in this constraint, although it can include additional key-value pairs.
    encryptionContextSubset :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'encryptionContextEquals', 'grantConstraints_encryptionContextEquals' - A list of key-value pairs that must match the encryption context in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the operation only when the encryption context
-- in the request is the same as the encryption context specified in this
-- constraint.
--
-- 'encryptionContextSubset', 'grantConstraints_encryptionContextSubset' - A list of key-value pairs that must be included in the encryption
-- context of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the cryptographic operation only when the
-- encryption context in the request includes the key-value pairs specified
-- in this constraint, although it can include additional key-value pairs.
newGrantConstraints ::
  GrantConstraints
newGrantConstraints =
  GrantConstraints'
    { encryptionContextEquals =
        Prelude.Nothing,
      encryptionContextSubset = Prelude.Nothing
    }

-- | A list of key-value pairs that must match the encryption context in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the operation only when the encryption context
-- in the request is the same as the encryption context specified in this
-- constraint.
grantConstraints_encryptionContextEquals :: Lens.Lens' GrantConstraints (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
grantConstraints_encryptionContextEquals = Lens.lens (\GrantConstraints' {encryptionContextEquals} -> encryptionContextEquals) (\s@GrantConstraints' {} a -> s {encryptionContextEquals = a} :: GrantConstraints) Prelude.. Lens.mapping Lens._Coerce

-- | A list of key-value pairs that must be included in the encryption
-- context of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the cryptographic operation only when the
-- encryption context in the request includes the key-value pairs specified
-- in this constraint, although it can include additional key-value pairs.
grantConstraints_encryptionContextSubset :: Lens.Lens' GrantConstraints (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
grantConstraints_encryptionContextSubset = Lens.lens (\GrantConstraints' {encryptionContextSubset} -> encryptionContextSubset) (\s@GrantConstraints' {} a -> s {encryptionContextSubset = a} :: GrantConstraints) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON GrantConstraints where
  parseJSON =
    Core.withObject
      "GrantConstraints"
      ( \x ->
          GrantConstraints'
            Prelude.<$> ( x Core..:? "EncryptionContextEquals"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "EncryptionContextSubset"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable GrantConstraints

instance Prelude.NFData GrantConstraints

instance Core.ToJSON GrantConstraints where
  toJSON GrantConstraints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EncryptionContextEquals" Core..=)
              Prelude.<$> encryptionContextEquals,
            ("EncryptionContextSubset" Core..=)
              Prelude.<$> encryptionContextSubset
          ]
      )
