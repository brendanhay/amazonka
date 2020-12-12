{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.GrantConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.GrantConstraints
  ( GrantConstraints (..),

    -- * Smart constructor
    mkGrantConstraints,

    -- * Lenses
    gcEncryptionContextEquals,
    gcEncryptionContextSubset,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use this structure to allow <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> in the grant only when the operation request includes the specified <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context> .
--
-- AWS KMS applies the grant constraints only to cryptographic operations that support an encryption context, that is, all cryptographic operations with a <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html#symmetric-cmks symmetric CMK> . Grant constraints are not applied to operations that do not support an encryption context, such as cryptographic operations with asymmetric CMKs and management operations, such as 'DescribeKey' or 'ScheduleKeyDeletion' .
-- /Important:/ In a cryptographic operation, the encryption context in the decryption operation must be an exact, case-sensitive match for the keys and values in the encryption context of the encryption operation. Only the order of the pairs can vary.
-- However, in a grant constraint, the key in each key-value pair is not case sensitive, but the value is case sensitive.
-- To avoid confusion, do not use multiple encryption context pairs that differ only by case. To require a fully case-sensitive encryption context, use the @kms:EncryptionContext:@ and @kms:EncryptionContextKeys@ conditions in an IAM or key policy. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-encryption-context kms:EncryptionContext:> in the /\/AWS Key Management Service Developer Guide\/ / .
--
-- /See:/ 'mkGrantConstraints' smart constructor.
data GrantConstraints = GrantConstraints'
  { encryptionContextEquals ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    encryptionContextSubset ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GrantConstraints' with the minimum fields required to make a request.
--
-- * 'encryptionContextEquals' - A list of key-value pairs that must match the encryption context in the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the operation only when the encryption context in the request is the same as the encryption context specified in this constraint.
-- * 'encryptionContextSubset' - A list of key-value pairs that must be included in the encryption context of the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the cryptographic operation only when the encryption context in the request includes the key-value pairs specified in this constraint, although it can include additional key-value pairs.
mkGrantConstraints ::
  GrantConstraints
mkGrantConstraints =
  GrantConstraints'
    { encryptionContextEquals = Lude.Nothing,
      encryptionContextSubset = Lude.Nothing
    }

-- | A list of key-value pairs that must match the encryption context in the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the operation only when the encryption context in the request is the same as the encryption context specified in this constraint.
--
-- /Note:/ Consider using 'encryptionContextEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEncryptionContextEquals :: Lens.Lens' GrantConstraints (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gcEncryptionContextEquals = Lens.lens (encryptionContextEquals :: GrantConstraints -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {encryptionContextEquals = a} :: GrantConstraints)
{-# DEPRECATED gcEncryptionContextEquals "Use generic-lens or generic-optics with 'encryptionContextEquals' instead." #-}

-- | A list of key-value pairs that must be included in the encryption context of the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the cryptographic operation only when the encryption context in the request includes the key-value pairs specified in this constraint, although it can include additional key-value pairs.
--
-- /Note:/ Consider using 'encryptionContextSubset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEncryptionContextSubset :: Lens.Lens' GrantConstraints (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gcEncryptionContextSubset = Lens.lens (encryptionContextSubset :: GrantConstraints -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {encryptionContextSubset = a} :: GrantConstraints)
{-# DEPRECATED gcEncryptionContextSubset "Use generic-lens or generic-optics with 'encryptionContextSubset' instead." #-}

instance Lude.FromJSON GrantConstraints where
  parseJSON =
    Lude.withObject
      "GrantConstraints"
      ( \x ->
          GrantConstraints'
            Lude.<$> (x Lude..:? "EncryptionContextEquals" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EncryptionContextSubset" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON GrantConstraints where
  toJSON GrantConstraints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EncryptionContextEquals" Lude..=)
              Lude.<$> encryptionContextEquals,
            ("EncryptionContextSubset" Lude..=)
              Lude.<$> encryptionContextSubset
          ]
      )
