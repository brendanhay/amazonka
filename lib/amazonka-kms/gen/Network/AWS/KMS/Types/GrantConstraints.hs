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

import qualified Network.AWS.KMS.Types.EncryptionContextKey as Types
import qualified Network.AWS.KMS.Types.EncryptionContextValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use this structure to allow <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> in the grant only when the operation request includes the specified <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context> .
--
-- AWS KMS applies the grant constraints only to cryptographic operations that support an encryption context, that is, all cryptographic operations with a <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html#symmetric-cmks symmetric CMK> . Grant constraints are not applied to operations that do not support an encryption context, such as cryptographic operations with asymmetric CMKs and management operations, such as 'DescribeKey' or 'ScheduleKeyDeletion' .
-- /Important:/ In a cryptographic operation, the encryption context in the decryption operation must be an exact, case-sensitive match for the keys and values in the encryption context of the encryption operation. Only the order of the pairs can vary.
-- However, in a grant constraint, the key in each key-value pair is not case sensitive, but the value is case sensitive.
-- To avoid confusion, do not use multiple encryption context pairs that differ only by case. To require a fully case-sensitive encryption context, use the @kms:EncryptionContext:@ and @kms:EncryptionContextKeys@ conditions in an IAM or key policy. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-encryption-context kms:EncryptionContext:> in the /\/AWS Key Management Service Developer Guide\/ / .
--
-- /See:/ 'mkGrantConstraints' smart constructor.
data GrantConstraints = GrantConstraints'
  { -- | A list of key-value pairs that must match the encryption context in the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the operation only when the encryption context in the request is the same as the encryption context specified in this constraint.
    encryptionContextEquals :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue),
    -- | A list of key-value pairs that must be included in the encryption context of the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the cryptographic operation only when the encryption context in the request includes the key-value pairs specified in this constraint, although it can include additional key-value pairs.
    encryptionContextSubset :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GrantConstraints' value with any optional fields omitted.
mkGrantConstraints ::
  GrantConstraints
mkGrantConstraints =
  GrantConstraints'
    { encryptionContextEquals = Core.Nothing,
      encryptionContextSubset = Core.Nothing
    }

-- | A list of key-value pairs that must match the encryption context in the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the operation only when the encryption context in the request is the same as the encryption context specified in this constraint.
--
-- /Note:/ Consider using 'encryptionContextEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEncryptionContextEquals :: Lens.Lens' GrantConstraints (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
gcEncryptionContextEquals = Lens.field @"encryptionContextEquals"
{-# DEPRECATED gcEncryptionContextEquals "Use generic-lens or generic-optics with 'encryptionContextEquals' instead." #-}

-- | A list of key-value pairs that must be included in the encryption context of the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the cryptographic operation only when the encryption context in the request includes the key-value pairs specified in this constraint, although it can include additional key-value pairs.
--
-- /Note:/ Consider using 'encryptionContextSubset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEncryptionContextSubset :: Lens.Lens' GrantConstraints (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
gcEncryptionContextSubset = Lens.field @"encryptionContextSubset"
{-# DEPRECATED gcEncryptionContextSubset "Use generic-lens or generic-optics with 'encryptionContextSubset' instead." #-}

instance Core.FromJSON GrantConstraints where
  toJSON GrantConstraints {..} =
    Core.object
      ( Core.catMaybes
          [ ("EncryptionContextEquals" Core..=)
              Core.<$> encryptionContextEquals,
            ("EncryptionContextSubset" Core..=)
              Core.<$> encryptionContextSubset
          ]
      )

instance Core.FromJSON GrantConstraints where
  parseJSON =
    Core.withObject "GrantConstraints" Core.$
      \x ->
        GrantConstraints'
          Core.<$> (x Core..:? "EncryptionContextEquals")
          Core.<*> (x Core..:? "EncryptionContextSubset")
