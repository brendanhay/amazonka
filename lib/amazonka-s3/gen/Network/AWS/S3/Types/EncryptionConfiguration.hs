-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.EncryptionConfiguration
  ( EncryptionConfiguration (..),

    -- * Smart constructor
    mkEncryptionConfiguration,

    -- * Lenses
    ecReplicaKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Specifies encryption-related information for an Amazon S3 bucket that is a destination for replicated objects.
--
-- /See:/ 'mkEncryptionConfiguration' smart constructor.
newtype EncryptionConfiguration = EncryptionConfiguration'
  { replicaKMSKeyId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- * 'replicaKMSKeyId' - Specifies the ID (Key ARN or Alias ARN) of the customer managed customer master key (CMK) stored in AWS Key Management Service (KMS) for the destination bucket. Amazon S3 uses this key to encrypt replica objects. Amazon S3 only supports symmetric customer managed CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
mkEncryptionConfiguration ::
  EncryptionConfiguration
mkEncryptionConfiguration =
  EncryptionConfiguration' {replicaKMSKeyId = Lude.Nothing}

-- | Specifies the ID (Key ARN or Alias ARN) of the customer managed customer master key (CMK) stored in AWS Key Management Service (KMS) for the destination bucket. Amazon S3 uses this key to encrypt replica objects. Amazon S3 only supports symmetric customer managed CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'replicaKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecReplicaKMSKeyId :: Lens.Lens' EncryptionConfiguration (Lude.Maybe Lude.Text)
ecReplicaKMSKeyId = Lens.lens (replicaKMSKeyId :: EncryptionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {replicaKMSKeyId = a} :: EncryptionConfiguration)
{-# DEPRECATED ecReplicaKMSKeyId "Use generic-lens or generic-optics with 'replicaKMSKeyId' instead." #-}

instance Lude.FromXML EncryptionConfiguration where
  parseXML x =
    EncryptionConfiguration' Lude.<$> (x Lude..@? "ReplicaKmsKeyID")

instance Lude.ToXML EncryptionConfiguration where
  toXML EncryptionConfiguration' {..} =
    Lude.mconcat ["ReplicaKmsKeyID" Lude.@= replicaKMSKeyId]
