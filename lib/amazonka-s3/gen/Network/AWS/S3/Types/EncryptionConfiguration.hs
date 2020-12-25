{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ecReplicaKmsKeyID,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ReplicaKmsKeyID as Types

-- | Specifies encryption-related information for an Amazon S3 bucket that is a destination for replicated objects.
--
-- /See:/ 'mkEncryptionConfiguration' smart constructor.
newtype EncryptionConfiguration = EncryptionConfiguration'
  { -- | Specifies the ID (Key ARN or Alias ARN) of the customer managed customer master key (CMK) stored in AWS Key Management Service (KMS) for the destination bucket. Amazon S3 uses this key to encrypt replica objects. Amazon S3 only supports symmetric customer managed CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
    replicaKmsKeyID :: Core.Maybe Types.ReplicaKmsKeyID
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionConfiguration' value with any optional fields omitted.
mkEncryptionConfiguration ::
  EncryptionConfiguration
mkEncryptionConfiguration =
  EncryptionConfiguration' {replicaKmsKeyID = Core.Nothing}

-- | Specifies the ID (Key ARN or Alias ARN) of the customer managed customer master key (CMK) stored in AWS Key Management Service (KMS) for the destination bucket. Amazon S3 uses this key to encrypt replica objects. Amazon S3 only supports symmetric customer managed CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'replicaKmsKeyID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecReplicaKmsKeyID :: Lens.Lens' EncryptionConfiguration (Core.Maybe Types.ReplicaKmsKeyID)
ecReplicaKmsKeyID = Lens.field @"replicaKmsKeyID"
{-# DEPRECATED ecReplicaKmsKeyID "Use generic-lens or generic-optics with 'replicaKmsKeyID' instead." #-}

instance Core.ToXML EncryptionConfiguration where
  toXML EncryptionConfiguration {..} =
    Core.toXMLNode "ReplicaKmsKeyID" Core.<$> replicaKmsKeyID

instance Core.FromXML EncryptionConfiguration where
  parseXML x =
    EncryptionConfiguration' Core.<$> (x Core..@? "ReplicaKmsKeyID")
