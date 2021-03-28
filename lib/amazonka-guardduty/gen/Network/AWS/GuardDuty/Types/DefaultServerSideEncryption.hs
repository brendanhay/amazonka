{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
  ( DefaultServerSideEncryption (..)
  -- * Smart constructor
  , mkDefaultServerSideEncryption
  -- * Lenses
  , dsseEncryptionType
  , dsseKmsMasterKeyArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the server side encryption method used in the S3 bucket. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html S3 Server-Side Encryption> for more information.
--
-- /See:/ 'mkDefaultServerSideEncryption' smart constructor.
data DefaultServerSideEncryption = DefaultServerSideEncryption'
  { encryptionType :: Core.Maybe Core.Text
    -- ^ The type of encryption used for objects within the S3 bucket.
  , kmsMasterKeyArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the KMS encryption key. Only available if the bucket @EncryptionType@ is @aws:kms@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefaultServerSideEncryption' value with any optional fields omitted.
mkDefaultServerSideEncryption
    :: DefaultServerSideEncryption
mkDefaultServerSideEncryption
  = DefaultServerSideEncryption'{encryptionType = Core.Nothing,
                                 kmsMasterKeyArn = Core.Nothing}

-- | The type of encryption used for objects within the S3 bucket.
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsseEncryptionType :: Lens.Lens' DefaultServerSideEncryption (Core.Maybe Core.Text)
dsseEncryptionType = Lens.field @"encryptionType"
{-# INLINEABLE dsseEncryptionType #-}
{-# DEPRECATED encryptionType "Use generic-lens or generic-optics with 'encryptionType' instead"  #-}

-- | The Amazon Resource Name (ARN) of the KMS encryption key. Only available if the bucket @EncryptionType@ is @aws:kms@ .
--
-- /Note:/ Consider using 'kmsMasterKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsseKmsMasterKeyArn :: Lens.Lens' DefaultServerSideEncryption (Core.Maybe Core.Text)
dsseKmsMasterKeyArn = Lens.field @"kmsMasterKeyArn"
{-# INLINEABLE dsseKmsMasterKeyArn #-}
{-# DEPRECATED kmsMasterKeyArn "Use generic-lens or generic-optics with 'kmsMasterKeyArn' instead"  #-}

instance Core.FromJSON DefaultServerSideEncryption where
        parseJSON
          = Core.withObject "DefaultServerSideEncryption" Core.$
              \ x ->
                DefaultServerSideEncryption' Core.<$>
                  (x Core..:? "encryptionType") Core.<*> x Core..:? "kmsMasterKeyArn"
