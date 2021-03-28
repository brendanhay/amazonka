{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MLUserDataEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.MLUserDataEncryption
  ( MLUserDataEncryption (..)
  -- * Smart constructor
  , mkMLUserDataEncryption
  -- * Lenses
  , mludeMlUserDataEncryptionMode
  , mludeKmsKeyId
  ) where

import qualified Network.AWS.Glue.Types.KmsKeyId as Types
import qualified Network.AWS.Glue.Types.MLUserDataEncryptionModeString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The encryption-at-rest settings of the transform that apply to accessing user data.
--
-- /See:/ 'mkMLUserDataEncryption' smart constructor.
data MLUserDataEncryption = MLUserDataEncryption'
  { mlUserDataEncryptionMode :: Types.MLUserDataEncryptionModeString
    -- ^ The encryption mode applied to user data. Valid values are:
--
--
--     * DISABLED: encryption is disabled
--
--
--     * SSEKMS: use of server-side encryption with AWS Key Management Service (SSE-KMS) for user data stored in Amazon S3.
--
--
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The ID for the customer-provided KMS key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MLUserDataEncryption' value with any optional fields omitted.
mkMLUserDataEncryption
    :: Types.MLUserDataEncryptionModeString -- ^ 'mlUserDataEncryptionMode'
    -> MLUserDataEncryption
mkMLUserDataEncryption mlUserDataEncryptionMode
  = MLUserDataEncryption'{mlUserDataEncryptionMode,
                          kmsKeyId = Core.Nothing}

-- | The encryption mode applied to user data. Valid values are:
--
--
--     * DISABLED: encryption is disabled
--
--
--     * SSEKMS: use of server-side encryption with AWS Key Management Service (SSE-KMS) for user data stored in Amazon S3.
--
--
--
-- /Note:/ Consider using 'mlUserDataEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mludeMlUserDataEncryptionMode :: Lens.Lens' MLUserDataEncryption Types.MLUserDataEncryptionModeString
mludeMlUserDataEncryptionMode = Lens.field @"mlUserDataEncryptionMode"
{-# INLINEABLE mludeMlUserDataEncryptionMode #-}
{-# DEPRECATED mlUserDataEncryptionMode "Use generic-lens or generic-optics with 'mlUserDataEncryptionMode' instead"  #-}

-- | The ID for the customer-provided KMS key.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mludeKmsKeyId :: Lens.Lens' MLUserDataEncryption (Core.Maybe Types.KmsKeyId)
mludeKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE mludeKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

instance Core.FromJSON MLUserDataEncryption where
        toJSON MLUserDataEncryption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("MlUserDataEncryptionMode" Core..= mlUserDataEncryptionMode),
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId])

instance Core.FromJSON MLUserDataEncryption where
        parseJSON
          = Core.withObject "MLUserDataEncryption" Core.$
              \ x ->
                MLUserDataEncryption' Core.<$>
                  (x Core..: "MlUserDataEncryptionMode") Core.<*>
                    x Core..:? "KmsKeyId"
