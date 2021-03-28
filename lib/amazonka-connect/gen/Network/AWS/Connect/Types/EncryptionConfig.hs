{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.EncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.EncryptionConfig
  ( EncryptionConfig (..)
  -- * Smart constructor
  , mkEncryptionConfig
  -- * Lenses
  , ecEncryptionType
  , ecKeyId
  ) where

import qualified Network.AWS.Connect.Types.EncryptionType as Types
import qualified Network.AWS.Connect.Types.KeyId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The encryption configuration.
--
-- /See:/ 'mkEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { encryptionType :: Types.EncryptionType
    -- ^ The type of encryption.
  , keyId :: Types.KeyId
    -- ^ The identifier of the encryption key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionConfig' value with any optional fields omitted.
mkEncryptionConfig
    :: Types.EncryptionType -- ^ 'encryptionType'
    -> Types.KeyId -- ^ 'keyId'
    -> EncryptionConfig
mkEncryptionConfig encryptionType keyId
  = EncryptionConfig'{encryptionType, keyId}

-- | The type of encryption.
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEncryptionType :: Lens.Lens' EncryptionConfig Types.EncryptionType
ecEncryptionType = Lens.field @"encryptionType"
{-# INLINEABLE ecEncryptionType #-}
{-# DEPRECATED encryptionType "Use generic-lens or generic-optics with 'encryptionType' instead"  #-}

-- | The identifier of the encryption key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecKeyId :: Lens.Lens' EncryptionConfig Types.KeyId
ecKeyId = Lens.field @"keyId"
{-# INLINEABLE ecKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.FromJSON EncryptionConfig where
        toJSON EncryptionConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EncryptionType" Core..= encryptionType),
                  Core.Just ("KeyId" Core..= keyId)])

instance Core.FromJSON EncryptionConfig where
        parseJSON
          = Core.withObject "EncryptionConfig" Core.$
              \ x ->
                EncryptionConfig' Core.<$>
                  (x Core..: "EncryptionType") Core.<*> x Core..: "KeyId"
