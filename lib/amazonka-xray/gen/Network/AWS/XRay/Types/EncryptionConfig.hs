{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.EncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.EncryptionConfig
  ( EncryptionConfig (..)
  -- * Smart constructor
  , mkEncryptionConfig
  -- * Lenses
  , ecKeyId
  , ecStatus
  , ecType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.EncryptionStatus as Types
import qualified Network.AWS.XRay.Types.EncryptionType as Types

-- | A configuration document that specifies encryption configuration settings.
--
-- /See:/ 'mkEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { keyId :: Core.Maybe Core.Text
    -- ^ The ID of the customer master key (CMK) used for encryption, if applicable.
  , status :: Core.Maybe Types.EncryptionStatus
    -- ^ The encryption status. While the status is @UPDATING@ , X-Ray may encrypt data with a combination of the new and old settings.
  , type' :: Core.Maybe Types.EncryptionType
    -- ^ The type of encryption. Set to @KMS@ for encryption with CMKs. Set to @NONE@ for default encryption.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionConfig' value with any optional fields omitted.
mkEncryptionConfig
    :: EncryptionConfig
mkEncryptionConfig
  = EncryptionConfig'{keyId = Core.Nothing, status = Core.Nothing,
                      type' = Core.Nothing}

-- | The ID of the customer master key (CMK) used for encryption, if applicable.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecKeyId :: Lens.Lens' EncryptionConfig (Core.Maybe Core.Text)
ecKeyId = Lens.field @"keyId"
{-# INLINEABLE ecKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The encryption status. While the status is @UPDATING@ , X-Ray may encrypt data with a combination of the new and old settings.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecStatus :: Lens.Lens' EncryptionConfig (Core.Maybe Types.EncryptionStatus)
ecStatus = Lens.field @"status"
{-# INLINEABLE ecStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The type of encryption. Set to @KMS@ for encryption with CMKs. Set to @NONE@ for default encryption.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecType :: Lens.Lens' EncryptionConfig (Core.Maybe Types.EncryptionType)
ecType = Lens.field @"type'"
{-# INLINEABLE ecType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON EncryptionConfig where
        parseJSON
          = Core.withObject "EncryptionConfig" Core.$
              \ x ->
                EncryptionConfig' Core.<$>
                  (x Core..:? "KeyId") Core.<*> x Core..:? "Status" Core.<*>
                    x Core..:? "Type"
