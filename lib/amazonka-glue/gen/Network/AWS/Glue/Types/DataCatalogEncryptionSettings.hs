{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.DataCatalogEncryptionSettings
  ( DataCatalogEncryptionSettings (..)
  -- * Smart constructor
  , mkDataCatalogEncryptionSettings
  -- * Lenses
  , dcesConnectionPasswordEncryption
  , dcesEncryptionAtRest
  ) where

import qualified Network.AWS.Glue.Types.ConnectionPasswordEncryption as Types
import qualified Network.AWS.Glue.Types.EncryptionAtRest as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains configuration information for maintaining Data Catalog security.
--
-- /See:/ 'mkDataCatalogEncryptionSettings' smart constructor.
data DataCatalogEncryptionSettings = DataCatalogEncryptionSettings'
  { connectionPasswordEncryption :: Core.Maybe Types.ConnectionPasswordEncryption
    -- ^ When connection password protection is enabled, the Data Catalog uses a customer-provided key to encrypt the password as part of @CreateConnection@ or @UpdateConnection@ and store it in the @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable catalog encryption or only password encryption.
  , encryptionAtRest :: Core.Maybe Types.EncryptionAtRest
    -- ^ Specifies the encryption-at-rest configuration for the Data Catalog.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataCatalogEncryptionSettings' value with any optional fields omitted.
mkDataCatalogEncryptionSettings
    :: DataCatalogEncryptionSettings
mkDataCatalogEncryptionSettings
  = DataCatalogEncryptionSettings'{connectionPasswordEncryption =
                                     Core.Nothing,
                                   encryptionAtRest = Core.Nothing}

-- | When connection password protection is enabled, the Data Catalog uses a customer-provided key to encrypt the password as part of @CreateConnection@ or @UpdateConnection@ and store it in the @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable catalog encryption or only password encryption.
--
-- /Note:/ Consider using 'connectionPasswordEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcesConnectionPasswordEncryption :: Lens.Lens' DataCatalogEncryptionSettings (Core.Maybe Types.ConnectionPasswordEncryption)
dcesConnectionPasswordEncryption = Lens.field @"connectionPasswordEncryption"
{-# INLINEABLE dcesConnectionPasswordEncryption #-}
{-# DEPRECATED connectionPasswordEncryption "Use generic-lens or generic-optics with 'connectionPasswordEncryption' instead"  #-}

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
--
-- /Note:/ Consider using 'encryptionAtRest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcesEncryptionAtRest :: Lens.Lens' DataCatalogEncryptionSettings (Core.Maybe Types.EncryptionAtRest)
dcesEncryptionAtRest = Lens.field @"encryptionAtRest"
{-# INLINEABLE dcesEncryptionAtRest #-}
{-# DEPRECATED encryptionAtRest "Use generic-lens or generic-optics with 'encryptionAtRest' instead"  #-}

instance Core.FromJSON DataCatalogEncryptionSettings where
        toJSON DataCatalogEncryptionSettings{..}
          = Core.object
              (Core.catMaybes
                 [("ConnectionPasswordEncryption" Core..=) Core.<$>
                    connectionPasswordEncryption,
                  ("EncryptionAtRest" Core..=) Core.<$> encryptionAtRest])

instance Core.FromJSON DataCatalogEncryptionSettings where
        parseJSON
          = Core.withObject "DataCatalogEncryptionSettings" Core.$
              \ x ->
                DataCatalogEncryptionSettings' Core.<$>
                  (x Core..:? "ConnectionPasswordEncryption") Core.<*>
                    x Core..:? "EncryptionAtRest"
