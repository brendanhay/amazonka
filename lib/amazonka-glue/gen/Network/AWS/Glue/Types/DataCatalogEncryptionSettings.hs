{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DataCatalogEncryptionSettings
  ( DataCatalogEncryptionSettings (..),

    -- * Smart constructor
    mkDataCatalogEncryptionSettings,

    -- * Lenses
    dcesEncryptionAtRest,
    dcesConnectionPasswordEncryption,
  )
where

import Network.AWS.Glue.Types.ConnectionPasswordEncryption
import Network.AWS.Glue.Types.EncryptionAtRest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains configuration information for maintaining Data Catalog security.
--
-- /See:/ 'mkDataCatalogEncryptionSettings' smart constructor.
data DataCatalogEncryptionSettings = DataCatalogEncryptionSettings'
  { encryptionAtRest ::
      Lude.Maybe EncryptionAtRest,
    connectionPasswordEncryption ::
      Lude.Maybe
        ConnectionPasswordEncryption
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataCatalogEncryptionSettings' with the minimum fields required to make a request.
--
-- * 'connectionPasswordEncryption' - When connection password protection is enabled, the Data Catalog uses a customer-provided key to encrypt the password as part of @CreateConnection@ or @UpdateConnection@ and store it in the @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable catalog encryption or only password encryption.
-- * 'encryptionAtRest' - Specifies the encryption-at-rest configuration for the Data Catalog.
mkDataCatalogEncryptionSettings ::
  DataCatalogEncryptionSettings
mkDataCatalogEncryptionSettings =
  DataCatalogEncryptionSettings'
    { encryptionAtRest = Lude.Nothing,
      connectionPasswordEncryption = Lude.Nothing
    }

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
--
-- /Note:/ Consider using 'encryptionAtRest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcesEncryptionAtRest :: Lens.Lens' DataCatalogEncryptionSettings (Lude.Maybe EncryptionAtRest)
dcesEncryptionAtRest = Lens.lens (encryptionAtRest :: DataCatalogEncryptionSettings -> Lude.Maybe EncryptionAtRest) (\s a -> s {encryptionAtRest = a} :: DataCatalogEncryptionSettings)
{-# DEPRECATED dcesEncryptionAtRest "Use generic-lens or generic-optics with 'encryptionAtRest' instead." #-}

-- | When connection password protection is enabled, the Data Catalog uses a customer-provided key to encrypt the password as part of @CreateConnection@ or @UpdateConnection@ and store it in the @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable catalog encryption or only password encryption.
--
-- /Note:/ Consider using 'connectionPasswordEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcesConnectionPasswordEncryption :: Lens.Lens' DataCatalogEncryptionSettings (Lude.Maybe ConnectionPasswordEncryption)
dcesConnectionPasswordEncryption = Lens.lens (connectionPasswordEncryption :: DataCatalogEncryptionSettings -> Lude.Maybe ConnectionPasswordEncryption) (\s a -> s {connectionPasswordEncryption = a} :: DataCatalogEncryptionSettings)
{-# DEPRECATED dcesConnectionPasswordEncryption "Use generic-lens or generic-optics with 'connectionPasswordEncryption' instead." #-}

instance Lude.FromJSON DataCatalogEncryptionSettings where
  parseJSON =
    Lude.withObject
      "DataCatalogEncryptionSettings"
      ( \x ->
          DataCatalogEncryptionSettings'
            Lude.<$> (x Lude..:? "EncryptionAtRest")
            Lude.<*> (x Lude..:? "ConnectionPasswordEncryption")
      )

instance Lude.ToJSON DataCatalogEncryptionSettings where
  toJSON DataCatalogEncryptionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EncryptionAtRest" Lude..=) Lude.<$> encryptionAtRest,
            ("ConnectionPasswordEncryption" Lude..=)
              Lude.<$> connectionPasswordEncryption
          ]
      )
