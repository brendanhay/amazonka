{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DataCatalogEncryptionSettings where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.ConnectionPasswordEncryption
import Network.AWS.Glue.Types.EncryptionAtRest
import qualified Network.AWS.Lens as Lens

-- | Contains configuration information for maintaining Data Catalog
-- security.
--
-- /See:/ 'newDataCatalogEncryptionSettings' smart constructor.
data DataCatalogEncryptionSettings = DataCatalogEncryptionSettings'
  { -- | Specifies the encryption-at-rest configuration for the Data Catalog.
    encryptionAtRest :: Core.Maybe EncryptionAtRest,
    -- | When connection password protection is enabled, the Data Catalog uses a
    -- customer-provided key to encrypt the password as part of
    -- @CreateConnection@ or @UpdateConnection@ and store it in the
    -- @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable
    -- catalog encryption or only password encryption.
    connectionPasswordEncryption :: Core.Maybe ConnectionPasswordEncryption
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DataCatalogEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionAtRest', 'dataCatalogEncryptionSettings_encryptionAtRest' - Specifies the encryption-at-rest configuration for the Data Catalog.
--
-- 'connectionPasswordEncryption', 'dataCatalogEncryptionSettings_connectionPasswordEncryption' - When connection password protection is enabled, the Data Catalog uses a
-- customer-provided key to encrypt the password as part of
-- @CreateConnection@ or @UpdateConnection@ and store it in the
-- @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable
-- catalog encryption or only password encryption.
newDataCatalogEncryptionSettings ::
  DataCatalogEncryptionSettings
newDataCatalogEncryptionSettings =
  DataCatalogEncryptionSettings'
    { encryptionAtRest =
        Core.Nothing,
      connectionPasswordEncryption = Core.Nothing
    }

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
dataCatalogEncryptionSettings_encryptionAtRest :: Lens.Lens' DataCatalogEncryptionSettings (Core.Maybe EncryptionAtRest)
dataCatalogEncryptionSettings_encryptionAtRest = Lens.lens (\DataCatalogEncryptionSettings' {encryptionAtRest} -> encryptionAtRest) (\s@DataCatalogEncryptionSettings' {} a -> s {encryptionAtRest = a} :: DataCatalogEncryptionSettings)

-- | When connection password protection is enabled, the Data Catalog uses a
-- customer-provided key to encrypt the password as part of
-- @CreateConnection@ or @UpdateConnection@ and store it in the
-- @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable
-- catalog encryption or only password encryption.
dataCatalogEncryptionSettings_connectionPasswordEncryption :: Lens.Lens' DataCatalogEncryptionSettings (Core.Maybe ConnectionPasswordEncryption)
dataCatalogEncryptionSettings_connectionPasswordEncryption = Lens.lens (\DataCatalogEncryptionSettings' {connectionPasswordEncryption} -> connectionPasswordEncryption) (\s@DataCatalogEncryptionSettings' {} a -> s {connectionPasswordEncryption = a} :: DataCatalogEncryptionSettings)

instance Core.FromJSON DataCatalogEncryptionSettings where
  parseJSON =
    Core.withObject
      "DataCatalogEncryptionSettings"
      ( \x ->
          DataCatalogEncryptionSettings'
            Core.<$> (x Core..:? "EncryptionAtRest")
            Core.<*> (x Core..:? "ConnectionPasswordEncryption")
      )

instance Core.Hashable DataCatalogEncryptionSettings

instance Core.NFData DataCatalogEncryptionSettings

instance Core.ToJSON DataCatalogEncryptionSettings where
  toJSON DataCatalogEncryptionSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EncryptionAtRest" Core..=)
              Core.<$> encryptionAtRest,
            ("ConnectionPasswordEncryption" Core..=)
              Core.<$> connectionPasswordEncryption
          ]
      )
