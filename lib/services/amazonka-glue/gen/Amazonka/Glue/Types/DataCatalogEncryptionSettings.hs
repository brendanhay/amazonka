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
-- Module      : Amazonka.Glue.Types.DataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataCatalogEncryptionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ConnectionPasswordEncryption
import Amazonka.Glue.Types.EncryptionAtRest
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration information for maintaining Data Catalog
-- security.
--
-- /See:/ 'newDataCatalogEncryptionSettings' smart constructor.
data DataCatalogEncryptionSettings = DataCatalogEncryptionSettings'
  { -- | When connection password protection is enabled, the Data Catalog uses a
    -- customer-provided key to encrypt the password as part of
    -- @CreateConnection@ or @UpdateConnection@ and store it in the
    -- @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable
    -- catalog encryption or only password encryption.
    connectionPasswordEncryption :: Prelude.Maybe ConnectionPasswordEncryption,
    -- | Specifies the encryption-at-rest configuration for the Data Catalog.
    encryptionAtRest :: Prelude.Maybe EncryptionAtRest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCatalogEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionPasswordEncryption', 'dataCatalogEncryptionSettings_connectionPasswordEncryption' - When connection password protection is enabled, the Data Catalog uses a
-- customer-provided key to encrypt the password as part of
-- @CreateConnection@ or @UpdateConnection@ and store it in the
-- @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable
-- catalog encryption or only password encryption.
--
-- 'encryptionAtRest', 'dataCatalogEncryptionSettings_encryptionAtRest' - Specifies the encryption-at-rest configuration for the Data Catalog.
newDataCatalogEncryptionSettings ::
  DataCatalogEncryptionSettings
newDataCatalogEncryptionSettings =
  DataCatalogEncryptionSettings'
    { connectionPasswordEncryption =
        Prelude.Nothing,
      encryptionAtRest = Prelude.Nothing
    }

-- | When connection password protection is enabled, the Data Catalog uses a
-- customer-provided key to encrypt the password as part of
-- @CreateConnection@ or @UpdateConnection@ and store it in the
-- @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable
-- catalog encryption or only password encryption.
dataCatalogEncryptionSettings_connectionPasswordEncryption :: Lens.Lens' DataCatalogEncryptionSettings (Prelude.Maybe ConnectionPasswordEncryption)
dataCatalogEncryptionSettings_connectionPasswordEncryption = Lens.lens (\DataCatalogEncryptionSettings' {connectionPasswordEncryption} -> connectionPasswordEncryption) (\s@DataCatalogEncryptionSettings' {} a -> s {connectionPasswordEncryption = a} :: DataCatalogEncryptionSettings)

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
dataCatalogEncryptionSettings_encryptionAtRest :: Lens.Lens' DataCatalogEncryptionSettings (Prelude.Maybe EncryptionAtRest)
dataCatalogEncryptionSettings_encryptionAtRest = Lens.lens (\DataCatalogEncryptionSettings' {encryptionAtRest} -> encryptionAtRest) (\s@DataCatalogEncryptionSettings' {} a -> s {encryptionAtRest = a} :: DataCatalogEncryptionSettings)

instance Data.FromJSON DataCatalogEncryptionSettings where
  parseJSON =
    Data.withObject
      "DataCatalogEncryptionSettings"
      ( \x ->
          DataCatalogEncryptionSettings'
            Prelude.<$> (x Data..:? "ConnectionPasswordEncryption")
            Prelude.<*> (x Data..:? "EncryptionAtRest")
      )

instance
  Prelude.Hashable
    DataCatalogEncryptionSettings
  where
  hashWithSalt _salt DataCatalogEncryptionSettings' {..} =
    _salt
      `Prelude.hashWithSalt` connectionPasswordEncryption
      `Prelude.hashWithSalt` encryptionAtRest

instance Prelude.NFData DataCatalogEncryptionSettings where
  rnf DataCatalogEncryptionSettings' {..} =
    Prelude.rnf connectionPasswordEncryption
      `Prelude.seq` Prelude.rnf encryptionAtRest

instance Data.ToJSON DataCatalogEncryptionSettings where
  toJSON DataCatalogEncryptionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionPasswordEncryption" Data..=)
              Prelude.<$> connectionPasswordEncryption,
            ("EncryptionAtRest" Data..=)
              Prelude.<$> encryptionAtRest
          ]
      )
