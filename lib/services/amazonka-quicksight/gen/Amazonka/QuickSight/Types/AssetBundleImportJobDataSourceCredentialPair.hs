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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobDataSourceCredentialPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobDataSourceCredentialPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A username and password credential pair to use to import a data source
-- resource.
--
-- /See:/ 'newAssetBundleImportJobDataSourceCredentialPair' smart constructor.
data AssetBundleImportJobDataSourceCredentialPair = AssetBundleImportJobDataSourceCredentialPair'
  { -- | The username for the data source connection.
    username :: Prelude.Text,
    -- | The password for the data source connection.
    password :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobDataSourceCredentialPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'assetBundleImportJobDataSourceCredentialPair_username' - The username for the data source connection.
--
-- 'password', 'assetBundleImportJobDataSourceCredentialPair_password' - The password for the data source connection.
newAssetBundleImportJobDataSourceCredentialPair ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  AssetBundleImportJobDataSourceCredentialPair
newAssetBundleImportJobDataSourceCredentialPair
  pUsername_
  pPassword_ =
    AssetBundleImportJobDataSourceCredentialPair'
      { username =
          pUsername_,
        password = pPassword_
      }

-- | The username for the data source connection.
assetBundleImportJobDataSourceCredentialPair_username :: Lens.Lens' AssetBundleImportJobDataSourceCredentialPair Prelude.Text
assetBundleImportJobDataSourceCredentialPair_username = Lens.lens (\AssetBundleImportJobDataSourceCredentialPair' {username} -> username) (\s@AssetBundleImportJobDataSourceCredentialPair' {} a -> s {username = a} :: AssetBundleImportJobDataSourceCredentialPair)

-- | The password for the data source connection.
assetBundleImportJobDataSourceCredentialPair_password :: Lens.Lens' AssetBundleImportJobDataSourceCredentialPair Prelude.Text
assetBundleImportJobDataSourceCredentialPair_password = Lens.lens (\AssetBundleImportJobDataSourceCredentialPair' {password} -> password) (\s@AssetBundleImportJobDataSourceCredentialPair' {} a -> s {password = a} :: AssetBundleImportJobDataSourceCredentialPair)

instance
  Data.FromJSON
    AssetBundleImportJobDataSourceCredentialPair
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobDataSourceCredentialPair"
      ( \x ->
          AssetBundleImportJobDataSourceCredentialPair'
            Prelude.<$> (x Data..: "Username")
            Prelude.<*> (x Data..: "Password")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobDataSourceCredentialPair
  where
  hashWithSalt
    _salt
    AssetBundleImportJobDataSourceCredentialPair' {..} =
      _salt
        `Prelude.hashWithSalt` username
        `Prelude.hashWithSalt` password

instance
  Prelude.NFData
    AssetBundleImportJobDataSourceCredentialPair
  where
  rnf AssetBundleImportJobDataSourceCredentialPair' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance
  Data.ToJSON
    AssetBundleImportJobDataSourceCredentialPair
  where
  toJSON
    AssetBundleImportJobDataSourceCredentialPair' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("Username" Data..= username),
              Prelude.Just ("Password" Data..= password)
            ]
        )
