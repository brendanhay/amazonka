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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobDataSourceCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobDataSourceCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleImportJobDataSourceCredentialPair

-- | The login credentials to use to import a data source resource.
--
-- /See:/ 'newAssetBundleImportJobDataSourceCredentials' smart constructor.
data AssetBundleImportJobDataSourceCredentials = AssetBundleImportJobDataSourceCredentials'
  { -- | A username and password credential pair to be used to create the
    -- imported data source. Keep this field blank if you are using a Secrets
    -- Manager secret to provide credentials.
    credentialPair :: Prelude.Maybe (Data.Sensitive AssetBundleImportJobDataSourceCredentialPair),
    -- | The ARN of the Secrets Manager secret that\'s used to create the
    -- imported data source. Keep this field blank, unless you are using a
    -- secret in place of a credential pair.
    secretArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobDataSourceCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentialPair', 'assetBundleImportJobDataSourceCredentials_credentialPair' - A username and password credential pair to be used to create the
-- imported data source. Keep this field blank if you are using a Secrets
-- Manager secret to provide credentials.
--
-- 'secretArn', 'assetBundleImportJobDataSourceCredentials_secretArn' - The ARN of the Secrets Manager secret that\'s used to create the
-- imported data source. Keep this field blank, unless you are using a
-- secret in place of a credential pair.
newAssetBundleImportJobDataSourceCredentials ::
  AssetBundleImportJobDataSourceCredentials
newAssetBundleImportJobDataSourceCredentials =
  AssetBundleImportJobDataSourceCredentials'
    { credentialPair =
        Prelude.Nothing,
      secretArn = Prelude.Nothing
    }

-- | A username and password credential pair to be used to create the
-- imported data source. Keep this field blank if you are using a Secrets
-- Manager secret to provide credentials.
assetBundleImportJobDataSourceCredentials_credentialPair :: Lens.Lens' AssetBundleImportJobDataSourceCredentials (Prelude.Maybe AssetBundleImportJobDataSourceCredentialPair)
assetBundleImportJobDataSourceCredentials_credentialPair = Lens.lens (\AssetBundleImportJobDataSourceCredentials' {credentialPair} -> credentialPair) (\s@AssetBundleImportJobDataSourceCredentials' {} a -> s {credentialPair = a} :: AssetBundleImportJobDataSourceCredentials) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the Secrets Manager secret that\'s used to create the
-- imported data source. Keep this field blank, unless you are using a
-- secret in place of a credential pair.
assetBundleImportJobDataSourceCredentials_secretArn :: Lens.Lens' AssetBundleImportJobDataSourceCredentials (Prelude.Maybe Prelude.Text)
assetBundleImportJobDataSourceCredentials_secretArn = Lens.lens (\AssetBundleImportJobDataSourceCredentials' {secretArn} -> secretArn) (\s@AssetBundleImportJobDataSourceCredentials' {} a -> s {secretArn = a} :: AssetBundleImportJobDataSourceCredentials)

instance
  Data.FromJSON
    AssetBundleImportJobDataSourceCredentials
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobDataSourceCredentials"
      ( \x ->
          AssetBundleImportJobDataSourceCredentials'
            Prelude.<$> (x Data..:? "CredentialPair")
            Prelude.<*> (x Data..:? "SecretArn")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobDataSourceCredentials
  where
  hashWithSalt
    _salt
    AssetBundleImportJobDataSourceCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` credentialPair
        `Prelude.hashWithSalt` secretArn

instance
  Prelude.NFData
    AssetBundleImportJobDataSourceCredentials
  where
  rnf AssetBundleImportJobDataSourceCredentials' {..} =
    Prelude.rnf credentialPair
      `Prelude.seq` Prelude.rnf secretArn

instance
  Data.ToJSON
    AssetBundleImportJobDataSourceCredentials
  where
  toJSON AssetBundleImportJobDataSourceCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CredentialPair" Data..=)
              Prelude.<$> credentialPair,
            ("SecretArn" Data..=) Prelude.<$> secretArn
          ]
      )
