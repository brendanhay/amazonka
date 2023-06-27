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
-- Module      : Amazonka.SSMSAP.Types.ApplicationCredential
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.ApplicationCredential where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.CredentialType

-- | The credentials of your SAP application.
--
-- /See:/ 'newApplicationCredential' smart constructor.
data ApplicationCredential = ApplicationCredential'
  { -- | The name of the SAP HANA database.
    databaseName :: Prelude.Text,
    -- | The type of the application credentials.
    credentialType :: CredentialType,
    -- | The secret ID created in AWS Secrets Manager to store the credentials of
    -- the SAP application.
    secretId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationCredential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'applicationCredential_databaseName' - The name of the SAP HANA database.
--
-- 'credentialType', 'applicationCredential_credentialType' - The type of the application credentials.
--
-- 'secretId', 'applicationCredential_secretId' - The secret ID created in AWS Secrets Manager to store the credentials of
-- the SAP application.
newApplicationCredential ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'credentialType'
  CredentialType ->
  -- | 'secretId'
  Prelude.Text ->
  ApplicationCredential
newApplicationCredential
  pDatabaseName_
  pCredentialType_
  pSecretId_ =
    ApplicationCredential'
      { databaseName =
          pDatabaseName_,
        credentialType = pCredentialType_,
        secretId = Data._Sensitive Lens.# pSecretId_
      }

-- | The name of the SAP HANA database.
applicationCredential_databaseName :: Lens.Lens' ApplicationCredential Prelude.Text
applicationCredential_databaseName = Lens.lens (\ApplicationCredential' {databaseName} -> databaseName) (\s@ApplicationCredential' {} a -> s {databaseName = a} :: ApplicationCredential)

-- | The type of the application credentials.
applicationCredential_credentialType :: Lens.Lens' ApplicationCredential CredentialType
applicationCredential_credentialType = Lens.lens (\ApplicationCredential' {credentialType} -> credentialType) (\s@ApplicationCredential' {} a -> s {credentialType = a} :: ApplicationCredential)

-- | The secret ID created in AWS Secrets Manager to store the credentials of
-- the SAP application.
applicationCredential_secretId :: Lens.Lens' ApplicationCredential Prelude.Text
applicationCredential_secretId = Lens.lens (\ApplicationCredential' {secretId} -> secretId) (\s@ApplicationCredential' {} a -> s {secretId = a} :: ApplicationCredential) Prelude.. Data._Sensitive

instance Data.FromJSON ApplicationCredential where
  parseJSON =
    Data.withObject
      "ApplicationCredential"
      ( \x ->
          ApplicationCredential'
            Prelude.<$> (x Data..: "DatabaseName")
            Prelude.<*> (x Data..: "CredentialType")
            Prelude.<*> (x Data..: "SecretId")
      )

instance Prelude.Hashable ApplicationCredential where
  hashWithSalt _salt ApplicationCredential' {..} =
    _salt
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` credentialType
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData ApplicationCredential where
  rnf ApplicationCredential' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf credentialType
      `Prelude.seq` Prelude.rnf secretId

instance Data.ToJSON ApplicationCredential where
  toJSON ApplicationCredential' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just
              ("CredentialType" Data..= credentialType),
            Prelude.Just ("SecretId" Data..= secretId)
          ]
      )
