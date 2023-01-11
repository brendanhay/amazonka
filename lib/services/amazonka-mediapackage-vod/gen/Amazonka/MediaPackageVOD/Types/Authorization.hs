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
-- Module      : Amazonka.MediaPackageVOD.Types.Authorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.Authorization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | CDN Authorization credentials
--
-- /See:/ 'newAuthorization' smart constructor.
data Authorization = Authorization'
  { -- | The Amazon Resource Name (ARN) for the IAM role that allows MediaPackage
    -- to communicate with AWS Secrets Manager.
    secretsRoleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the secret in AWS Secrets Manager
    -- that is used for CDN authorization.
    cdnIdentifierSecret :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Authorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretsRoleArn', 'authorization_secretsRoleArn' - The Amazon Resource Name (ARN) for the IAM role that allows MediaPackage
-- to communicate with AWS Secrets Manager.
--
-- 'cdnIdentifierSecret', 'authorization_cdnIdentifierSecret' - The Amazon Resource Name (ARN) for the secret in AWS Secrets Manager
-- that is used for CDN authorization.
newAuthorization ::
  -- | 'secretsRoleArn'
  Prelude.Text ->
  -- | 'cdnIdentifierSecret'
  Prelude.Text ->
  Authorization
newAuthorization
  pSecretsRoleArn_
  pCdnIdentifierSecret_ =
    Authorization'
      { secretsRoleArn = pSecretsRoleArn_,
        cdnIdentifierSecret = pCdnIdentifierSecret_
      }

-- | The Amazon Resource Name (ARN) for the IAM role that allows MediaPackage
-- to communicate with AWS Secrets Manager.
authorization_secretsRoleArn :: Lens.Lens' Authorization Prelude.Text
authorization_secretsRoleArn = Lens.lens (\Authorization' {secretsRoleArn} -> secretsRoleArn) (\s@Authorization' {} a -> s {secretsRoleArn = a} :: Authorization)

-- | The Amazon Resource Name (ARN) for the secret in AWS Secrets Manager
-- that is used for CDN authorization.
authorization_cdnIdentifierSecret :: Lens.Lens' Authorization Prelude.Text
authorization_cdnIdentifierSecret = Lens.lens (\Authorization' {cdnIdentifierSecret} -> cdnIdentifierSecret) (\s@Authorization' {} a -> s {cdnIdentifierSecret = a} :: Authorization)

instance Data.FromJSON Authorization where
  parseJSON =
    Data.withObject
      "Authorization"
      ( \x ->
          Authorization'
            Prelude.<$> (x Data..: "secretsRoleArn")
            Prelude.<*> (x Data..: "cdnIdentifierSecret")
      )

instance Prelude.Hashable Authorization where
  hashWithSalt _salt Authorization' {..} =
    _salt `Prelude.hashWithSalt` secretsRoleArn
      `Prelude.hashWithSalt` cdnIdentifierSecret

instance Prelude.NFData Authorization where
  rnf Authorization' {..} =
    Prelude.rnf secretsRoleArn
      `Prelude.seq` Prelude.rnf cdnIdentifierSecret

instance Data.ToJSON Authorization where
  toJSON Authorization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("secretsRoleArn" Data..= secretsRoleArn),
            Prelude.Just
              ("cdnIdentifierSecret" Data..= cdnIdentifierSecret)
          ]
      )
