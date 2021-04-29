{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaPackage.Types.Authorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.Authorization where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | CDN Authorization credentials
--
-- /See:/ 'newAuthorization' smart constructor.
data Authorization = Authorization'
  { -- | The Amazon Resource Name (ARN) for the IAM role that allows MediaPackage
    -- to communicate with AWS Secrets Manager.
    secretsRoleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the secret in Secrets Manager that
    -- your Content Distribution Network (CDN) uses for authorization to access
    -- your endpoint.
    cdnIdentifierSecret :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'cdnIdentifierSecret', 'authorization_cdnIdentifierSecret' - The Amazon Resource Name (ARN) for the secret in Secrets Manager that
-- your Content Distribution Network (CDN) uses for authorization to access
-- your endpoint.
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

-- | The Amazon Resource Name (ARN) for the secret in Secrets Manager that
-- your Content Distribution Network (CDN) uses for authorization to access
-- your endpoint.
authorization_cdnIdentifierSecret :: Lens.Lens' Authorization Prelude.Text
authorization_cdnIdentifierSecret = Lens.lens (\Authorization' {cdnIdentifierSecret} -> cdnIdentifierSecret) (\s@Authorization' {} a -> s {cdnIdentifierSecret = a} :: Authorization)

instance Prelude.FromJSON Authorization where
  parseJSON =
    Prelude.withObject
      "Authorization"
      ( \x ->
          Authorization'
            Prelude.<$> (x Prelude..: "secretsRoleArn")
            Prelude.<*> (x Prelude..: "cdnIdentifierSecret")
      )

instance Prelude.Hashable Authorization

instance Prelude.NFData Authorization

instance Prelude.ToJSON Authorization where
  toJSON Authorization' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("secretsRoleArn" Prelude..= secretsRoleArn),
            Prelude.Just
              ( "cdnIdentifierSecret"
                  Prelude..= cdnIdentifierSecret
              )
          ]
      )
