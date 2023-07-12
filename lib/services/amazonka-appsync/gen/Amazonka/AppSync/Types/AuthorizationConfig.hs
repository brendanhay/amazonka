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
-- Module      : Amazonka.AppSync.Types.AuthorizationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.AuthorizationConfig where

import Amazonka.AppSync.Types.AuthorizationType
import Amazonka.AppSync.Types.AwsIamConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The authorization configuration in case the HTTP endpoint requires
-- authorization.
--
-- /See:/ 'newAuthorizationConfig' smart constructor.
data AuthorizationConfig = AuthorizationConfig'
  { -- | The Identity and Access Management (IAM) settings.
    awsIamConfig :: Prelude.Maybe AwsIamConfig,
    -- | The authorization type that the HTTP endpoint requires.
    --
    -- -   __AWS_IAM__: The authorization type is Signature Version 4 (SigV4).
    authorizationType :: AuthorizationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsIamConfig', 'authorizationConfig_awsIamConfig' - The Identity and Access Management (IAM) settings.
--
-- 'authorizationType', 'authorizationConfig_authorizationType' - The authorization type that the HTTP endpoint requires.
--
-- -   __AWS_IAM__: The authorization type is Signature Version 4 (SigV4).
newAuthorizationConfig ::
  -- | 'authorizationType'
  AuthorizationType ->
  AuthorizationConfig
newAuthorizationConfig pAuthorizationType_ =
  AuthorizationConfig'
    { awsIamConfig =
        Prelude.Nothing,
      authorizationType = pAuthorizationType_
    }

-- | The Identity and Access Management (IAM) settings.
authorizationConfig_awsIamConfig :: Lens.Lens' AuthorizationConfig (Prelude.Maybe AwsIamConfig)
authorizationConfig_awsIamConfig = Lens.lens (\AuthorizationConfig' {awsIamConfig} -> awsIamConfig) (\s@AuthorizationConfig' {} a -> s {awsIamConfig = a} :: AuthorizationConfig)

-- | The authorization type that the HTTP endpoint requires.
--
-- -   __AWS_IAM__: The authorization type is Signature Version 4 (SigV4).
authorizationConfig_authorizationType :: Lens.Lens' AuthorizationConfig AuthorizationType
authorizationConfig_authorizationType = Lens.lens (\AuthorizationConfig' {authorizationType} -> authorizationType) (\s@AuthorizationConfig' {} a -> s {authorizationType = a} :: AuthorizationConfig)

instance Data.FromJSON AuthorizationConfig where
  parseJSON =
    Data.withObject
      "AuthorizationConfig"
      ( \x ->
          AuthorizationConfig'
            Prelude.<$> (x Data..:? "awsIamConfig")
            Prelude.<*> (x Data..: "authorizationType")
      )

instance Prelude.Hashable AuthorizationConfig where
  hashWithSalt _salt AuthorizationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` awsIamConfig
      `Prelude.hashWithSalt` authorizationType

instance Prelude.NFData AuthorizationConfig where
  rnf AuthorizationConfig' {..} =
    Prelude.rnf awsIamConfig
      `Prelude.seq` Prelude.rnf authorizationType

instance Data.ToJSON AuthorizationConfig where
  toJSON AuthorizationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsIamConfig" Data..=) Prelude.<$> awsIamConfig,
            Prelude.Just
              ("authorizationType" Data..= authorizationType)
          ]
      )
