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
-- Module      : Network.AWS.AppSync.Types.AuthorizationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AuthorizationConfig where

import Network.AWS.AppSync.Types.AuthorizationType
import Network.AWS.AppSync.Types.AwsIamConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The authorization config in case the HTTP endpoint requires
-- authorization.
--
-- /See:/ 'newAuthorizationConfig' smart constructor.
data AuthorizationConfig = AuthorizationConfig'
  { -- | The AWS IAM settings.
    awsIamConfig :: Core.Maybe AwsIamConfig,
    -- | The authorization type required by the HTTP endpoint.
    --
    -- -   __AWS_IAM__: The authorization type is Sigv4.
    authorizationType :: AuthorizationType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthorizationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsIamConfig', 'authorizationConfig_awsIamConfig' - The AWS IAM settings.
--
-- 'authorizationType', 'authorizationConfig_authorizationType' - The authorization type required by the HTTP endpoint.
--
-- -   __AWS_IAM__: The authorization type is Sigv4.
newAuthorizationConfig ::
  -- | 'authorizationType'
  AuthorizationType ->
  AuthorizationConfig
newAuthorizationConfig pAuthorizationType_ =
  AuthorizationConfig'
    { awsIamConfig = Core.Nothing,
      authorizationType = pAuthorizationType_
    }

-- | The AWS IAM settings.
authorizationConfig_awsIamConfig :: Lens.Lens' AuthorizationConfig (Core.Maybe AwsIamConfig)
authorizationConfig_awsIamConfig = Lens.lens (\AuthorizationConfig' {awsIamConfig} -> awsIamConfig) (\s@AuthorizationConfig' {} a -> s {awsIamConfig = a} :: AuthorizationConfig)

-- | The authorization type required by the HTTP endpoint.
--
-- -   __AWS_IAM__: The authorization type is Sigv4.
authorizationConfig_authorizationType :: Lens.Lens' AuthorizationConfig AuthorizationType
authorizationConfig_authorizationType = Lens.lens (\AuthorizationConfig' {authorizationType} -> authorizationType) (\s@AuthorizationConfig' {} a -> s {authorizationType = a} :: AuthorizationConfig)

instance Core.FromJSON AuthorizationConfig where
  parseJSON =
    Core.withObject
      "AuthorizationConfig"
      ( \x ->
          AuthorizationConfig'
            Core.<$> (x Core..:? "awsIamConfig")
            Core.<*> (x Core..: "authorizationType")
      )

instance Core.Hashable AuthorizationConfig

instance Core.NFData AuthorizationConfig

instance Core.ToJSON AuthorizationConfig where
  toJSON AuthorizationConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("awsIamConfig" Core..=) Core.<$> awsIamConfig,
            Core.Just
              ("authorizationType" Core..= authorizationType)
          ]
      )
