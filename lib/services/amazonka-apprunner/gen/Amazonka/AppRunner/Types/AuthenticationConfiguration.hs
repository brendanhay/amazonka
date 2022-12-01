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
-- Module      : Amazonka.AppRunner.Types.AuthenticationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.AuthenticationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes resources needed to authenticate access to some source
-- repositories. The specific resource depends on the repository provider.
--
-- /See:/ 'newAuthenticationConfiguration' smart constructor.
data AuthenticationConfiguration = AuthenticationConfiguration'
  { -- | The Amazon Resource Name (ARN) of the IAM role that grants the App
    -- Runner service access to a source repository. It\'s required for ECR
    -- image repositories (but not for ECR Public repositories).
    accessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the App Runner connection that enables
    -- the App Runner service to connect to a source repository. It\'s required
    -- for GitHub code repositories.
    connectionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessRoleArn', 'authenticationConfiguration_accessRoleArn' - The Amazon Resource Name (ARN) of the IAM role that grants the App
-- Runner service access to a source repository. It\'s required for ECR
-- image repositories (but not for ECR Public repositories).
--
-- 'connectionArn', 'authenticationConfiguration_connectionArn' - The Amazon Resource Name (ARN) of the App Runner connection that enables
-- the App Runner service to connect to a source repository. It\'s required
-- for GitHub code repositories.
newAuthenticationConfiguration ::
  AuthenticationConfiguration
newAuthenticationConfiguration =
  AuthenticationConfiguration'
    { accessRoleArn =
        Prelude.Nothing,
      connectionArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role that grants the App
-- Runner service access to a source repository. It\'s required for ECR
-- image repositories (but not for ECR Public repositories).
authenticationConfiguration_accessRoleArn :: Lens.Lens' AuthenticationConfiguration (Prelude.Maybe Prelude.Text)
authenticationConfiguration_accessRoleArn = Lens.lens (\AuthenticationConfiguration' {accessRoleArn} -> accessRoleArn) (\s@AuthenticationConfiguration' {} a -> s {accessRoleArn = a} :: AuthenticationConfiguration)

-- | The Amazon Resource Name (ARN) of the App Runner connection that enables
-- the App Runner service to connect to a source repository. It\'s required
-- for GitHub code repositories.
authenticationConfiguration_connectionArn :: Lens.Lens' AuthenticationConfiguration (Prelude.Maybe Prelude.Text)
authenticationConfiguration_connectionArn = Lens.lens (\AuthenticationConfiguration' {connectionArn} -> connectionArn) (\s@AuthenticationConfiguration' {} a -> s {connectionArn = a} :: AuthenticationConfiguration)

instance Core.FromJSON AuthenticationConfiguration where
  parseJSON =
    Core.withObject
      "AuthenticationConfiguration"
      ( \x ->
          AuthenticationConfiguration'
            Prelude.<$> (x Core..:? "AccessRoleArn")
            Prelude.<*> (x Core..:? "ConnectionArn")
      )

instance Prelude.Hashable AuthenticationConfiguration where
  hashWithSalt _salt AuthenticationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` accessRoleArn
      `Prelude.hashWithSalt` connectionArn

instance Prelude.NFData AuthenticationConfiguration where
  rnf AuthenticationConfiguration' {..} =
    Prelude.rnf accessRoleArn
      `Prelude.seq` Prelude.rnf connectionArn

instance Core.ToJSON AuthenticationConfiguration where
  toJSON AuthenticationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccessRoleArn" Core..=) Prelude.<$> accessRoleArn,
            ("ConnectionArn" Core..=) Prelude.<$> connectionArn
          ]
      )
