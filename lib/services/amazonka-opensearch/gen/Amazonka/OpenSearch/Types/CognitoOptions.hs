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
-- Module      : Amazonka.OpenSearch.Types.CognitoOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.CognitoOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Options to specify the Cognito user and identity pools for OpenSearch
-- Dashboards authentication. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
--
-- /See:/ 'newCognitoOptions' smart constructor.
data CognitoOptions = CognitoOptions'
  { -- | The role ARN that provides OpenSearch permissions for accessing Cognito
    -- resources.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The option to enable Cognito for OpenSearch Dashboards authentication.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Cognito identity pool ID for OpenSearch Dashboards authentication.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The Cognito user pool ID for OpenSearch Dashboards authentication.
    userPoolId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CognitoOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'cognitoOptions_roleArn' - The role ARN that provides OpenSearch permissions for accessing Cognito
-- resources.
--
-- 'enabled', 'cognitoOptions_enabled' - The option to enable Cognito for OpenSearch Dashboards authentication.
--
-- 'identityPoolId', 'cognitoOptions_identityPoolId' - The Cognito identity pool ID for OpenSearch Dashboards authentication.
--
-- 'userPoolId', 'cognitoOptions_userPoolId' - The Cognito user pool ID for OpenSearch Dashboards authentication.
newCognitoOptions ::
  CognitoOptions
newCognitoOptions =
  CognitoOptions'
    { roleArn = Prelude.Nothing,
      enabled = Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      userPoolId = Prelude.Nothing
    }

-- | The role ARN that provides OpenSearch permissions for accessing Cognito
-- resources.
cognitoOptions_roleArn :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_roleArn = Lens.lens (\CognitoOptions' {roleArn} -> roleArn) (\s@CognitoOptions' {} a -> s {roleArn = a} :: CognitoOptions)

-- | The option to enable Cognito for OpenSearch Dashboards authentication.
cognitoOptions_enabled :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Bool)
cognitoOptions_enabled = Lens.lens (\CognitoOptions' {enabled} -> enabled) (\s@CognitoOptions' {} a -> s {enabled = a} :: CognitoOptions)

-- | The Cognito identity pool ID for OpenSearch Dashboards authentication.
cognitoOptions_identityPoolId :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_identityPoolId = Lens.lens (\CognitoOptions' {identityPoolId} -> identityPoolId) (\s@CognitoOptions' {} a -> s {identityPoolId = a} :: CognitoOptions)

-- | The Cognito user pool ID for OpenSearch Dashboards authentication.
cognitoOptions_userPoolId :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_userPoolId = Lens.lens (\CognitoOptions' {userPoolId} -> userPoolId) (\s@CognitoOptions' {} a -> s {userPoolId = a} :: CognitoOptions)

instance Core.FromJSON CognitoOptions where
  parseJSON =
    Core.withObject
      "CognitoOptions"
      ( \x ->
          CognitoOptions'
            Prelude.<$> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "IdentityPoolId")
            Prelude.<*> (x Core..:? "UserPoolId")
      )

instance Prelude.Hashable CognitoOptions where
  hashWithSalt _salt CognitoOptions' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData CognitoOptions where
  rnf CognitoOptions' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf userPoolId

instance Core.ToJSON CognitoOptions where
  toJSON CognitoOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("IdentityPoolId" Core..=)
              Prelude.<$> identityPoolId,
            ("UserPoolId" Core..=) Prelude.<$> userPoolId
          ]
      )
