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
-- Module      : Amazonka.ElasticSearch.Types.CognitoOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.CognitoOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Options to specify the Cognito user and identity pools for Kibana
-- authentication. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
--
-- /See:/ 'newCognitoOptions' smart constructor.
data CognitoOptions = CognitoOptions'
  { -- | Specifies the role ARN that provides Elasticsearch permissions for
    -- accessing Cognito resources.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the option to enable Cognito for Kibana authentication.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the Cognito identity pool ID for Kibana authentication.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Cognito user pool ID for Kibana authentication.
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
-- 'roleArn', 'cognitoOptions_roleArn' - Specifies the role ARN that provides Elasticsearch permissions for
-- accessing Cognito resources.
--
-- 'enabled', 'cognitoOptions_enabled' - Specifies the option to enable Cognito for Kibana authentication.
--
-- 'identityPoolId', 'cognitoOptions_identityPoolId' - Specifies the Cognito identity pool ID for Kibana authentication.
--
-- 'userPoolId', 'cognitoOptions_userPoolId' - Specifies the Cognito user pool ID for Kibana authentication.
newCognitoOptions ::
  CognitoOptions
newCognitoOptions =
  CognitoOptions'
    { roleArn = Prelude.Nothing,
      enabled = Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      userPoolId = Prelude.Nothing
    }

-- | Specifies the role ARN that provides Elasticsearch permissions for
-- accessing Cognito resources.
cognitoOptions_roleArn :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_roleArn = Lens.lens (\CognitoOptions' {roleArn} -> roleArn) (\s@CognitoOptions' {} a -> s {roleArn = a} :: CognitoOptions)

-- | Specifies the option to enable Cognito for Kibana authentication.
cognitoOptions_enabled :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Bool)
cognitoOptions_enabled = Lens.lens (\CognitoOptions' {enabled} -> enabled) (\s@CognitoOptions' {} a -> s {enabled = a} :: CognitoOptions)

-- | Specifies the Cognito identity pool ID for Kibana authentication.
cognitoOptions_identityPoolId :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_identityPoolId = Lens.lens (\CognitoOptions' {identityPoolId} -> identityPoolId) (\s@CognitoOptions' {} a -> s {identityPoolId = a} :: CognitoOptions)

-- | Specifies the Cognito user pool ID for Kibana authentication.
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
