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
-- Module      : Network.AWS.ElasticSearch.Types.CognitoOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.CognitoOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options to specify the Cognito user and identity pools for Kibana
-- authentication. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
--
-- /See:/ 'newCognitoOptions' smart constructor.
data CognitoOptions = CognitoOptions'
  { -- | Specifies the Cognito identity pool ID for Kibana authentication.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the role ARN that provides Elasticsearch permissions for
    -- accessing Cognito resources.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Cognito user pool ID for Kibana authentication.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the option to enable Cognito for Kibana authentication.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CognitoOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'cognitoOptions_identityPoolId' - Specifies the Cognito identity pool ID for Kibana authentication.
--
-- 'roleArn', 'cognitoOptions_roleArn' - Specifies the role ARN that provides Elasticsearch permissions for
-- accessing Cognito resources.
--
-- 'userPoolId', 'cognitoOptions_userPoolId' - Specifies the Cognito user pool ID for Kibana authentication.
--
-- 'enabled', 'cognitoOptions_enabled' - Specifies the option to enable Cognito for Kibana authentication.
newCognitoOptions ::
  CognitoOptions
newCognitoOptions =
  CognitoOptions'
    { identityPoolId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | Specifies the Cognito identity pool ID for Kibana authentication.
cognitoOptions_identityPoolId :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_identityPoolId = Lens.lens (\CognitoOptions' {identityPoolId} -> identityPoolId) (\s@CognitoOptions' {} a -> s {identityPoolId = a} :: CognitoOptions)

-- | Specifies the role ARN that provides Elasticsearch permissions for
-- accessing Cognito resources.
cognitoOptions_roleArn :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_roleArn = Lens.lens (\CognitoOptions' {roleArn} -> roleArn) (\s@CognitoOptions' {} a -> s {roleArn = a} :: CognitoOptions)

-- | Specifies the Cognito user pool ID for Kibana authentication.
cognitoOptions_userPoolId :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_userPoolId = Lens.lens (\CognitoOptions' {userPoolId} -> userPoolId) (\s@CognitoOptions' {} a -> s {userPoolId = a} :: CognitoOptions)

-- | Specifies the option to enable Cognito for Kibana authentication.
cognitoOptions_enabled :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Bool)
cognitoOptions_enabled = Lens.lens (\CognitoOptions' {enabled} -> enabled) (\s@CognitoOptions' {} a -> s {enabled = a} :: CognitoOptions)

instance Prelude.FromJSON CognitoOptions where
  parseJSON =
    Prelude.withObject
      "CognitoOptions"
      ( \x ->
          CognitoOptions'
            Prelude.<$> (x Prelude..:? "IdentityPoolId")
            Prelude.<*> (x Prelude..:? "RoleArn")
            Prelude.<*> (x Prelude..:? "UserPoolId")
            Prelude.<*> (x Prelude..:? "Enabled")
      )

instance Prelude.Hashable CognitoOptions

instance Prelude.NFData CognitoOptions

instance Prelude.ToJSON CognitoOptions where
  toJSON CognitoOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IdentityPoolId" Prelude..=)
              Prelude.<$> identityPoolId,
            ("RoleArn" Prelude..=) Prelude.<$> roleArn,
            ("UserPoolId" Prelude..=) Prelude.<$> userPoolId,
            ("Enabled" Prelude..=) Prelude.<$> enabled
          ]
      )
