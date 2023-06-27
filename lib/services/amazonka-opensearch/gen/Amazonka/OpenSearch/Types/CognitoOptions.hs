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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.CognitoOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Container for the parameters required to enable Cognito authentication
-- for an OpenSearch Service domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
--
-- /See:/ 'newCognitoOptions' smart constructor.
data CognitoOptions = CognitoOptions'
  { -- | Whether to enable or disable Amazon Cognito authentication for
    -- OpenSearch Dashboards.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Cognito identity pool ID that you want OpenSearch Service to
    -- use for OpenSearch Dashboards authentication.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The @AmazonOpenSearchServiceCognitoAccess@ role that allows OpenSearch
    -- Service to configure your user pool and identity pool.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Cognito user pool ID that you want OpenSearch Service to use
    -- for OpenSearch Dashboards authentication.
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
-- 'enabled', 'cognitoOptions_enabled' - Whether to enable or disable Amazon Cognito authentication for
-- OpenSearch Dashboards.
--
-- 'identityPoolId', 'cognitoOptions_identityPoolId' - The Amazon Cognito identity pool ID that you want OpenSearch Service to
-- use for OpenSearch Dashboards authentication.
--
-- 'roleArn', 'cognitoOptions_roleArn' - The @AmazonOpenSearchServiceCognitoAccess@ role that allows OpenSearch
-- Service to configure your user pool and identity pool.
--
-- 'userPoolId', 'cognitoOptions_userPoolId' - The Amazon Cognito user pool ID that you want OpenSearch Service to use
-- for OpenSearch Dashboards authentication.
newCognitoOptions ::
  CognitoOptions
newCognitoOptions =
  CognitoOptions'
    { enabled = Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      userPoolId = Prelude.Nothing
    }

-- | Whether to enable or disable Amazon Cognito authentication for
-- OpenSearch Dashboards.
cognitoOptions_enabled :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Bool)
cognitoOptions_enabled = Lens.lens (\CognitoOptions' {enabled} -> enabled) (\s@CognitoOptions' {} a -> s {enabled = a} :: CognitoOptions)

-- | The Amazon Cognito identity pool ID that you want OpenSearch Service to
-- use for OpenSearch Dashboards authentication.
cognitoOptions_identityPoolId :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_identityPoolId = Lens.lens (\CognitoOptions' {identityPoolId} -> identityPoolId) (\s@CognitoOptions' {} a -> s {identityPoolId = a} :: CognitoOptions)

-- | The @AmazonOpenSearchServiceCognitoAccess@ role that allows OpenSearch
-- Service to configure your user pool and identity pool.
cognitoOptions_roleArn :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_roleArn = Lens.lens (\CognitoOptions' {roleArn} -> roleArn) (\s@CognitoOptions' {} a -> s {roleArn = a} :: CognitoOptions)

-- | The Amazon Cognito user pool ID that you want OpenSearch Service to use
-- for OpenSearch Dashboards authentication.
cognitoOptions_userPoolId :: Lens.Lens' CognitoOptions (Prelude.Maybe Prelude.Text)
cognitoOptions_userPoolId = Lens.lens (\CognitoOptions' {userPoolId} -> userPoolId) (\s@CognitoOptions' {} a -> s {userPoolId = a} :: CognitoOptions)

instance Data.FromJSON CognitoOptions where
  parseJSON =
    Data.withObject
      "CognitoOptions"
      ( \x ->
          CognitoOptions'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "IdentityPoolId")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "UserPoolId")
      )

instance Prelude.Hashable CognitoOptions where
  hashWithSalt _salt CognitoOptions' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData CognitoOptions where
  rnf CognitoOptions' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToJSON CognitoOptions where
  toJSON CognitoOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("IdentityPoolId" Data..=)
              Prelude.<$> identityPoolId,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("UserPoolId" Data..=) Prelude.<$> userPoolId
          ]
      )
