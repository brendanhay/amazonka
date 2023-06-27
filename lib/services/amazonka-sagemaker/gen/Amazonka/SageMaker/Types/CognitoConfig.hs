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
-- Module      : Amazonka.SageMaker.Types.CognitoConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CognitoConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this parameter to configure your Amazon Cognito workforce. A single
-- Cognito workforce is created using and corresponds to a single
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
--
-- /See:/ 'newCognitoConfig' smart constructor.
data CognitoConfig = CognitoConfig'
  { -- | A
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool>
    -- is a user directory in Amazon Cognito. With a user pool, your users can
    -- sign in to your web or mobile app through Amazon Cognito. Your users can
    -- also sign in through social identity providers like Google, Facebook,
    -- Amazon, or Apple, and through SAML identity providers.
    userPool :: Prelude.Text,
    -- | The client ID for your Amazon Cognito user pool.
    clientId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CognitoConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPool', 'cognitoConfig_userPool' - A
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool>
-- is a user directory in Amazon Cognito. With a user pool, your users can
-- sign in to your web or mobile app through Amazon Cognito. Your users can
-- also sign in through social identity providers like Google, Facebook,
-- Amazon, or Apple, and through SAML identity providers.
--
-- 'clientId', 'cognitoConfig_clientId' - The client ID for your Amazon Cognito user pool.
newCognitoConfig ::
  -- | 'userPool'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  CognitoConfig
newCognitoConfig pUserPool_ pClientId_ =
  CognitoConfig'
    { userPool = pUserPool_,
      clientId = pClientId_
    }

-- | A
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool>
-- is a user directory in Amazon Cognito. With a user pool, your users can
-- sign in to your web or mobile app through Amazon Cognito. Your users can
-- also sign in through social identity providers like Google, Facebook,
-- Amazon, or Apple, and through SAML identity providers.
cognitoConfig_userPool :: Lens.Lens' CognitoConfig Prelude.Text
cognitoConfig_userPool = Lens.lens (\CognitoConfig' {userPool} -> userPool) (\s@CognitoConfig' {} a -> s {userPool = a} :: CognitoConfig)

-- | The client ID for your Amazon Cognito user pool.
cognitoConfig_clientId :: Lens.Lens' CognitoConfig Prelude.Text
cognitoConfig_clientId = Lens.lens (\CognitoConfig' {clientId} -> clientId) (\s@CognitoConfig' {} a -> s {clientId = a} :: CognitoConfig)

instance Data.FromJSON CognitoConfig where
  parseJSON =
    Data.withObject
      "CognitoConfig"
      ( \x ->
          CognitoConfig'
            Prelude.<$> (x Data..: "UserPool")
            Prelude.<*> (x Data..: "ClientId")
      )

instance Prelude.Hashable CognitoConfig where
  hashWithSalt _salt CognitoConfig' {..} =
    _salt
      `Prelude.hashWithSalt` userPool
      `Prelude.hashWithSalt` clientId

instance Prelude.NFData CognitoConfig where
  rnf CognitoConfig' {..} =
    Prelude.rnf userPool
      `Prelude.seq` Prelude.rnf clientId

instance Data.ToJSON CognitoConfig where
  toJSON CognitoConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPool" Data..= userPool),
            Prelude.Just ("ClientId" Data..= clientId)
          ]
      )
