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
-- Module      : Network.AWS.AppSync.Types.CognitoUserPoolConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.CognitoUserPoolConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon Cognito user pool configuration.
--
-- /See:/ 'newCognitoUserPoolConfig' smart constructor.
data CognitoUserPoolConfig = CognitoUserPoolConfig'
  { -- | A regular expression for validating the incoming Amazon Cognito user
    -- pool app client ID.
    appIdClientRegex :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The AWS Region in which the user pool was created.
    awsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CognitoUserPoolConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appIdClientRegex', 'cognitoUserPoolConfig_appIdClientRegex' - A regular expression for validating the incoming Amazon Cognito user
-- pool app client ID.
--
-- 'userPoolId', 'cognitoUserPoolConfig_userPoolId' - The user pool ID.
--
-- 'awsRegion', 'cognitoUserPoolConfig_awsRegion' - The AWS Region in which the user pool was created.
newCognitoUserPoolConfig ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'awsRegion'
  Prelude.Text ->
  CognitoUserPoolConfig
newCognitoUserPoolConfig pUserPoolId_ pAwsRegion_ =
  CognitoUserPoolConfig'
    { appIdClientRegex =
        Prelude.Nothing,
      userPoolId = pUserPoolId_,
      awsRegion = pAwsRegion_
    }

-- | A regular expression for validating the incoming Amazon Cognito user
-- pool app client ID.
cognitoUserPoolConfig_appIdClientRegex :: Lens.Lens' CognitoUserPoolConfig (Prelude.Maybe Prelude.Text)
cognitoUserPoolConfig_appIdClientRegex = Lens.lens (\CognitoUserPoolConfig' {appIdClientRegex} -> appIdClientRegex) (\s@CognitoUserPoolConfig' {} a -> s {appIdClientRegex = a} :: CognitoUserPoolConfig)

-- | The user pool ID.
cognitoUserPoolConfig_userPoolId :: Lens.Lens' CognitoUserPoolConfig Prelude.Text
cognitoUserPoolConfig_userPoolId = Lens.lens (\CognitoUserPoolConfig' {userPoolId} -> userPoolId) (\s@CognitoUserPoolConfig' {} a -> s {userPoolId = a} :: CognitoUserPoolConfig)

-- | The AWS Region in which the user pool was created.
cognitoUserPoolConfig_awsRegion :: Lens.Lens' CognitoUserPoolConfig Prelude.Text
cognitoUserPoolConfig_awsRegion = Lens.lens (\CognitoUserPoolConfig' {awsRegion} -> awsRegion) (\s@CognitoUserPoolConfig' {} a -> s {awsRegion = a} :: CognitoUserPoolConfig)

instance Core.FromJSON CognitoUserPoolConfig where
  parseJSON =
    Core.withObject
      "CognitoUserPoolConfig"
      ( \x ->
          CognitoUserPoolConfig'
            Prelude.<$> (x Core..:? "appIdClientRegex")
            Prelude.<*> (x Core..: "userPoolId")
            Prelude.<*> (x Core..: "awsRegion")
      )

instance Prelude.Hashable CognitoUserPoolConfig

instance Prelude.NFData CognitoUserPoolConfig

instance Core.ToJSON CognitoUserPoolConfig where
  toJSON CognitoUserPoolConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("appIdClientRegex" Core..=)
              Prelude.<$> appIdClientRegex,
            Prelude.Just ("userPoolId" Core..= userPoolId),
            Prelude.Just ("awsRegion" Core..= awsRegion)
          ]
      )
