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
-- Module      : Network.AWS.AppSync.Types.UserPoolConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.UserPoolConfig where

import Network.AWS.AppSync.Types.DefaultAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon Cognito user pool configuration.
--
-- /See:/ 'newUserPoolConfig' smart constructor.
data UserPoolConfig = UserPoolConfig'
  { -- | A regular expression for validating the incoming Amazon Cognito user
    -- pool app client ID.
    appIdClientRegex :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The AWS Region in which the user pool was created.
    awsRegion :: Prelude.Text,
    -- | The action that you want your GraphQL API to take when a request that
    -- uses Amazon Cognito user pool authentication doesn\'t match the Amazon
    -- Cognito user pool configuration.
    defaultAction :: DefaultAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserPoolConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appIdClientRegex', 'userPoolConfig_appIdClientRegex' - A regular expression for validating the incoming Amazon Cognito user
-- pool app client ID.
--
-- 'userPoolId', 'userPoolConfig_userPoolId' - The user pool ID.
--
-- 'awsRegion', 'userPoolConfig_awsRegion' - The AWS Region in which the user pool was created.
--
-- 'defaultAction', 'userPoolConfig_defaultAction' - The action that you want your GraphQL API to take when a request that
-- uses Amazon Cognito user pool authentication doesn\'t match the Amazon
-- Cognito user pool configuration.
newUserPoolConfig ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'awsRegion'
  Prelude.Text ->
  -- | 'defaultAction'
  DefaultAction ->
  UserPoolConfig
newUserPoolConfig
  pUserPoolId_
  pAwsRegion_
  pDefaultAction_ =
    UserPoolConfig'
      { appIdClientRegex = Prelude.Nothing,
        userPoolId = pUserPoolId_,
        awsRegion = pAwsRegion_,
        defaultAction = pDefaultAction_
      }

-- | A regular expression for validating the incoming Amazon Cognito user
-- pool app client ID.
userPoolConfig_appIdClientRegex :: Lens.Lens' UserPoolConfig (Prelude.Maybe Prelude.Text)
userPoolConfig_appIdClientRegex = Lens.lens (\UserPoolConfig' {appIdClientRegex} -> appIdClientRegex) (\s@UserPoolConfig' {} a -> s {appIdClientRegex = a} :: UserPoolConfig)

-- | The user pool ID.
userPoolConfig_userPoolId :: Lens.Lens' UserPoolConfig Prelude.Text
userPoolConfig_userPoolId = Lens.lens (\UserPoolConfig' {userPoolId} -> userPoolId) (\s@UserPoolConfig' {} a -> s {userPoolId = a} :: UserPoolConfig)

-- | The AWS Region in which the user pool was created.
userPoolConfig_awsRegion :: Lens.Lens' UserPoolConfig Prelude.Text
userPoolConfig_awsRegion = Lens.lens (\UserPoolConfig' {awsRegion} -> awsRegion) (\s@UserPoolConfig' {} a -> s {awsRegion = a} :: UserPoolConfig)

-- | The action that you want your GraphQL API to take when a request that
-- uses Amazon Cognito user pool authentication doesn\'t match the Amazon
-- Cognito user pool configuration.
userPoolConfig_defaultAction :: Lens.Lens' UserPoolConfig DefaultAction
userPoolConfig_defaultAction = Lens.lens (\UserPoolConfig' {defaultAction} -> defaultAction) (\s@UserPoolConfig' {} a -> s {defaultAction = a} :: UserPoolConfig)

instance Prelude.FromJSON UserPoolConfig where
  parseJSON =
    Prelude.withObject
      "UserPoolConfig"
      ( \x ->
          UserPoolConfig'
            Prelude.<$> (x Prelude..:? "appIdClientRegex")
            Prelude.<*> (x Prelude..: "userPoolId")
            Prelude.<*> (x Prelude..: "awsRegion")
            Prelude.<*> (x Prelude..: "defaultAction")
      )

instance Prelude.Hashable UserPoolConfig

instance Prelude.NFData UserPoolConfig

instance Prelude.ToJSON UserPoolConfig where
  toJSON UserPoolConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("appIdClientRegex" Prelude..=)
              Prelude.<$> appIdClientRegex,
            Prelude.Just ("userPoolId" Prelude..= userPoolId),
            Prelude.Just ("awsRegion" Prelude..= awsRegion),
            Prelude.Just
              ("defaultAction" Prelude..= defaultAction)
          ]
      )
