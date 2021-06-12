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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an Amazon Cognito user pool configuration.
--
-- /See:/ 'newUserPoolConfig' smart constructor.
data UserPoolConfig = UserPoolConfig'
  { -- | A regular expression for validating the incoming Amazon Cognito user
    -- pool app client ID.
    appIdClientRegex :: Core.Maybe Core.Text,
    -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The AWS Region in which the user pool was created.
    awsRegion :: Core.Text,
    -- | The action that you want your GraphQL API to take when a request that
    -- uses Amazon Cognito user pool authentication doesn\'t match the Amazon
    -- Cognito user pool configuration.
    defaultAction :: DefaultAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'awsRegion'
  Core.Text ->
  -- | 'defaultAction'
  DefaultAction ->
  UserPoolConfig
newUserPoolConfig
  pUserPoolId_
  pAwsRegion_
  pDefaultAction_ =
    UserPoolConfig'
      { appIdClientRegex = Core.Nothing,
        userPoolId = pUserPoolId_,
        awsRegion = pAwsRegion_,
        defaultAction = pDefaultAction_
      }

-- | A regular expression for validating the incoming Amazon Cognito user
-- pool app client ID.
userPoolConfig_appIdClientRegex :: Lens.Lens' UserPoolConfig (Core.Maybe Core.Text)
userPoolConfig_appIdClientRegex = Lens.lens (\UserPoolConfig' {appIdClientRegex} -> appIdClientRegex) (\s@UserPoolConfig' {} a -> s {appIdClientRegex = a} :: UserPoolConfig)

-- | The user pool ID.
userPoolConfig_userPoolId :: Lens.Lens' UserPoolConfig Core.Text
userPoolConfig_userPoolId = Lens.lens (\UserPoolConfig' {userPoolId} -> userPoolId) (\s@UserPoolConfig' {} a -> s {userPoolId = a} :: UserPoolConfig)

-- | The AWS Region in which the user pool was created.
userPoolConfig_awsRegion :: Lens.Lens' UserPoolConfig Core.Text
userPoolConfig_awsRegion = Lens.lens (\UserPoolConfig' {awsRegion} -> awsRegion) (\s@UserPoolConfig' {} a -> s {awsRegion = a} :: UserPoolConfig)

-- | The action that you want your GraphQL API to take when a request that
-- uses Amazon Cognito user pool authentication doesn\'t match the Amazon
-- Cognito user pool configuration.
userPoolConfig_defaultAction :: Lens.Lens' UserPoolConfig DefaultAction
userPoolConfig_defaultAction = Lens.lens (\UserPoolConfig' {defaultAction} -> defaultAction) (\s@UserPoolConfig' {} a -> s {defaultAction = a} :: UserPoolConfig)

instance Core.FromJSON UserPoolConfig where
  parseJSON =
    Core.withObject
      "UserPoolConfig"
      ( \x ->
          UserPoolConfig'
            Core.<$> (x Core..:? "appIdClientRegex")
            Core.<*> (x Core..: "userPoolId")
            Core.<*> (x Core..: "awsRegion")
            Core.<*> (x Core..: "defaultAction")
      )

instance Core.Hashable UserPoolConfig

instance Core.NFData UserPoolConfig

instance Core.ToJSON UserPoolConfig where
  toJSON UserPoolConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("appIdClientRegex" Core..=)
              Core.<$> appIdClientRegex,
            Core.Just ("userPoolId" Core..= userPoolId),
            Core.Just ("awsRegion" Core..= awsRegion),
            Core.Just ("defaultAction" Core..= defaultAction)
          ]
      )
