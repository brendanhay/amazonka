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
-- Module      : Amazonka.AppSync.Types.UserPoolConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.UserPoolConfig where

import Amazonka.AppSync.Types.DefaultAction
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Cognito user pool configuration.
--
-- /See:/ 'newUserPoolConfig' smart constructor.
data UserPoolConfig = UserPoolConfig'
  { -- | A regular expression for validating the incoming Amazon Cognito user
    -- pool app client ID. If this value isn\'t set, no filtering is applied.
    appIdClientRegex :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The Amazon Web Services Region in which the user pool was created.
    awsRegion :: Prelude.Text,
    -- | The action that you want your GraphQL API to take when a request that
    -- uses Amazon Cognito user pool authentication doesn\'t match the Amazon
    -- Cognito user pool configuration.
    defaultAction :: DefaultAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPoolConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appIdClientRegex', 'userPoolConfig_appIdClientRegex' - A regular expression for validating the incoming Amazon Cognito user
-- pool app client ID. If this value isn\'t set, no filtering is applied.
--
-- 'userPoolId', 'userPoolConfig_userPoolId' - The user pool ID.
--
-- 'awsRegion', 'userPoolConfig_awsRegion' - The Amazon Web Services Region in which the user pool was created.
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
-- pool app client ID. If this value isn\'t set, no filtering is applied.
userPoolConfig_appIdClientRegex :: Lens.Lens' UserPoolConfig (Prelude.Maybe Prelude.Text)
userPoolConfig_appIdClientRegex = Lens.lens (\UserPoolConfig' {appIdClientRegex} -> appIdClientRegex) (\s@UserPoolConfig' {} a -> s {appIdClientRegex = a} :: UserPoolConfig)

-- | The user pool ID.
userPoolConfig_userPoolId :: Lens.Lens' UserPoolConfig Prelude.Text
userPoolConfig_userPoolId = Lens.lens (\UserPoolConfig' {userPoolId} -> userPoolId) (\s@UserPoolConfig' {} a -> s {userPoolId = a} :: UserPoolConfig)

-- | The Amazon Web Services Region in which the user pool was created.
userPoolConfig_awsRegion :: Lens.Lens' UserPoolConfig Prelude.Text
userPoolConfig_awsRegion = Lens.lens (\UserPoolConfig' {awsRegion} -> awsRegion) (\s@UserPoolConfig' {} a -> s {awsRegion = a} :: UserPoolConfig)

-- | The action that you want your GraphQL API to take when a request that
-- uses Amazon Cognito user pool authentication doesn\'t match the Amazon
-- Cognito user pool configuration.
userPoolConfig_defaultAction :: Lens.Lens' UserPoolConfig DefaultAction
userPoolConfig_defaultAction = Lens.lens (\UserPoolConfig' {defaultAction} -> defaultAction) (\s@UserPoolConfig' {} a -> s {defaultAction = a} :: UserPoolConfig)

instance Data.FromJSON UserPoolConfig where
  parseJSON =
    Data.withObject
      "UserPoolConfig"
      ( \x ->
          UserPoolConfig'
            Prelude.<$> (x Data..:? "appIdClientRegex")
            Prelude.<*> (x Data..: "userPoolId")
            Prelude.<*> (x Data..: "awsRegion")
            Prelude.<*> (x Data..: "defaultAction")
      )

instance Prelude.Hashable UserPoolConfig where
  hashWithSalt _salt UserPoolConfig' {..} =
    _salt `Prelude.hashWithSalt` appIdClientRegex
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` defaultAction

instance Prelude.NFData UserPoolConfig where
  rnf UserPoolConfig' {..} =
    Prelude.rnf appIdClientRegex
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf defaultAction

instance Data.ToJSON UserPoolConfig where
  toJSON UserPoolConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("appIdClientRegex" Data..=)
              Prelude.<$> appIdClientRegex,
            Prelude.Just ("userPoolId" Data..= userPoolId),
            Prelude.Just ("awsRegion" Data..= awsRegion),
            Prelude.Just
              ("defaultAction" Data..= defaultAction)
          ]
      )
