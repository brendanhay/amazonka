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
-- Module      : Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiUserPoolConfigDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiUserPoolConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the authorization configuration for using Amazon Cognito user
-- pools with your AppSync GraphQL API endpoint.
--
-- /See:/ 'newAwsAppSyncGraphQlApiUserPoolConfigDetails' smart constructor.
data AwsAppSyncGraphQlApiUserPoolConfigDetails = AwsAppSyncGraphQlApiUserPoolConfigDetails'
  { -- | A regular expression for validating the incoming Amazon Cognito user
    -- pools app client ID. If this value isn\'t set, no filtering is applied.
    appIdClientRegex :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region in which the user pool was created.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The action that you want your GraphQL API to take when a request that
    -- uses Amazon Cognito user pools authentication doesn\'t match the Amazon
    -- Cognito user pools configuration.
    defaultAction :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAppSyncGraphQlApiUserPoolConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appIdClientRegex', 'awsAppSyncGraphQlApiUserPoolConfigDetails_appIdClientRegex' - A regular expression for validating the incoming Amazon Cognito user
-- pools app client ID. If this value isn\'t set, no filtering is applied.
--
-- 'awsRegion', 'awsAppSyncGraphQlApiUserPoolConfigDetails_awsRegion' - The Amazon Web Services Region in which the user pool was created.
--
-- 'defaultAction', 'awsAppSyncGraphQlApiUserPoolConfigDetails_defaultAction' - The action that you want your GraphQL API to take when a request that
-- uses Amazon Cognito user pools authentication doesn\'t match the Amazon
-- Cognito user pools configuration.
--
-- 'userPoolId', 'awsAppSyncGraphQlApiUserPoolConfigDetails_userPoolId' - The user pool ID.
newAwsAppSyncGraphQlApiUserPoolConfigDetails ::
  AwsAppSyncGraphQlApiUserPoolConfigDetails
newAwsAppSyncGraphQlApiUserPoolConfigDetails =
  AwsAppSyncGraphQlApiUserPoolConfigDetails'
    { appIdClientRegex =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      defaultAction = Prelude.Nothing,
      userPoolId = Prelude.Nothing
    }

-- | A regular expression for validating the incoming Amazon Cognito user
-- pools app client ID. If this value isn\'t set, no filtering is applied.
awsAppSyncGraphQlApiUserPoolConfigDetails_appIdClientRegex :: Lens.Lens' AwsAppSyncGraphQlApiUserPoolConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiUserPoolConfigDetails_appIdClientRegex = Lens.lens (\AwsAppSyncGraphQlApiUserPoolConfigDetails' {appIdClientRegex} -> appIdClientRegex) (\s@AwsAppSyncGraphQlApiUserPoolConfigDetails' {} a -> s {appIdClientRegex = a} :: AwsAppSyncGraphQlApiUserPoolConfigDetails)

-- | The Amazon Web Services Region in which the user pool was created.
awsAppSyncGraphQlApiUserPoolConfigDetails_awsRegion :: Lens.Lens' AwsAppSyncGraphQlApiUserPoolConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiUserPoolConfigDetails_awsRegion = Lens.lens (\AwsAppSyncGraphQlApiUserPoolConfigDetails' {awsRegion} -> awsRegion) (\s@AwsAppSyncGraphQlApiUserPoolConfigDetails' {} a -> s {awsRegion = a} :: AwsAppSyncGraphQlApiUserPoolConfigDetails)

-- | The action that you want your GraphQL API to take when a request that
-- uses Amazon Cognito user pools authentication doesn\'t match the Amazon
-- Cognito user pools configuration.
awsAppSyncGraphQlApiUserPoolConfigDetails_defaultAction :: Lens.Lens' AwsAppSyncGraphQlApiUserPoolConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiUserPoolConfigDetails_defaultAction = Lens.lens (\AwsAppSyncGraphQlApiUserPoolConfigDetails' {defaultAction} -> defaultAction) (\s@AwsAppSyncGraphQlApiUserPoolConfigDetails' {} a -> s {defaultAction = a} :: AwsAppSyncGraphQlApiUserPoolConfigDetails)

-- | The user pool ID.
awsAppSyncGraphQlApiUserPoolConfigDetails_userPoolId :: Lens.Lens' AwsAppSyncGraphQlApiUserPoolConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiUserPoolConfigDetails_userPoolId = Lens.lens (\AwsAppSyncGraphQlApiUserPoolConfigDetails' {userPoolId} -> userPoolId) (\s@AwsAppSyncGraphQlApiUserPoolConfigDetails' {} a -> s {userPoolId = a} :: AwsAppSyncGraphQlApiUserPoolConfigDetails)

instance
  Data.FromJSON
    AwsAppSyncGraphQlApiUserPoolConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsAppSyncGraphQlApiUserPoolConfigDetails"
      ( \x ->
          AwsAppSyncGraphQlApiUserPoolConfigDetails'
            Prelude.<$> (x Data..:? "AppIdClientRegex")
            Prelude.<*> (x Data..:? "AwsRegion")
            Prelude.<*> (x Data..:? "DefaultAction")
            Prelude.<*> (x Data..:? "UserPoolId")
      )

instance
  Prelude.Hashable
    AwsAppSyncGraphQlApiUserPoolConfigDetails
  where
  hashWithSalt
    _salt
    AwsAppSyncGraphQlApiUserPoolConfigDetails' {..} =
      _salt
        `Prelude.hashWithSalt` appIdClientRegex
        `Prelude.hashWithSalt` awsRegion
        `Prelude.hashWithSalt` defaultAction
        `Prelude.hashWithSalt` userPoolId

instance
  Prelude.NFData
    AwsAppSyncGraphQlApiUserPoolConfigDetails
  where
  rnf AwsAppSyncGraphQlApiUserPoolConfigDetails' {..} =
    Prelude.rnf appIdClientRegex
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf userPoolId

instance
  Data.ToJSON
    AwsAppSyncGraphQlApiUserPoolConfigDetails
  where
  toJSON AwsAppSyncGraphQlApiUserPoolConfigDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppIdClientRegex" Data..=)
              Prelude.<$> appIdClientRegex,
            ("AwsRegion" Data..=) Prelude.<$> awsRegion,
            ("DefaultAction" Data..=) Prelude.<$> defaultAction,
            ("UserPoolId" Data..=) Prelude.<$> userPoolId
          ]
      )
