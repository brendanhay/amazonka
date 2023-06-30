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
-- Module      : Amazonka.APIGateway.Types.Account
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.Account where

import Amazonka.APIGateway.Types.ThrottleSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an AWS account that is associated with API Gateway.
--
-- /See:/ 'newAccount' smart constructor.
data Account = Account'
  { -- | The version of the API keys used for the account.
    apiKeyVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of an Amazon CloudWatch role for the current Account.
    cloudwatchRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of features supported for the account. When usage plans are
    -- enabled, the features list will include an entry of @\"UsagePlans\"@.
    features :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the API request limits configured for the current Account.
    throttleSettings :: Prelude.Maybe ThrottleSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Account' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeyVersion', 'account_apiKeyVersion' - The version of the API keys used for the account.
--
-- 'cloudwatchRoleArn', 'account_cloudwatchRoleArn' - The ARN of an Amazon CloudWatch role for the current Account.
--
-- 'features', 'account_features' - A list of features supported for the account. When usage plans are
-- enabled, the features list will include an entry of @\"UsagePlans\"@.
--
-- 'throttleSettings', 'account_throttleSettings' - Specifies the API request limits configured for the current Account.
newAccount ::
  Account
newAccount =
  Account'
    { apiKeyVersion = Prelude.Nothing,
      cloudwatchRoleArn = Prelude.Nothing,
      features = Prelude.Nothing,
      throttleSettings = Prelude.Nothing
    }

-- | The version of the API keys used for the account.
account_apiKeyVersion :: Lens.Lens' Account (Prelude.Maybe Prelude.Text)
account_apiKeyVersion = Lens.lens (\Account' {apiKeyVersion} -> apiKeyVersion) (\s@Account' {} a -> s {apiKeyVersion = a} :: Account)

-- | The ARN of an Amazon CloudWatch role for the current Account.
account_cloudwatchRoleArn :: Lens.Lens' Account (Prelude.Maybe Prelude.Text)
account_cloudwatchRoleArn = Lens.lens (\Account' {cloudwatchRoleArn} -> cloudwatchRoleArn) (\s@Account' {} a -> s {cloudwatchRoleArn = a} :: Account)

-- | A list of features supported for the account. When usage plans are
-- enabled, the features list will include an entry of @\"UsagePlans\"@.
account_features :: Lens.Lens' Account (Prelude.Maybe [Prelude.Text])
account_features = Lens.lens (\Account' {features} -> features) (\s@Account' {} a -> s {features = a} :: Account) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the API request limits configured for the current Account.
account_throttleSettings :: Lens.Lens' Account (Prelude.Maybe ThrottleSettings)
account_throttleSettings = Lens.lens (\Account' {throttleSettings} -> throttleSettings) (\s@Account' {} a -> s {throttleSettings = a} :: Account)

instance Data.FromJSON Account where
  parseJSON =
    Data.withObject
      "Account"
      ( \x ->
          Account'
            Prelude.<$> (x Data..:? "apiKeyVersion")
            Prelude.<*> (x Data..:? "cloudwatchRoleArn")
            Prelude.<*> (x Data..:? "features" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "throttleSettings")
      )

instance Prelude.Hashable Account where
  hashWithSalt _salt Account' {..} =
    _salt
      `Prelude.hashWithSalt` apiKeyVersion
      `Prelude.hashWithSalt` cloudwatchRoleArn
      `Prelude.hashWithSalt` features
      `Prelude.hashWithSalt` throttleSettings

instance Prelude.NFData Account where
  rnf Account' {..} =
    Prelude.rnf apiKeyVersion
      `Prelude.seq` Prelude.rnf cloudwatchRoleArn
      `Prelude.seq` Prelude.rnf features
      `Prelude.seq` Prelude.rnf throttleSettings
