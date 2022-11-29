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
-- Module      : Amazonka.AppFlow.Types.DatadogConnectorProfileCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DatadogConnectorProfileCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific credentials required by Datadog.
--
-- /See:/ 'newDatadogConnectorProfileCredentials' smart constructor.
data DatadogConnectorProfileCredentials = DatadogConnectorProfileCredentials'
  { -- | A unique alphanumeric identifier used to authenticate a user, developer,
    -- or calling program to your API.
    apiKey :: Core.Sensitive Prelude.Text,
    -- | Application keys, in conjunction with your API key, give you full access
    -- to Datadog’s programmatic API. Application keys are associated with the
    -- user account that created them. The application key is used to log all
    -- requests made to the API.
    applicationKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatadogConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKey', 'datadogConnectorProfileCredentials_apiKey' - A unique alphanumeric identifier used to authenticate a user, developer,
-- or calling program to your API.
--
-- 'applicationKey', 'datadogConnectorProfileCredentials_applicationKey' - Application keys, in conjunction with your API key, give you full access
-- to Datadog’s programmatic API. Application keys are associated with the
-- user account that created them. The application key is used to log all
-- requests made to the API.
newDatadogConnectorProfileCredentials ::
  -- | 'apiKey'
  Prelude.Text ->
  -- | 'applicationKey'
  Prelude.Text ->
  DatadogConnectorProfileCredentials
newDatadogConnectorProfileCredentials
  pApiKey_
  pApplicationKey_ =
    DatadogConnectorProfileCredentials'
      { apiKey =
          Core._Sensitive Lens.# pApiKey_,
        applicationKey = pApplicationKey_
      }

-- | A unique alphanumeric identifier used to authenticate a user, developer,
-- or calling program to your API.
datadogConnectorProfileCredentials_apiKey :: Lens.Lens' DatadogConnectorProfileCredentials Prelude.Text
datadogConnectorProfileCredentials_apiKey = Lens.lens (\DatadogConnectorProfileCredentials' {apiKey} -> apiKey) (\s@DatadogConnectorProfileCredentials' {} a -> s {apiKey = a} :: DatadogConnectorProfileCredentials) Prelude.. Core._Sensitive

-- | Application keys, in conjunction with your API key, give you full access
-- to Datadog’s programmatic API. Application keys are associated with the
-- user account that created them. The application key is used to log all
-- requests made to the API.
datadogConnectorProfileCredentials_applicationKey :: Lens.Lens' DatadogConnectorProfileCredentials Prelude.Text
datadogConnectorProfileCredentials_applicationKey = Lens.lens (\DatadogConnectorProfileCredentials' {applicationKey} -> applicationKey) (\s@DatadogConnectorProfileCredentials' {} a -> s {applicationKey = a} :: DatadogConnectorProfileCredentials)

instance
  Prelude.Hashable
    DatadogConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    DatadogConnectorProfileCredentials' {..} =
      _salt `Prelude.hashWithSalt` apiKey
        `Prelude.hashWithSalt` applicationKey

instance
  Prelude.NFData
    DatadogConnectorProfileCredentials
  where
  rnf DatadogConnectorProfileCredentials' {..} =
    Prelude.rnf apiKey
      `Prelude.seq` Prelude.rnf applicationKey

instance
  Core.ToJSON
    DatadogConnectorProfileCredentials
  where
  toJSON DatadogConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("apiKey" Core..= apiKey),
            Prelude.Just
              ("applicationKey" Core..= applicationKey)
          ]
      )
