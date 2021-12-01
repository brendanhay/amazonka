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
-- Module      : Amazonka.CloudWatchEvents.Types.UpdateConnectionAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.UpdateConnectionAuthRequestParameters where

import Amazonka.CloudWatchEvents.Types.ConnectionHttpParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the additional parameters to use for the connection.
--
-- /See:/ 'newUpdateConnectionAuthRequestParameters' smart constructor.
data UpdateConnectionAuthRequestParameters = UpdateConnectionAuthRequestParameters'
  { -- | A @UpdateConnectionOAuthRequestParameters@ object that contains the
    -- authorization parameters for OAuth authorization.
    oAuthParameters :: Prelude.Maybe UpdateConnectionOAuthRequestParameters,
    -- | A @ConnectionHttpParameters@ object that contains the additional
    -- parameters to use for the connection.
    invocationHttpParameters :: Prelude.Maybe ConnectionHttpParameters,
    -- | A @UpdateConnectionApiKeyAuthRequestParameters@ object that contains the
    -- authorization parameters for API key authorization.
    apiKeyAuthParameters :: Prelude.Maybe UpdateConnectionApiKeyAuthRequestParameters,
    -- | A @UpdateConnectionBasicAuthRequestParameters@ object that contains the
    -- authorization parameters for Basic authorization.
    basicAuthParameters :: Prelude.Maybe UpdateConnectionBasicAuthRequestParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oAuthParameters', 'updateConnectionAuthRequestParameters_oAuthParameters' - A @UpdateConnectionOAuthRequestParameters@ object that contains the
-- authorization parameters for OAuth authorization.
--
-- 'invocationHttpParameters', 'updateConnectionAuthRequestParameters_invocationHttpParameters' - A @ConnectionHttpParameters@ object that contains the additional
-- parameters to use for the connection.
--
-- 'apiKeyAuthParameters', 'updateConnectionAuthRequestParameters_apiKeyAuthParameters' - A @UpdateConnectionApiKeyAuthRequestParameters@ object that contains the
-- authorization parameters for API key authorization.
--
-- 'basicAuthParameters', 'updateConnectionAuthRequestParameters_basicAuthParameters' - A @UpdateConnectionBasicAuthRequestParameters@ object that contains the
-- authorization parameters for Basic authorization.
newUpdateConnectionAuthRequestParameters ::
  UpdateConnectionAuthRequestParameters
newUpdateConnectionAuthRequestParameters =
  UpdateConnectionAuthRequestParameters'
    { oAuthParameters =
        Prelude.Nothing,
      invocationHttpParameters =
        Prelude.Nothing,
      apiKeyAuthParameters =
        Prelude.Nothing,
      basicAuthParameters =
        Prelude.Nothing
    }

-- | A @UpdateConnectionOAuthRequestParameters@ object that contains the
-- authorization parameters for OAuth authorization.
updateConnectionAuthRequestParameters_oAuthParameters :: Lens.Lens' UpdateConnectionAuthRequestParameters (Prelude.Maybe UpdateConnectionOAuthRequestParameters)
updateConnectionAuthRequestParameters_oAuthParameters = Lens.lens (\UpdateConnectionAuthRequestParameters' {oAuthParameters} -> oAuthParameters) (\s@UpdateConnectionAuthRequestParameters' {} a -> s {oAuthParameters = a} :: UpdateConnectionAuthRequestParameters)

-- | A @ConnectionHttpParameters@ object that contains the additional
-- parameters to use for the connection.
updateConnectionAuthRequestParameters_invocationHttpParameters :: Lens.Lens' UpdateConnectionAuthRequestParameters (Prelude.Maybe ConnectionHttpParameters)
updateConnectionAuthRequestParameters_invocationHttpParameters = Lens.lens (\UpdateConnectionAuthRequestParameters' {invocationHttpParameters} -> invocationHttpParameters) (\s@UpdateConnectionAuthRequestParameters' {} a -> s {invocationHttpParameters = a} :: UpdateConnectionAuthRequestParameters)

-- | A @UpdateConnectionApiKeyAuthRequestParameters@ object that contains the
-- authorization parameters for API key authorization.
updateConnectionAuthRequestParameters_apiKeyAuthParameters :: Lens.Lens' UpdateConnectionAuthRequestParameters (Prelude.Maybe UpdateConnectionApiKeyAuthRequestParameters)
updateConnectionAuthRequestParameters_apiKeyAuthParameters = Lens.lens (\UpdateConnectionAuthRequestParameters' {apiKeyAuthParameters} -> apiKeyAuthParameters) (\s@UpdateConnectionAuthRequestParameters' {} a -> s {apiKeyAuthParameters = a} :: UpdateConnectionAuthRequestParameters)

-- | A @UpdateConnectionBasicAuthRequestParameters@ object that contains the
-- authorization parameters for Basic authorization.
updateConnectionAuthRequestParameters_basicAuthParameters :: Lens.Lens' UpdateConnectionAuthRequestParameters (Prelude.Maybe UpdateConnectionBasicAuthRequestParameters)
updateConnectionAuthRequestParameters_basicAuthParameters = Lens.lens (\UpdateConnectionAuthRequestParameters' {basicAuthParameters} -> basicAuthParameters) (\s@UpdateConnectionAuthRequestParameters' {} a -> s {basicAuthParameters = a} :: UpdateConnectionAuthRequestParameters)

instance
  Prelude.Hashable
    UpdateConnectionAuthRequestParameters
  where
  hashWithSalt
    salt'
    UpdateConnectionAuthRequestParameters' {..} =
      salt' `Prelude.hashWithSalt` basicAuthParameters
        `Prelude.hashWithSalt` apiKeyAuthParameters
        `Prelude.hashWithSalt` invocationHttpParameters
        `Prelude.hashWithSalt` oAuthParameters

instance
  Prelude.NFData
    UpdateConnectionAuthRequestParameters
  where
  rnf UpdateConnectionAuthRequestParameters' {..} =
    Prelude.rnf oAuthParameters
      `Prelude.seq` Prelude.rnf basicAuthParameters
      `Prelude.seq` Prelude.rnf apiKeyAuthParameters
      `Prelude.seq` Prelude.rnf invocationHttpParameters

instance
  Core.ToJSON
    UpdateConnectionAuthRequestParameters
  where
  toJSON UpdateConnectionAuthRequestParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OAuthParameters" Core..=)
              Prelude.<$> oAuthParameters,
            ("InvocationHttpParameters" Core..=)
              Prelude.<$> invocationHttpParameters,
            ("ApiKeyAuthParameters" Core..=)
              Prelude.<$> apiKeyAuthParameters,
            ("BasicAuthParameters" Core..=)
              Prelude.<$> basicAuthParameters
          ]
      )
