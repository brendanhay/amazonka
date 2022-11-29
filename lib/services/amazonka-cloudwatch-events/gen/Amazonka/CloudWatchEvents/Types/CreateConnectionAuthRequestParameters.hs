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
-- Module      : Amazonka.CloudWatchEvents.Types.CreateConnectionAuthRequestParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.CreateConnectionAuthRequestParameters where

import Amazonka.CloudWatchEvents.Types.ConnectionHttpParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionApiKeyAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the authorization parameters for the connection.
--
-- /See:/ 'newCreateConnectionAuthRequestParameters' smart constructor.
data CreateConnectionAuthRequestParameters = CreateConnectionAuthRequestParameters'
  { -- | A @CreateConnectionOAuthRequestParameters@ object that contains the
    -- OAuth authorization parameters to use for the connection.
    oAuthParameters :: Prelude.Maybe CreateConnectionOAuthRequestParameters,
    -- | A @ConnectionHttpParameters@ object that contains the API key
    -- authorization parameters to use for the connection. Note that if you
    -- include additional parameters for the target of a rule via
    -- @HttpParameters@, including query strings, the parameters added for the
    -- connection take precedence.
    invocationHttpParameters :: Prelude.Maybe ConnectionHttpParameters,
    -- | A @CreateConnectionBasicAuthRequestParameters@ object that contains the
    -- Basic authorization parameters to use for the connection.
    basicAuthParameters :: Prelude.Maybe CreateConnectionBasicAuthRequestParameters,
    -- | A @CreateConnectionApiKeyAuthRequestParameters@ object that contains the
    -- API key authorization parameters to use for the connection.
    apiKeyAuthParameters :: Prelude.Maybe CreateConnectionApiKeyAuthRequestParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oAuthParameters', 'createConnectionAuthRequestParameters_oAuthParameters' - A @CreateConnectionOAuthRequestParameters@ object that contains the
-- OAuth authorization parameters to use for the connection.
--
-- 'invocationHttpParameters', 'createConnectionAuthRequestParameters_invocationHttpParameters' - A @ConnectionHttpParameters@ object that contains the API key
-- authorization parameters to use for the connection. Note that if you
-- include additional parameters for the target of a rule via
-- @HttpParameters@, including query strings, the parameters added for the
-- connection take precedence.
--
-- 'basicAuthParameters', 'createConnectionAuthRequestParameters_basicAuthParameters' - A @CreateConnectionBasicAuthRequestParameters@ object that contains the
-- Basic authorization parameters to use for the connection.
--
-- 'apiKeyAuthParameters', 'createConnectionAuthRequestParameters_apiKeyAuthParameters' - A @CreateConnectionApiKeyAuthRequestParameters@ object that contains the
-- API key authorization parameters to use for the connection.
newCreateConnectionAuthRequestParameters ::
  CreateConnectionAuthRequestParameters
newCreateConnectionAuthRequestParameters =
  CreateConnectionAuthRequestParameters'
    { oAuthParameters =
        Prelude.Nothing,
      invocationHttpParameters =
        Prelude.Nothing,
      basicAuthParameters =
        Prelude.Nothing,
      apiKeyAuthParameters =
        Prelude.Nothing
    }

-- | A @CreateConnectionOAuthRequestParameters@ object that contains the
-- OAuth authorization parameters to use for the connection.
createConnectionAuthRequestParameters_oAuthParameters :: Lens.Lens' CreateConnectionAuthRequestParameters (Prelude.Maybe CreateConnectionOAuthRequestParameters)
createConnectionAuthRequestParameters_oAuthParameters = Lens.lens (\CreateConnectionAuthRequestParameters' {oAuthParameters} -> oAuthParameters) (\s@CreateConnectionAuthRequestParameters' {} a -> s {oAuthParameters = a} :: CreateConnectionAuthRequestParameters)

-- | A @ConnectionHttpParameters@ object that contains the API key
-- authorization parameters to use for the connection. Note that if you
-- include additional parameters for the target of a rule via
-- @HttpParameters@, including query strings, the parameters added for the
-- connection take precedence.
createConnectionAuthRequestParameters_invocationHttpParameters :: Lens.Lens' CreateConnectionAuthRequestParameters (Prelude.Maybe ConnectionHttpParameters)
createConnectionAuthRequestParameters_invocationHttpParameters = Lens.lens (\CreateConnectionAuthRequestParameters' {invocationHttpParameters} -> invocationHttpParameters) (\s@CreateConnectionAuthRequestParameters' {} a -> s {invocationHttpParameters = a} :: CreateConnectionAuthRequestParameters)

-- | A @CreateConnectionBasicAuthRequestParameters@ object that contains the
-- Basic authorization parameters to use for the connection.
createConnectionAuthRequestParameters_basicAuthParameters :: Lens.Lens' CreateConnectionAuthRequestParameters (Prelude.Maybe CreateConnectionBasicAuthRequestParameters)
createConnectionAuthRequestParameters_basicAuthParameters = Lens.lens (\CreateConnectionAuthRequestParameters' {basicAuthParameters} -> basicAuthParameters) (\s@CreateConnectionAuthRequestParameters' {} a -> s {basicAuthParameters = a} :: CreateConnectionAuthRequestParameters)

-- | A @CreateConnectionApiKeyAuthRequestParameters@ object that contains the
-- API key authorization parameters to use for the connection.
createConnectionAuthRequestParameters_apiKeyAuthParameters :: Lens.Lens' CreateConnectionAuthRequestParameters (Prelude.Maybe CreateConnectionApiKeyAuthRequestParameters)
createConnectionAuthRequestParameters_apiKeyAuthParameters = Lens.lens (\CreateConnectionAuthRequestParameters' {apiKeyAuthParameters} -> apiKeyAuthParameters) (\s@CreateConnectionAuthRequestParameters' {} a -> s {apiKeyAuthParameters = a} :: CreateConnectionAuthRequestParameters)

instance
  Prelude.Hashable
    CreateConnectionAuthRequestParameters
  where
  hashWithSalt
    _salt
    CreateConnectionAuthRequestParameters' {..} =
      _salt `Prelude.hashWithSalt` oAuthParameters
        `Prelude.hashWithSalt` invocationHttpParameters
        `Prelude.hashWithSalt` basicAuthParameters
        `Prelude.hashWithSalt` apiKeyAuthParameters

instance
  Prelude.NFData
    CreateConnectionAuthRequestParameters
  where
  rnf CreateConnectionAuthRequestParameters' {..} =
    Prelude.rnf oAuthParameters
      `Prelude.seq` Prelude.rnf invocationHttpParameters
      `Prelude.seq` Prelude.rnf basicAuthParameters
      `Prelude.seq` Prelude.rnf apiKeyAuthParameters

instance
  Core.ToJSON
    CreateConnectionAuthRequestParameters
  where
  toJSON CreateConnectionAuthRequestParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OAuthParameters" Core..=)
              Prelude.<$> oAuthParameters,
            ("InvocationHttpParameters" Core..=)
              Prelude.<$> invocationHttpParameters,
            ("BasicAuthParameters" Core..=)
              Prelude.<$> basicAuthParameters,
            ("ApiKeyAuthParameters" Core..=)
              Prelude.<$> apiKeyAuthParameters
          ]
      )
