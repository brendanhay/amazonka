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
-- Module      : Amazonka.IoT.Types.HttpAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.HttpAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.HttpActionHeader
import Amazonka.IoT.Types.HttpAuthorization
import qualified Amazonka.Prelude as Prelude

-- | Send data to an HTTPS endpoint.
--
-- /See:/ 'newHttpAction' smart constructor.
data HttpAction = HttpAction'
  { -- | The authentication method to use when sending data to an HTTPS endpoint.
    auth :: Prelude.Maybe HttpAuthorization,
    -- | The URL to which IoT sends a confirmation message. The value of the
    -- confirmation URL must be a prefix of the endpoint URL. If you do not
    -- specify a confirmation URL IoT uses the endpoint URL as the confirmation
    -- URL. If you use substitution templates in the confirmationUrl, you must
    -- create and enable topic rule destinations that match each possible value
    -- of the substitution template before traffic is allowed to your endpoint
    -- URL.
    confirmationUrl :: Prelude.Maybe Prelude.Text,
    -- | The HTTP headers to send with the message data.
    headers :: Prelude.Maybe [HttpActionHeader],
    -- | The endpoint URL. If substitution templates are used in the URL, you
    -- must also specify a @confirmationUrl@. If this is a new destination, a
    -- new @TopicRuleDestination@ is created if possible.
    url :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auth', 'httpAction_auth' - The authentication method to use when sending data to an HTTPS endpoint.
--
-- 'confirmationUrl', 'httpAction_confirmationUrl' - The URL to which IoT sends a confirmation message. The value of the
-- confirmation URL must be a prefix of the endpoint URL. If you do not
-- specify a confirmation URL IoT uses the endpoint URL as the confirmation
-- URL. If you use substitution templates in the confirmationUrl, you must
-- create and enable topic rule destinations that match each possible value
-- of the substitution template before traffic is allowed to your endpoint
-- URL.
--
-- 'headers', 'httpAction_headers' - The HTTP headers to send with the message data.
--
-- 'url', 'httpAction_url' - The endpoint URL. If substitution templates are used in the URL, you
-- must also specify a @confirmationUrl@. If this is a new destination, a
-- new @TopicRuleDestination@ is created if possible.
newHttpAction ::
  -- | 'url'
  Prelude.Text ->
  HttpAction
newHttpAction pUrl_ =
  HttpAction'
    { auth = Prelude.Nothing,
      confirmationUrl = Prelude.Nothing,
      headers = Prelude.Nothing,
      url = pUrl_
    }

-- | The authentication method to use when sending data to an HTTPS endpoint.
httpAction_auth :: Lens.Lens' HttpAction (Prelude.Maybe HttpAuthorization)
httpAction_auth = Lens.lens (\HttpAction' {auth} -> auth) (\s@HttpAction' {} a -> s {auth = a} :: HttpAction)

-- | The URL to which IoT sends a confirmation message. The value of the
-- confirmation URL must be a prefix of the endpoint URL. If you do not
-- specify a confirmation URL IoT uses the endpoint URL as the confirmation
-- URL. If you use substitution templates in the confirmationUrl, you must
-- create and enable topic rule destinations that match each possible value
-- of the substitution template before traffic is allowed to your endpoint
-- URL.
httpAction_confirmationUrl :: Lens.Lens' HttpAction (Prelude.Maybe Prelude.Text)
httpAction_confirmationUrl = Lens.lens (\HttpAction' {confirmationUrl} -> confirmationUrl) (\s@HttpAction' {} a -> s {confirmationUrl = a} :: HttpAction)

-- | The HTTP headers to send with the message data.
httpAction_headers :: Lens.Lens' HttpAction (Prelude.Maybe [HttpActionHeader])
httpAction_headers = Lens.lens (\HttpAction' {headers} -> headers) (\s@HttpAction' {} a -> s {headers = a} :: HttpAction) Prelude.. Lens.mapping Lens.coerced

-- | The endpoint URL. If substitution templates are used in the URL, you
-- must also specify a @confirmationUrl@. If this is a new destination, a
-- new @TopicRuleDestination@ is created if possible.
httpAction_url :: Lens.Lens' HttpAction Prelude.Text
httpAction_url = Lens.lens (\HttpAction' {url} -> url) (\s@HttpAction' {} a -> s {url = a} :: HttpAction)

instance Data.FromJSON HttpAction where
  parseJSON =
    Data.withObject
      "HttpAction"
      ( \x ->
          HttpAction'
            Prelude.<$> (x Data..:? "auth")
            Prelude.<*> (x Data..:? "confirmationUrl")
            Prelude.<*> (x Data..:? "headers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "url")
      )

instance Prelude.Hashable HttpAction where
  hashWithSalt _salt HttpAction' {..} =
    _salt
      `Prelude.hashWithSalt` auth
      `Prelude.hashWithSalt` confirmationUrl
      `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` url

instance Prelude.NFData HttpAction where
  rnf HttpAction' {..} =
    Prelude.rnf auth
      `Prelude.seq` Prelude.rnf confirmationUrl
      `Prelude.seq` Prelude.rnf headers
      `Prelude.seq` Prelude.rnf url

instance Data.ToJSON HttpAction where
  toJSON HttpAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("auth" Data..=) Prelude.<$> auth,
            ("confirmationUrl" Data..=)
              Prelude.<$> confirmationUrl,
            ("headers" Data..=) Prelude.<$> headers,
            Prelude.Just ("url" Data..= url)
          ]
      )
