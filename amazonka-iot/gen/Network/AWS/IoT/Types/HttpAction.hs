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
-- Module      : Network.AWS.IoT.Types.HttpAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HttpAction where

import Network.AWS.IoT.Types.HttpActionHeader
import Network.AWS.IoT.Types.HttpAuthorization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Send data to an HTTPS endpoint.
--
-- /See:/ 'newHttpAction' smart constructor.
data HttpAction = HttpAction'
  { -- | The HTTP headers to send with the message data.
    headers :: Prelude.Maybe [HttpActionHeader],
    -- | The authentication method to use when sending data to an HTTPS endpoint.
    auth :: Prelude.Maybe HttpAuthorization,
    -- | The URL to which AWS IoT sends a confirmation message. The value of the
    -- confirmation URL must be a prefix of the endpoint URL. If you do not
    -- specify a confirmation URL AWS IoT uses the endpoint URL as the
    -- confirmation URL. If you use substitution templates in the
    -- confirmationUrl, you must create and enable topic rule destinations that
    -- match each possible value of the substitution template before traffic is
    -- allowed to your endpoint URL.
    confirmationUrl :: Prelude.Maybe Prelude.Text,
    -- | The endpoint URL. If substitution templates are used in the URL, you
    -- must also specify a @confirmationUrl@. If this is a new destination, a
    -- new @TopicRuleDestination@ is created if possible.
    url :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HttpAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headers', 'httpAction_headers' - The HTTP headers to send with the message data.
--
-- 'auth', 'httpAction_auth' - The authentication method to use when sending data to an HTTPS endpoint.
--
-- 'confirmationUrl', 'httpAction_confirmationUrl' - The URL to which AWS IoT sends a confirmation message. The value of the
-- confirmation URL must be a prefix of the endpoint URL. If you do not
-- specify a confirmation URL AWS IoT uses the endpoint URL as the
-- confirmation URL. If you use substitution templates in the
-- confirmationUrl, you must create and enable topic rule destinations that
-- match each possible value of the substitution template before traffic is
-- allowed to your endpoint URL.
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
    { headers = Prelude.Nothing,
      auth = Prelude.Nothing,
      confirmationUrl = Prelude.Nothing,
      url = pUrl_
    }

-- | The HTTP headers to send with the message data.
httpAction_headers :: Lens.Lens' HttpAction (Prelude.Maybe [HttpActionHeader])
httpAction_headers = Lens.lens (\HttpAction' {headers} -> headers) (\s@HttpAction' {} a -> s {headers = a} :: HttpAction) Prelude.. Lens.mapping Prelude._Coerce

-- | The authentication method to use when sending data to an HTTPS endpoint.
httpAction_auth :: Lens.Lens' HttpAction (Prelude.Maybe HttpAuthorization)
httpAction_auth = Lens.lens (\HttpAction' {auth} -> auth) (\s@HttpAction' {} a -> s {auth = a} :: HttpAction)

-- | The URL to which AWS IoT sends a confirmation message. The value of the
-- confirmation URL must be a prefix of the endpoint URL. If you do not
-- specify a confirmation URL AWS IoT uses the endpoint URL as the
-- confirmation URL. If you use substitution templates in the
-- confirmationUrl, you must create and enable topic rule destinations that
-- match each possible value of the substitution template before traffic is
-- allowed to your endpoint URL.
httpAction_confirmationUrl :: Lens.Lens' HttpAction (Prelude.Maybe Prelude.Text)
httpAction_confirmationUrl = Lens.lens (\HttpAction' {confirmationUrl} -> confirmationUrl) (\s@HttpAction' {} a -> s {confirmationUrl = a} :: HttpAction)

-- | The endpoint URL. If substitution templates are used in the URL, you
-- must also specify a @confirmationUrl@. If this is a new destination, a
-- new @TopicRuleDestination@ is created if possible.
httpAction_url :: Lens.Lens' HttpAction Prelude.Text
httpAction_url = Lens.lens (\HttpAction' {url} -> url) (\s@HttpAction' {} a -> s {url = a} :: HttpAction)

instance Prelude.FromJSON HttpAction where
  parseJSON =
    Prelude.withObject
      "HttpAction"
      ( \x ->
          HttpAction'
            Prelude.<$> (x Prelude..:? "headers" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "auth")
            Prelude.<*> (x Prelude..:? "confirmationUrl")
            Prelude.<*> (x Prelude..: "url")
      )

instance Prelude.Hashable HttpAction

instance Prelude.NFData HttpAction

instance Prelude.ToJSON HttpAction where
  toJSON HttpAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("headers" Prelude..=) Prelude.<$> headers,
            ("auth" Prelude..=) Prelude.<$> auth,
            ("confirmationUrl" Prelude..=)
              Prelude.<$> confirmationUrl,
            Prelude.Just ("url" Prelude..= url)
          ]
      )
