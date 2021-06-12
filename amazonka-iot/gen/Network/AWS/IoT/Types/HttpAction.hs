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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.HttpActionHeader
import Network.AWS.IoT.Types.HttpAuthorization
import qualified Network.AWS.Lens as Lens

-- | Send data to an HTTPS endpoint.
--
-- /See:/ 'newHttpAction' smart constructor.
data HttpAction = HttpAction'
  { -- | The HTTP headers to send with the message data.
    headers :: Core.Maybe [HttpActionHeader],
    -- | The authentication method to use when sending data to an HTTPS endpoint.
    auth :: Core.Maybe HttpAuthorization,
    -- | The URL to which AWS IoT sends a confirmation message. The value of the
    -- confirmation URL must be a prefix of the endpoint URL. If you do not
    -- specify a confirmation URL AWS IoT uses the endpoint URL as the
    -- confirmation URL. If you use substitution templates in the
    -- confirmationUrl, you must create and enable topic rule destinations that
    -- match each possible value of the substitution template before traffic is
    -- allowed to your endpoint URL.
    confirmationUrl :: Core.Maybe Core.Text,
    -- | The endpoint URL. If substitution templates are used in the URL, you
    -- must also specify a @confirmationUrl@. If this is a new destination, a
    -- new @TopicRuleDestination@ is created if possible.
    url :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  HttpAction
newHttpAction pUrl_ =
  HttpAction'
    { headers = Core.Nothing,
      auth = Core.Nothing,
      confirmationUrl = Core.Nothing,
      url = pUrl_
    }

-- | The HTTP headers to send with the message data.
httpAction_headers :: Lens.Lens' HttpAction (Core.Maybe [HttpActionHeader])
httpAction_headers = Lens.lens (\HttpAction' {headers} -> headers) (\s@HttpAction' {} a -> s {headers = a} :: HttpAction) Core.. Lens.mapping Lens._Coerce

-- | The authentication method to use when sending data to an HTTPS endpoint.
httpAction_auth :: Lens.Lens' HttpAction (Core.Maybe HttpAuthorization)
httpAction_auth = Lens.lens (\HttpAction' {auth} -> auth) (\s@HttpAction' {} a -> s {auth = a} :: HttpAction)

-- | The URL to which AWS IoT sends a confirmation message. The value of the
-- confirmation URL must be a prefix of the endpoint URL. If you do not
-- specify a confirmation URL AWS IoT uses the endpoint URL as the
-- confirmation URL. If you use substitution templates in the
-- confirmationUrl, you must create and enable topic rule destinations that
-- match each possible value of the substitution template before traffic is
-- allowed to your endpoint URL.
httpAction_confirmationUrl :: Lens.Lens' HttpAction (Core.Maybe Core.Text)
httpAction_confirmationUrl = Lens.lens (\HttpAction' {confirmationUrl} -> confirmationUrl) (\s@HttpAction' {} a -> s {confirmationUrl = a} :: HttpAction)

-- | The endpoint URL. If substitution templates are used in the URL, you
-- must also specify a @confirmationUrl@. If this is a new destination, a
-- new @TopicRuleDestination@ is created if possible.
httpAction_url :: Lens.Lens' HttpAction Core.Text
httpAction_url = Lens.lens (\HttpAction' {url} -> url) (\s@HttpAction' {} a -> s {url = a} :: HttpAction)

instance Core.FromJSON HttpAction where
  parseJSON =
    Core.withObject
      "HttpAction"
      ( \x ->
          HttpAction'
            Core.<$> (x Core..:? "headers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "auth")
            Core.<*> (x Core..:? "confirmationUrl")
            Core.<*> (x Core..: "url")
      )

instance Core.Hashable HttpAction

instance Core.NFData HttpAction

instance Core.ToJSON HttpAction where
  toJSON HttpAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("headers" Core..=) Core.<$> headers,
            ("auth" Core..=) Core.<$> auth,
            ("confirmationUrl" Core..=) Core.<$> confirmationUrl,
            Core.Just ("url" Core..= url)
          ]
      )
