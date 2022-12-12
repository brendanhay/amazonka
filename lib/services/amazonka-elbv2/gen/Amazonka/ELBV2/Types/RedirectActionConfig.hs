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
-- Module      : Amazonka.ELBV2.Types.RedirectActionConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.RedirectActionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types.RedirectActionStatusCodeEnum
import qualified Amazonka.Prelude as Prelude

-- | Information about a redirect action.
--
-- A URI consists of the following components:
-- protocol:\/\/hostname:port\/path?query. You must modify at least one of
-- the following components to avoid a redirect loop: protocol, hostname,
-- port, or path. Any components that you do not modify retain their
-- original values.
--
-- You can reuse URI components using the following reserved keywords:
--
-- -   #{protocol}
--
-- -   #{host}
--
-- -   #{port}
--
-- -   #{path} (the leading \"\/\" is removed)
--
-- -   #{query}
--
-- For example, you can change the path to \"\/new\/#{path}\", the hostname
-- to \"example.#{host}\", or the query to \"#{query}&value=xyz\".
--
-- /See:/ 'newRedirectActionConfig' smart constructor.
data RedirectActionConfig = RedirectActionConfig'
  { -- | The hostname. This component is not percent-encoded. The hostname can
    -- contain #{host}.
    host :: Prelude.Maybe Prelude.Text,
    -- | The absolute path, starting with the leading \"\/\". This component is
    -- not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
    path :: Prelude.Maybe Prelude.Text,
    -- | The port. You can specify a value from 1 to 65535 or #{port}.
    port :: Prelude.Maybe Prelude.Text,
    -- | The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can
    -- redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot
    -- redirect HTTPS to HTTP.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The query parameters, URL-encoded when necessary, but not
    -- percent-encoded. Do not include the leading \"?\", as it is
    -- automatically added. You can specify any of the reserved keywords.
    query :: Prelude.Maybe Prelude.Text,
    -- | The HTTP redirect code. The redirect is either permanent (HTTP 301) or
    -- temporary (HTTP 302).
    statusCode :: RedirectActionStatusCodeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedirectActionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'redirectActionConfig_host' - The hostname. This component is not percent-encoded. The hostname can
-- contain #{host}.
--
-- 'path', 'redirectActionConfig_path' - The absolute path, starting with the leading \"\/\". This component is
-- not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
--
-- 'port', 'redirectActionConfig_port' - The port. You can specify a value from 1 to 65535 or #{port}.
--
-- 'protocol', 'redirectActionConfig_protocol' - The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can
-- redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot
-- redirect HTTPS to HTTP.
--
-- 'query', 'redirectActionConfig_query' - The query parameters, URL-encoded when necessary, but not
-- percent-encoded. Do not include the leading \"?\", as it is
-- automatically added. You can specify any of the reserved keywords.
--
-- 'statusCode', 'redirectActionConfig_statusCode' - The HTTP redirect code. The redirect is either permanent (HTTP 301) or
-- temporary (HTTP 302).
newRedirectActionConfig ::
  -- | 'statusCode'
  RedirectActionStatusCodeEnum ->
  RedirectActionConfig
newRedirectActionConfig pStatusCode_ =
  RedirectActionConfig'
    { host = Prelude.Nothing,
      path = Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing,
      query = Prelude.Nothing,
      statusCode = pStatusCode_
    }

-- | The hostname. This component is not percent-encoded. The hostname can
-- contain #{host}.
redirectActionConfig_host :: Lens.Lens' RedirectActionConfig (Prelude.Maybe Prelude.Text)
redirectActionConfig_host = Lens.lens (\RedirectActionConfig' {host} -> host) (\s@RedirectActionConfig' {} a -> s {host = a} :: RedirectActionConfig)

-- | The absolute path, starting with the leading \"\/\". This component is
-- not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
redirectActionConfig_path :: Lens.Lens' RedirectActionConfig (Prelude.Maybe Prelude.Text)
redirectActionConfig_path = Lens.lens (\RedirectActionConfig' {path} -> path) (\s@RedirectActionConfig' {} a -> s {path = a} :: RedirectActionConfig)

-- | The port. You can specify a value from 1 to 65535 or #{port}.
redirectActionConfig_port :: Lens.Lens' RedirectActionConfig (Prelude.Maybe Prelude.Text)
redirectActionConfig_port = Lens.lens (\RedirectActionConfig' {port} -> port) (\s@RedirectActionConfig' {} a -> s {port = a} :: RedirectActionConfig)

-- | The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can
-- redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot
-- redirect HTTPS to HTTP.
redirectActionConfig_protocol :: Lens.Lens' RedirectActionConfig (Prelude.Maybe Prelude.Text)
redirectActionConfig_protocol = Lens.lens (\RedirectActionConfig' {protocol} -> protocol) (\s@RedirectActionConfig' {} a -> s {protocol = a} :: RedirectActionConfig)

-- | The query parameters, URL-encoded when necessary, but not
-- percent-encoded. Do not include the leading \"?\", as it is
-- automatically added. You can specify any of the reserved keywords.
redirectActionConfig_query :: Lens.Lens' RedirectActionConfig (Prelude.Maybe Prelude.Text)
redirectActionConfig_query = Lens.lens (\RedirectActionConfig' {query} -> query) (\s@RedirectActionConfig' {} a -> s {query = a} :: RedirectActionConfig)

-- | The HTTP redirect code. The redirect is either permanent (HTTP 301) or
-- temporary (HTTP 302).
redirectActionConfig_statusCode :: Lens.Lens' RedirectActionConfig RedirectActionStatusCodeEnum
redirectActionConfig_statusCode = Lens.lens (\RedirectActionConfig' {statusCode} -> statusCode) (\s@RedirectActionConfig' {} a -> s {statusCode = a} :: RedirectActionConfig)

instance Data.FromXML RedirectActionConfig where
  parseXML x =
    RedirectActionConfig'
      Prelude.<$> (x Data..@? "Host")
      Prelude.<*> (x Data..@? "Path")
      Prelude.<*> (x Data..@? "Port")
      Prelude.<*> (x Data..@? "Protocol")
      Prelude.<*> (x Data..@? "Query")
      Prelude.<*> (x Data..@ "StatusCode")

instance Prelude.Hashable RedirectActionConfig where
  hashWithSalt _salt RedirectActionConfig' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` query
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData RedirectActionConfig where
  rnf RedirectActionConfig' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf query
      `Prelude.seq` Prelude.rnf statusCode

instance Data.ToQuery RedirectActionConfig where
  toQuery RedirectActionConfig' {..} =
    Prelude.mconcat
      [ "Host" Data.=: host,
        "Path" Data.=: path,
        "Port" Data.=: port,
        "Protocol" Data.=: protocol,
        "Query" Data.=: query,
        "StatusCode" Data.=: statusCode
      ]
