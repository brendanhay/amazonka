{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RedirectActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.RedirectActionConfig
  ( RedirectActionConfig (..),

    -- * Smart constructor
    mkRedirectActionConfig,

    -- * Lenses
    racPath,
    racProtocol,
    racQuery,
    racHost,
    racPort,
    racStatusCode,
  )
where

import Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a redirect action.
--
-- A URI consists of the following components: protocol://hostname:port/path?query. You must modify at least one of the following components to avoid a redirect loop: protocol, hostname, port, or path. Any components that you do not modify retain their original values.
-- You can reuse URI components using the following reserved keywords:
--
--     * #{protocol}
--
--
--     * #{host}
--
--
--     * #{port}
--
--
--     * #{path} (the leading "/" is removed)
--
--
--     * #{query}
--
--
-- For example, you can change the path to "/new/#{path}", the hostname to "example.#{host}", or the query to "#{query}&value=xyz".
--
-- /See:/ 'mkRedirectActionConfig' smart constructor.
data RedirectActionConfig = RedirectActionConfig'
  { -- | The absolute path, starting with the leading "/". This component is not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
    path :: Lude.Maybe Lude.Text,
    -- | The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot redirect HTTPS to HTTP.
    protocol :: Lude.Maybe Lude.Text,
    -- | The query parameters, URL-encoded when necessary, but not percent-encoded. Do not include the leading "?", as it is automatically added. You can specify any of the reserved keywords.
    query :: Lude.Maybe Lude.Text,
    -- | The hostname. This component is not percent-encoded. The hostname can contain #{host}.
    host :: Lude.Maybe Lude.Text,
    -- | The port. You can specify a value from 1 to 65535 or #{port}.
    port :: Lude.Maybe Lude.Text,
    -- | The HTTP redirect code. The redirect is either permanent (HTTP 301) or temporary (HTTP 302).
    statusCode :: RedirectActionStatusCodeEnum
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedirectActionConfig' with the minimum fields required to make a request.
--
-- * 'path' - The absolute path, starting with the leading "/". This component is not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
-- * 'protocol' - The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot redirect HTTPS to HTTP.
-- * 'query' - The query parameters, URL-encoded when necessary, but not percent-encoded. Do not include the leading "?", as it is automatically added. You can specify any of the reserved keywords.
-- * 'host' - The hostname. This component is not percent-encoded. The hostname can contain #{host}.
-- * 'port' - The port. You can specify a value from 1 to 65535 or #{port}.
-- * 'statusCode' - The HTTP redirect code. The redirect is either permanent (HTTP 301) or temporary (HTTP 302).
mkRedirectActionConfig ::
  -- | 'statusCode'
  RedirectActionStatusCodeEnum ->
  RedirectActionConfig
mkRedirectActionConfig pStatusCode_ =
  RedirectActionConfig'
    { path = Lude.Nothing,
      protocol = Lude.Nothing,
      query = Lude.Nothing,
      host = Lude.Nothing,
      port = Lude.Nothing,
      statusCode = pStatusCode_
    }

-- | The absolute path, starting with the leading "/". This component is not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racPath :: Lens.Lens' RedirectActionConfig (Lude.Maybe Lude.Text)
racPath = Lens.lens (path :: RedirectActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: RedirectActionConfig)
{-# DEPRECATED racPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot redirect HTTPS to HTTP.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racProtocol :: Lens.Lens' RedirectActionConfig (Lude.Maybe Lude.Text)
racProtocol = Lens.lens (protocol :: RedirectActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {protocol = a} :: RedirectActionConfig)
{-# DEPRECATED racProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The query parameters, URL-encoded when necessary, but not percent-encoded. Do not include the leading "?", as it is automatically added. You can specify any of the reserved keywords.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racQuery :: Lens.Lens' RedirectActionConfig (Lude.Maybe Lude.Text)
racQuery = Lens.lens (query :: RedirectActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {query = a} :: RedirectActionConfig)
{-# DEPRECATED racQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | The hostname. This component is not percent-encoded. The hostname can contain #{host}.
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racHost :: Lens.Lens' RedirectActionConfig (Lude.Maybe Lude.Text)
racHost = Lens.lens (host :: RedirectActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {host = a} :: RedirectActionConfig)
{-# DEPRECATED racHost "Use generic-lens or generic-optics with 'host' instead." #-}

-- | The port. You can specify a value from 1 to 65535 or #{port}.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racPort :: Lens.Lens' RedirectActionConfig (Lude.Maybe Lude.Text)
racPort = Lens.lens (port :: RedirectActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {port = a} :: RedirectActionConfig)
{-# DEPRECATED racPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The HTTP redirect code. The redirect is either permanent (HTTP 301) or temporary (HTTP 302).
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racStatusCode :: Lens.Lens' RedirectActionConfig RedirectActionStatusCodeEnum
racStatusCode = Lens.lens (statusCode :: RedirectActionConfig -> RedirectActionStatusCodeEnum) (\s a -> s {statusCode = a} :: RedirectActionConfig)
{-# DEPRECATED racStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromXML RedirectActionConfig where
  parseXML x =
    RedirectActionConfig'
      Lude.<$> (x Lude..@? "Path")
      Lude.<*> (x Lude..@? "Protocol")
      Lude.<*> (x Lude..@? "Query")
      Lude.<*> (x Lude..@? "Host")
      Lude.<*> (x Lude..@? "Port")
      Lude.<*> (x Lude..@ "StatusCode")

instance Lude.ToQuery RedirectActionConfig where
  toQuery RedirectActionConfig' {..} =
    Lude.mconcat
      [ "Path" Lude.=: path,
        "Protocol" Lude.=: protocol,
        "Query" Lude.=: query,
        "Host" Lude.=: host,
        "Port" Lude.=: port,
        "StatusCode" Lude.=: statusCode
      ]
