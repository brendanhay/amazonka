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
    racStatusCode,
    racHost,
    racPath,
    racPort,
    racProtocol,
    racQuery,
  )
where

import qualified Network.AWS.ELBv2.Types.RedirectActionHost as Types
import qualified Network.AWS.ELBv2.Types.RedirectActionPath as Types
import qualified Network.AWS.ELBv2.Types.RedirectActionPort as Types
import qualified Network.AWS.ELBv2.Types.RedirectActionProtocol as Types
import qualified Network.AWS.ELBv2.Types.RedirectActionQuery as Types
import qualified Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { -- | The HTTP redirect code. The redirect is either permanent (HTTP 301) or temporary (HTTP 302).
    statusCode :: Types.RedirectActionStatusCodeEnum,
    -- | The hostname. This component is not percent-encoded. The hostname can contain #{host}.
    host :: Core.Maybe Types.RedirectActionHost,
    -- | The absolute path, starting with the leading "/". This component is not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
    path :: Core.Maybe Types.RedirectActionPath,
    -- | The port. You can specify a value from 1 to 65535 or #{port}.
    port :: Core.Maybe Types.RedirectActionPort,
    -- | The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot redirect HTTPS to HTTP.
    protocol :: Core.Maybe Types.RedirectActionProtocol,
    -- | The query parameters, URL-encoded when necessary, but not percent-encoded. Do not include the leading "?", as it is automatically added. You can specify any of the reserved keywords.
    query :: Core.Maybe Types.RedirectActionQuery
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedirectActionConfig' value with any optional fields omitted.
mkRedirectActionConfig ::
  -- | 'statusCode'
  Types.RedirectActionStatusCodeEnum ->
  RedirectActionConfig
mkRedirectActionConfig statusCode =
  RedirectActionConfig'
    { statusCode,
      host = Core.Nothing,
      path = Core.Nothing,
      port = Core.Nothing,
      protocol = Core.Nothing,
      query = Core.Nothing
    }

-- | The HTTP redirect code. The redirect is either permanent (HTTP 301) or temporary (HTTP 302).
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racStatusCode :: Lens.Lens' RedirectActionConfig Types.RedirectActionStatusCodeEnum
racStatusCode = Lens.field @"statusCode"
{-# DEPRECATED racStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The hostname. This component is not percent-encoded. The hostname can contain #{host}.
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racHost :: Lens.Lens' RedirectActionConfig (Core.Maybe Types.RedirectActionHost)
racHost = Lens.field @"host"
{-# DEPRECATED racHost "Use generic-lens or generic-optics with 'host' instead." #-}

-- | The absolute path, starting with the leading "/". This component is not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racPath :: Lens.Lens' RedirectActionConfig (Core.Maybe Types.RedirectActionPath)
racPath = Lens.field @"path"
{-# DEPRECATED racPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The port. You can specify a value from 1 to 65535 or #{port}.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racPort :: Lens.Lens' RedirectActionConfig (Core.Maybe Types.RedirectActionPort)
racPort = Lens.field @"port"
{-# DEPRECATED racPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot redirect HTTPS to HTTP.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racProtocol :: Lens.Lens' RedirectActionConfig (Core.Maybe Types.RedirectActionProtocol)
racProtocol = Lens.field @"protocol"
{-# DEPRECATED racProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The query parameters, URL-encoded when necessary, but not percent-encoded. Do not include the leading "?", as it is automatically added. You can specify any of the reserved keywords.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
racQuery :: Lens.Lens' RedirectActionConfig (Core.Maybe Types.RedirectActionQuery)
racQuery = Lens.field @"query"
{-# DEPRECATED racQuery "Use generic-lens or generic-optics with 'query' instead." #-}

instance Core.FromXML RedirectActionConfig where
  parseXML x =
    RedirectActionConfig'
      Core.<$> (x Core..@ "StatusCode")
      Core.<*> (x Core..@? "Host")
      Core.<*> (x Core..@? "Path")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "Protocol")
      Core.<*> (x Core..@? "Query")
