{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.HttpHeaderConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.HttpHeaderConditionConfig
  ( HttpHeaderConditionConfig (..)
  -- * Smart constructor
  , mkHttpHeaderConditionConfig
  -- * Lenses
  , hHttpHeaderName
  , hValues
  ) where

import qualified Network.AWS.ELBv2.Types.HttpHeaderConditionName as Types
import qualified Network.AWS.ELBv2.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an HTTP header condition.
--
-- There is a set of standard HTTP header fields. You can also define custom HTTP header fields.
--
-- /See:/ 'mkHttpHeaderConditionConfig' smart constructor.
data HttpHeaderConditionConfig = HttpHeaderConditionConfig'
  { httpHeaderName :: Core.Maybe Types.HttpHeaderConditionName
    -- ^ The name of the HTTP header field. The maximum size is 40 characters. The header name is case insensitive. The allowed characters are specified by RFC 7230. Wildcards are not supported.
--
-- You can't use an HTTP header condition to specify the host header. Use 'HostHeaderConditionConfig' to specify a host header condition.
  , values :: Core.Maybe [Types.StringValue]
    -- ^ One or more strings to compare against the value of the HTTP header. The maximum size of each string is 128 characters. The comparison strings are case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If the same header appears multiple times in the request, we search them in order until a match is found.
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the value of the HTTP header. To require that all of the strings are a match, create one condition per string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpHeaderConditionConfig' value with any optional fields omitted.
mkHttpHeaderConditionConfig
    :: HttpHeaderConditionConfig
mkHttpHeaderConditionConfig
  = HttpHeaderConditionConfig'{httpHeaderName = Core.Nothing,
                               values = Core.Nothing}

-- | The name of the HTTP header field. The maximum size is 40 characters. The header name is case insensitive. The allowed characters are specified by RFC 7230. Wildcards are not supported.
--
-- You can't use an HTTP header condition to specify the host header. Use 'HostHeaderConditionConfig' to specify a host header condition.
--
-- /Note:/ Consider using 'httpHeaderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHttpHeaderName :: Lens.Lens' HttpHeaderConditionConfig (Core.Maybe Types.HttpHeaderConditionName)
hHttpHeaderName = Lens.field @"httpHeaderName"
{-# INLINEABLE hHttpHeaderName #-}
{-# DEPRECATED httpHeaderName "Use generic-lens or generic-optics with 'httpHeaderName' instead"  #-}

-- | One or more strings to compare against the value of the HTTP header. The maximum size of each string is 128 characters. The comparison strings are case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If the same header appears multiple times in the request, we search them in order until a match is found.
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the value of the HTTP header. To require that all of the strings are a match, create one condition per string.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hValues :: Lens.Lens' HttpHeaderConditionConfig (Core.Maybe [Types.StringValue])
hValues = Lens.field @"values"
{-# INLINEABLE hValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery HttpHeaderConditionConfig where
        toQuery HttpHeaderConditionConfig{..}
          = Core.maybe Core.mempty (Core.toQueryPair "HttpHeaderName")
              httpHeaderName
              Core.<>
              Core.toQueryPair "Values"
                (Core.maybe Core.mempty (Core.toQueryList "member") values)

instance Core.FromXML HttpHeaderConditionConfig where
        parseXML x
          = HttpHeaderConditionConfig' Core.<$>
              (x Core..@? "HttpHeaderName") Core.<*>
                x Core..@? "Values" Core..<@> Core.parseXMLList "member"
