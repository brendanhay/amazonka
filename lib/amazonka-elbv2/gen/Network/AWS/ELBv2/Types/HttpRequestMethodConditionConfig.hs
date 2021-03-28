{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.HttpRequestMethodConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.HttpRequestMethodConditionConfig
  ( HttpRequestMethodConditionConfig (..)
  -- * Smart constructor
  , mkHttpRequestMethodConditionConfig
  -- * Lenses
  , hrmccValues
  ) where

import qualified Network.AWS.ELBv2.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an HTTP method condition.
--
-- HTTP defines a set of request methods, also referred to as HTTP verbs. For more information, see the <https://www.iana.org/assignments/http-methods/http-methods.xhtml HTTP Method Registry> . You can also define custom HTTP methods.
--
-- /See:/ 'mkHttpRequestMethodConditionConfig' smart constructor.
newtype HttpRequestMethodConditionConfig = HttpRequestMethodConditionConfig'
  { values :: Core.Maybe [Types.StringValue]
    -- ^ The name of the request method. The maximum size is 40 characters. The allowed characters are A-Z, hyphen (-), and underscore (_). The comparison is case sensitive. Wildcards are not supported; therefore, the method name must be an exact match.
--
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the HTTP request method. We recommend that you route GET and HEAD requests in the same way, because the response to a HEAD request may be cached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HttpRequestMethodConditionConfig' value with any optional fields omitted.
mkHttpRequestMethodConditionConfig
    :: HttpRequestMethodConditionConfig
mkHttpRequestMethodConditionConfig
  = HttpRequestMethodConditionConfig'{values = Core.Nothing}

-- | The name of the request method. The maximum size is 40 characters. The allowed characters are A-Z, hyphen (-), and underscore (_). The comparison is case sensitive. Wildcards are not supported; therefore, the method name must be an exact match.
--
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the HTTP request method. We recommend that you route GET and HEAD requests in the same way, because the response to a HEAD request may be cached.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrmccValues :: Lens.Lens' HttpRequestMethodConditionConfig (Core.Maybe [Types.StringValue])
hrmccValues = Lens.field @"values"
{-# INLINEABLE hrmccValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery HttpRequestMethodConditionConfig where
        toQuery HttpRequestMethodConditionConfig{..}
          = Core.toQueryPair "Values"
              (Core.maybe Core.mempty (Core.toQueryList "member") values)

instance Core.FromXML HttpRequestMethodConditionConfig where
        parseXML x
          = HttpRequestMethodConditionConfig' Core.<$>
              (x Core..@? "Values" Core..<@> Core.parseXMLList "member")
