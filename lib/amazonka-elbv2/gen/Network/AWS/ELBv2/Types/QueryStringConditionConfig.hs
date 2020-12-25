{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.QueryStringConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.QueryStringConditionConfig
  ( QueryStringConditionConfig (..),

    -- * Smart constructor
    mkQueryStringConditionConfig,

    -- * Lenses
    qsccValues,
  )
where

import qualified Network.AWS.ELBv2.Types.QueryStringKeyValuePair as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a query string condition.
--
-- The query string component of a URI starts after the first '?' character and is terminated by either a '#' character or the end of the URI. A typical query string contains key/value pairs separated by '&' characters. The allowed characters are specified by RFC 3986. Any character can be percentage encoded.
--
-- /See:/ 'mkQueryStringConditionConfig' smart constructor.
newtype QueryStringConditionConfig = QueryStringConditionConfig'
  { -- | One or more key/value pairs or values to find in the query string. The maximum size of each string is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). To search for a literal '*' or '?' character in a query string, you must escape these characters in @Values@ using a '\' character.
    --
    -- If you specify multiple key/value pairs or values, the condition is satisfied if one of them is found in the query string.
    values :: Core.Maybe [Types.QueryStringKeyValuePair]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'QueryStringConditionConfig' value with any optional fields omitted.
mkQueryStringConditionConfig ::
  QueryStringConditionConfig
mkQueryStringConditionConfig =
  QueryStringConditionConfig' {values = Core.Nothing}

-- | One or more key/value pairs or values to find in the query string. The maximum size of each string is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). To search for a literal '*' or '?' character in a query string, you must escape these characters in @Values@ using a '\' character.
--
-- If you specify multiple key/value pairs or values, the condition is satisfied if one of them is found in the query string.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsccValues :: Lens.Lens' QueryStringConditionConfig (Core.Maybe [Types.QueryStringKeyValuePair])
qsccValues = Lens.field @"values"
{-# DEPRECATED qsccValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromXML QueryStringConditionConfig where
  parseXML x =
    QueryStringConditionConfig'
      Core.<$> (x Core..@? "Values" Core..<@> Core.parseXMLList "member")
