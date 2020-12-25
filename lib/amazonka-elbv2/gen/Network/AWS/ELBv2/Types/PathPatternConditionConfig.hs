{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.PathPatternConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.PathPatternConditionConfig
  ( PathPatternConditionConfig (..),

    -- * Smart constructor
    mkPathPatternConditionConfig,

    -- * Lenses
    ppccValues,
  )
where

import qualified Network.AWS.ELBv2.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a path pattern condition.
--
-- /See:/ 'mkPathPatternConditionConfig' smart constructor.
newtype PathPatternConditionConfig = PathPatternConditionConfig'
  { -- | One or more path patterns to compare against the request URL. The maximum size of each string is 128 characters. The comparison is case sensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
    --
    -- If you specify multiple strings, the condition is satisfied if one of them matches the request URL. The path pattern is compared only to the path of the URL, not to its query string. To compare against the query string, use 'QueryStringConditionConfig' .
    values :: Core.Maybe [Types.StringValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PathPatternConditionConfig' value with any optional fields omitted.
mkPathPatternConditionConfig ::
  PathPatternConditionConfig
mkPathPatternConditionConfig =
  PathPatternConditionConfig' {values = Core.Nothing}

-- | One or more path patterns to compare against the request URL. The maximum size of each string is 128 characters. The comparison is case sensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of them matches the request URL. The path pattern is compared only to the path of the URL, not to its query string. To compare against the query string, use 'QueryStringConditionConfig' .
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccValues :: Lens.Lens' PathPatternConditionConfig (Core.Maybe [Types.StringValue])
ppccValues = Lens.field @"values"
{-# DEPRECATED ppccValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromXML PathPatternConditionConfig where
  parseXML x =
    PathPatternConditionConfig'
      Core.<$> (x Core..@? "Values" Core..<@> Core.parseXMLList "member")
