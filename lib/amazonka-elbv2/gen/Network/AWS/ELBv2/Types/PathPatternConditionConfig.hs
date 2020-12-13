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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a path pattern condition.
--
-- /See:/ 'mkPathPatternConditionConfig' smart constructor.
newtype PathPatternConditionConfig = PathPatternConditionConfig'
  { -- | One or more path patterns to compare against the request URL. The maximum size of each string is 128 characters. The comparison is case sensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
    --
    -- If you specify multiple strings, the condition is satisfied if one of them matches the request URL. The path pattern is compared only to the path of the URL, not to its query string. To compare against the query string, use 'QueryStringConditionConfig' .
    values :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PathPatternConditionConfig' with the minimum fields required to make a request.
--
-- * 'values' - One or more path patterns to compare against the request URL. The maximum size of each string is 128 characters. The comparison is case sensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of them matches the request URL. The path pattern is compared only to the path of the URL, not to its query string. To compare against the query string, use 'QueryStringConditionConfig' .
mkPathPatternConditionConfig ::
  PathPatternConditionConfig
mkPathPatternConditionConfig =
  PathPatternConditionConfig' {values = Lude.Nothing}

-- | One or more path patterns to compare against the request URL. The maximum size of each string is 128 characters. The comparison is case sensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of them matches the request URL. The path pattern is compared only to the path of the URL, not to its query string. To compare against the query string, use 'QueryStringConditionConfig' .
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccValues :: Lens.Lens' PathPatternConditionConfig (Lude.Maybe [Lude.Text])
ppccValues = Lens.lens (values :: PathPatternConditionConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: PathPatternConditionConfig)
{-# DEPRECATED ppccValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.FromXML PathPatternConditionConfig where
  parseXML x =
    PathPatternConditionConfig'
      Lude.<$> ( x Lude..@? "Values" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )

instance Lude.ToQuery PathPatternConditionConfig where
  toQuery PathPatternConditionConfig' {..} =
    Lude.mconcat
      [ "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values)
      ]
