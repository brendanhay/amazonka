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

import Network.AWS.ELBv2.Types.QueryStringKeyValuePair
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a query string condition.
--
-- The query string component of a URI starts after the first '?' character and is terminated by either a '#' character or the end of the URI. A typical query string contains key/value pairs separated by '&' characters. The allowed characters are specified by RFC 3986. Any character can be percentage encoded.
--
-- /See:/ 'mkQueryStringConditionConfig' smart constructor.
newtype QueryStringConditionConfig = QueryStringConditionConfig'
  { values ::
      Lude.Maybe
        [QueryStringKeyValuePair]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryStringConditionConfig' with the minimum fields required to make a request.
--
-- * 'values' - One or more key/value pairs or values to find in the query string. The maximum size of each string is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). To search for a literal '*' or '?' character in a query string, you must escape these characters in @Values@ using a '\' character.
--
-- If you specify multiple key/value pairs or values, the condition is satisfied if one of them is found in the query string.
mkQueryStringConditionConfig ::
  QueryStringConditionConfig
mkQueryStringConditionConfig =
  QueryStringConditionConfig' {values = Lude.Nothing}

-- | One or more key/value pairs or values to find in the query string. The maximum size of each string is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). To search for a literal '*' or '?' character in a query string, you must escape these characters in @Values@ using a '\' character.
--
-- If you specify multiple key/value pairs or values, the condition is satisfied if one of them is found in the query string.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsccValues :: Lens.Lens' QueryStringConditionConfig (Lude.Maybe [QueryStringKeyValuePair])
qsccValues = Lens.lens (values :: QueryStringConditionConfig -> Lude.Maybe [QueryStringKeyValuePair]) (\s a -> s {values = a} :: QueryStringConditionConfig)
{-# DEPRECATED qsccValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.FromXML QueryStringConditionConfig where
  parseXML x =
    QueryStringConditionConfig'
      Lude.<$> ( x Lude..@? "Values" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )

instance Lude.ToQuery QueryStringConditionConfig where
  toQuery QueryStringConditionConfig' {..} =
    Lude.mconcat
      [ "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values)
      ]
