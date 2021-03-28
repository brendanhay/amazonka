{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate
  ( SqlInjectionMatchSetUpdate (..)
  -- * Smart constructor
  , mkSqlInjectionMatchSetUpdate
  -- * Lenses
  , simsuAction
  , simsuSqlInjectionMatchTuple
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ChangeAction as Types
import qualified Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple as Types

-- | Specifies the part of a web request that you want to inspect for snippets of malicious SQL code and indicates whether you want to add the specification to a 'SqlInjectionMatchSet' or delete it from a @SqlInjectionMatchSet@ .
--
-- /See:/ 'mkSqlInjectionMatchSetUpdate' smart constructor.
data SqlInjectionMatchSetUpdate = SqlInjectionMatchSetUpdate'
  { action :: Types.ChangeAction
    -- ^ Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
  , sqlInjectionMatchTuple :: Types.SqlInjectionMatchTuple
    -- ^ Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SqlInjectionMatchSetUpdate' value with any optional fields omitted.
mkSqlInjectionMatchSetUpdate
    :: Types.ChangeAction -- ^ 'action'
    -> Types.SqlInjectionMatchTuple -- ^ 'sqlInjectionMatchTuple'
    -> SqlInjectionMatchSetUpdate
mkSqlInjectionMatchSetUpdate action sqlInjectionMatchTuple
  = SqlInjectionMatchSetUpdate'{action, sqlInjectionMatchTuple}

-- | Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsuAction :: Lens.Lens' SqlInjectionMatchSetUpdate Types.ChangeAction
simsuAction = Lens.field @"action"
{-# INLINEABLE simsuAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
--
-- /Note:/ Consider using 'sqlInjectionMatchTuple' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsuSqlInjectionMatchTuple :: Lens.Lens' SqlInjectionMatchSetUpdate Types.SqlInjectionMatchTuple
simsuSqlInjectionMatchTuple = Lens.field @"sqlInjectionMatchTuple"
{-# INLINEABLE simsuSqlInjectionMatchTuple #-}
{-# DEPRECATED sqlInjectionMatchTuple "Use generic-lens or generic-optics with 'sqlInjectionMatchTuple' instead"  #-}

instance Core.FromJSON SqlInjectionMatchSetUpdate where
        toJSON SqlInjectionMatchSetUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Action" Core..= action),
                  Core.Just
                    ("SqlInjectionMatchTuple" Core..= sqlInjectionMatchTuple)])
