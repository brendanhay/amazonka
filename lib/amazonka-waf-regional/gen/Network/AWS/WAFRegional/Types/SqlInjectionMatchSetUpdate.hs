-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate
  ( SqlInjectionMatchSetUpdate (..),

    -- * Smart constructor
    mkSqlInjectionMatchSetUpdate,

    -- * Lenses
    simsuAction,
    simsuSqlInjectionMatchTuple,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.ChangeAction
import Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple

-- | Specifies the part of a web request that you want to inspect for snippets of malicious SQL code and indicates whether you want to add the specification to a 'SqlInjectionMatchSet' or delete it from a @SqlInjectionMatchSet@ .
--
-- /See:/ 'mkSqlInjectionMatchSetUpdate' smart constructor.
data SqlInjectionMatchSetUpdate = SqlInjectionMatchSetUpdate'
  { action ::
      ChangeAction,
    sqlInjectionMatchTuple ::
      SqlInjectionMatchTuple
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SqlInjectionMatchSetUpdate' with the minimum fields required to make a request.
--
-- * 'action' - Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
-- * 'sqlInjectionMatchTuple' - Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
mkSqlInjectionMatchSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'sqlInjectionMatchTuple'
  SqlInjectionMatchTuple ->
  SqlInjectionMatchSetUpdate
mkSqlInjectionMatchSetUpdate pAction_ pSqlInjectionMatchTuple_ =
  SqlInjectionMatchSetUpdate'
    { action = pAction_,
      sqlInjectionMatchTuple = pSqlInjectionMatchTuple_
    }

-- | Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsuAction :: Lens.Lens' SqlInjectionMatchSetUpdate ChangeAction
simsuAction = Lens.lens (action :: SqlInjectionMatchSetUpdate -> ChangeAction) (\s a -> s {action = a} :: SqlInjectionMatchSetUpdate)
{-# DEPRECATED simsuAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
--
-- /Note:/ Consider using 'sqlInjectionMatchTuple' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsuSqlInjectionMatchTuple :: Lens.Lens' SqlInjectionMatchSetUpdate SqlInjectionMatchTuple
simsuSqlInjectionMatchTuple = Lens.lens (sqlInjectionMatchTuple :: SqlInjectionMatchSetUpdate -> SqlInjectionMatchTuple) (\s a -> s {sqlInjectionMatchTuple = a} :: SqlInjectionMatchSetUpdate)
{-# DEPRECATED simsuSqlInjectionMatchTuple "Use generic-lens or generic-optics with 'sqlInjectionMatchTuple' instead." #-}

instance Lude.ToJSON SqlInjectionMatchSetUpdate where
  toJSON SqlInjectionMatchSetUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just
              ("SqlInjectionMatchTuple" Lude..= sqlInjectionMatchTuple)
          ]
      )
