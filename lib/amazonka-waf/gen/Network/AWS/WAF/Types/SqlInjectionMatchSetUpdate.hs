{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SqlInjectionMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SqlInjectionMatchSetUpdate
  ( SqlInjectionMatchSetUpdate (..),

    -- * Smart constructor
    mkSqlInjectionMatchSetUpdate,

    -- * Lenses
    simsuSqlInjectionMatchTuple,
    simsuAction,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.SqlInjectionMatchTuple

-- | Specifies the part of a web request that you want to inspect for snippets of malicious SQL code and indicates whether you want to add the specification to a 'SqlInjectionMatchSet' or delete it from a @SqlInjectionMatchSet@ .
--
-- /See:/ 'mkSqlInjectionMatchSetUpdate' smart constructor.
data SqlInjectionMatchSetUpdate = SqlInjectionMatchSetUpdate'
  { -- | Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
    sqlInjectionMatchTuple :: SqlInjectionMatchTuple,
    -- | Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
    action :: ChangeAction
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SqlInjectionMatchSetUpdate' with the minimum fields required to make a request.
--
-- * 'sqlInjectionMatchTuple' - Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
-- * 'action' - Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
mkSqlInjectionMatchSetUpdate ::
  -- | 'sqlInjectionMatchTuple'
  SqlInjectionMatchTuple ->
  -- | 'action'
  ChangeAction ->
  SqlInjectionMatchSetUpdate
mkSqlInjectionMatchSetUpdate pSqlInjectionMatchTuple_ pAction_ =
  SqlInjectionMatchSetUpdate'
    { sqlInjectionMatchTuple =
        pSqlInjectionMatchTuple_,
      action = pAction_
    }

-- | Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
--
-- /Note:/ Consider using 'sqlInjectionMatchTuple' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsuSqlInjectionMatchTuple :: Lens.Lens' SqlInjectionMatchSetUpdate SqlInjectionMatchTuple
simsuSqlInjectionMatchTuple = Lens.lens (sqlInjectionMatchTuple :: SqlInjectionMatchSetUpdate -> SqlInjectionMatchTuple) (\s a -> s {sqlInjectionMatchTuple = a} :: SqlInjectionMatchSetUpdate)
{-# DEPRECATED simsuSqlInjectionMatchTuple "Use generic-lens or generic-optics with 'sqlInjectionMatchTuple' instead." #-}

-- | Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsuAction :: Lens.Lens' SqlInjectionMatchSetUpdate ChangeAction
simsuAction = Lens.lens (action :: SqlInjectionMatchSetUpdate -> ChangeAction) (\s a -> s {action = a} :: SqlInjectionMatchSetUpdate)
{-# DEPRECATED simsuAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Lude.ToJSON SqlInjectionMatchSetUpdate where
  toJSON SqlInjectionMatchSetUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("SqlInjectionMatchTuple" Lude..= sqlInjectionMatchTuple),
            Lude.Just ("Action" Lude..= action)
          ]
      )
