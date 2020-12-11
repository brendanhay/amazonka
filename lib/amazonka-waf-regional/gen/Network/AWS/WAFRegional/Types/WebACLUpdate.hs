-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WebACLUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WebACLUpdate
  ( WebACLUpdate (..),

    -- * Smart constructor
    mkWebACLUpdate,

    -- * Lenses
    wauAction,
    wauActivatedRule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.ActivatedRule
import Network.AWS.WAFRegional.Types.ChangeAction

-- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
--
-- /See:/ 'mkWebACLUpdate' smart constructor.
data WebACLUpdate = WebACLUpdate'
  { action :: ChangeAction,
    activatedRule :: ActivatedRule
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WebACLUpdate' with the minimum fields required to make a request.
--
-- * 'action' - Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
-- * 'activatedRule' - The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
mkWebACLUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'activatedRule'
  ActivatedRule ->
  WebACLUpdate
mkWebACLUpdate pAction_ pActivatedRule_ =
  WebACLUpdate' {action = pAction_, activatedRule = pActivatedRule_}

-- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wauAction :: Lens.Lens' WebACLUpdate ChangeAction
wauAction = Lens.lens (action :: WebACLUpdate -> ChangeAction) (\s a -> s {action = a} :: WebACLUpdate)
{-# DEPRECATED wauAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
--
-- /Note:/ Consider using 'activatedRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wauActivatedRule :: Lens.Lens' WebACLUpdate ActivatedRule
wauActivatedRule = Lens.lens (activatedRule :: WebACLUpdate -> ActivatedRule) (\s a -> s {activatedRule = a} :: WebACLUpdate)
{-# DEPRECATED wauActivatedRule "Use generic-lens or generic-optics with 'activatedRule' instead." #-}

instance Lude.ToJSON WebACLUpdate where
  toJSON WebACLUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("ActivatedRule" Lude..= activatedRule)
          ]
      )
