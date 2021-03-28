{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.XssMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.XssMatchSetUpdate
  ( XssMatchSetUpdate (..)
  -- * Smart constructor
  , mkXssMatchSetUpdate
  -- * Lenses
  , xmsuAction
  , xmsuXssMatchTuple
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ChangeAction as Types
import qualified Network.AWS.WAFRegional.Types.XssMatchTuple as Types

-- | Specifies the part of a web request that you want to inspect for cross-site scripting attacks and indicates whether you want to add the specification to an 'XssMatchSet' or delete it from an @XssMatchSet@ .
--
-- /See:/ 'mkXssMatchSetUpdate' smart constructor.
data XssMatchSetUpdate = XssMatchSetUpdate'
  { action :: Types.ChangeAction
    -- ^ Specify @INSERT@ to add an 'XssMatchSetUpdate' to an 'XssMatchSet' . Use @DELETE@ to remove an @XssMatchSetUpdate@ from an @XssMatchSet@ .
  , xssMatchTuple :: Types.XssMatchTuple
    -- ^ Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'XssMatchSetUpdate' value with any optional fields omitted.
mkXssMatchSetUpdate
    :: Types.ChangeAction -- ^ 'action'
    -> Types.XssMatchTuple -- ^ 'xssMatchTuple'
    -> XssMatchSetUpdate
mkXssMatchSetUpdate action xssMatchTuple
  = XssMatchSetUpdate'{action, xssMatchTuple}

-- | Specify @INSERT@ to add an 'XssMatchSetUpdate' to an 'XssMatchSet' . Use @DELETE@ to remove an @XssMatchSetUpdate@ from an @XssMatchSet@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmsuAction :: Lens.Lens' XssMatchSetUpdate Types.ChangeAction
xmsuAction = Lens.field @"action"
{-# INLINEABLE xmsuAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.
--
-- /Note:/ Consider using 'xssMatchTuple' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmsuXssMatchTuple :: Lens.Lens' XssMatchSetUpdate Types.XssMatchTuple
xmsuXssMatchTuple = Lens.field @"xssMatchTuple"
{-# INLINEABLE xmsuXssMatchTuple #-}
{-# DEPRECATED xssMatchTuple "Use generic-lens or generic-optics with 'xssMatchTuple' instead"  #-}

instance Core.FromJSON XssMatchSetUpdate where
        toJSON XssMatchSetUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Action" Core..= action),
                  Core.Just ("XssMatchTuple" Core..= xssMatchTuple)])
