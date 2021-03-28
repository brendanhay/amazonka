{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.ByteMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.ByteMatchSetUpdate
  ( ByteMatchSetUpdate (..)
  -- * Smart constructor
  , mkByteMatchSetUpdate
  -- * Lenses
  , bmsuAction
  , bmsuByteMatchTuple
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ByteMatchTuple as Types
import qualified Network.AWS.WAF.Types.ChangeAction as Types

-- | In an 'UpdateByteMatchSet' request, @ByteMatchSetUpdate@ specifies whether to insert or delete a 'ByteMatchTuple' and includes the settings for the @ByteMatchTuple@ .
--
-- /See:/ 'mkByteMatchSetUpdate' smart constructor.
data ByteMatchSetUpdate = ByteMatchSetUpdate'
  { action :: Types.ChangeAction
    -- ^ Specifies whether to insert or delete a 'ByteMatchTuple' .
  , byteMatchTuple :: Types.ByteMatchTuple
    -- ^ Information about the part of a web request that you want AWS WAF to inspect and the value that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @ByteMatchTuple@ values must exactly match the values in the @ByteMatchTuple@ that you want to delete from the @ByteMatchSet@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ByteMatchSetUpdate' value with any optional fields omitted.
mkByteMatchSetUpdate
    :: Types.ChangeAction -- ^ 'action'
    -> Types.ByteMatchTuple -- ^ 'byteMatchTuple'
    -> ByteMatchSetUpdate
mkByteMatchSetUpdate action byteMatchTuple
  = ByteMatchSetUpdate'{action, byteMatchTuple}

-- | Specifies whether to insert or delete a 'ByteMatchTuple' .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsuAction :: Lens.Lens' ByteMatchSetUpdate Types.ChangeAction
bmsuAction = Lens.field @"action"
{-# INLINEABLE bmsuAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | Information about the part of a web request that you want AWS WAF to inspect and the value that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @ByteMatchTuple@ values must exactly match the values in the @ByteMatchTuple@ that you want to delete from the @ByteMatchSet@ .
--
-- /Note:/ Consider using 'byteMatchTuple' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsuByteMatchTuple :: Lens.Lens' ByteMatchSetUpdate Types.ByteMatchTuple
bmsuByteMatchTuple = Lens.field @"byteMatchTuple"
{-# INLINEABLE bmsuByteMatchTuple #-}
{-# DEPRECATED byteMatchTuple "Use generic-lens or generic-optics with 'byteMatchTuple' instead"  #-}

instance Core.FromJSON ByteMatchSetUpdate where
        toJSON ByteMatchSetUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Action" Core..= action),
                  Core.Just ("ByteMatchTuple" Core..= byteMatchTuple)])
