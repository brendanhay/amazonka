{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ByteMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ByteMatchSetUpdate
  ( ByteMatchSetUpdate (..),

    -- * Smart constructor
    mkByteMatchSetUpdate,

    -- * Lenses
    bmsuAction,
    bmsuByteMatchTuple,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.ByteMatchTuple
import Network.AWS.WAFRegional.Types.ChangeAction

-- | In an 'UpdateByteMatchSet' request, @ByteMatchSetUpdate@ specifies whether to insert or delete a 'ByteMatchTuple' and includes the settings for the @ByteMatchTuple@ .
--
-- /See:/ 'mkByteMatchSetUpdate' smart constructor.
data ByteMatchSetUpdate = ByteMatchSetUpdate'
  { action ::
      ChangeAction,
    byteMatchTuple :: ByteMatchTuple
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ByteMatchSetUpdate' with the minimum fields required to make a request.
--
-- * 'action' - Specifies whether to insert or delete a 'ByteMatchTuple' .
-- * 'byteMatchTuple' - Information about the part of a web request that you want AWS WAF to inspect and the value that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @ByteMatchTuple@ values must exactly match the values in the @ByteMatchTuple@ that you want to delete from the @ByteMatchSet@ .
mkByteMatchSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'byteMatchTuple'
  ByteMatchTuple ->
  ByteMatchSetUpdate
mkByteMatchSetUpdate pAction_ pByteMatchTuple_ =
  ByteMatchSetUpdate'
    { action = pAction_,
      byteMatchTuple = pByteMatchTuple_
    }

-- | Specifies whether to insert or delete a 'ByteMatchTuple' .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsuAction :: Lens.Lens' ByteMatchSetUpdate ChangeAction
bmsuAction = Lens.lens (action :: ByteMatchSetUpdate -> ChangeAction) (\s a -> s {action = a} :: ByteMatchSetUpdate)
{-# DEPRECATED bmsuAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Information about the part of a web request that you want AWS WAF to inspect and the value that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @ByteMatchTuple@ values must exactly match the values in the @ByteMatchTuple@ that you want to delete from the @ByteMatchSet@ .
--
-- /Note:/ Consider using 'byteMatchTuple' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsuByteMatchTuple :: Lens.Lens' ByteMatchSetUpdate ByteMatchTuple
bmsuByteMatchTuple = Lens.lens (byteMatchTuple :: ByteMatchSetUpdate -> ByteMatchTuple) (\s a -> s {byteMatchTuple = a} :: ByteMatchSetUpdate)
{-# DEPRECATED bmsuByteMatchTuple "Use generic-lens or generic-optics with 'byteMatchTuple' instead." #-}

instance Lude.ToJSON ByteMatchSetUpdate where
  toJSON ByteMatchSetUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("ByteMatchTuple" Lude..= byteMatchTuple)
          ]
      )
