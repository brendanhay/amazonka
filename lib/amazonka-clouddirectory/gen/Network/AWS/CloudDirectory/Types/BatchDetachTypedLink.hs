-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachTypedLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachTypedLink
  ( BatchDetachTypedLink (..),

    -- * Smart constructor
    mkBatchDetachTypedLink,

    -- * Lenses
    bdtlTypedLinkSpecifier,
  )
where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detaches a typed link from a specified source and target object inside a 'BatchRead' operation. For more information, see 'DetachTypedLink' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchDetachTypedLink' smart constructor.
newtype BatchDetachTypedLink = BatchDetachTypedLink'
  { typedLinkSpecifier ::
      TypedLinkSpecifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetachTypedLink' with the minimum fields required to make a request.
--
-- * 'typedLinkSpecifier' - Used to accept a typed link specifier as input.
mkBatchDetachTypedLink ::
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  BatchDetachTypedLink
mkBatchDetachTypedLink pTypedLinkSpecifier_ =
  BatchDetachTypedLink' {typedLinkSpecifier = pTypedLinkSpecifier_}

-- | Used to accept a typed link specifier as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtlTypedLinkSpecifier :: Lens.Lens' BatchDetachTypedLink TypedLinkSpecifier
bdtlTypedLinkSpecifier = Lens.lens (typedLinkSpecifier :: BatchDetachTypedLink -> TypedLinkSpecifier) (\s a -> s {typedLinkSpecifier = a} :: BatchDetachTypedLink)
{-# DEPRECATED bdtlTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

instance Lude.ToJSON BatchDetachTypedLink where
  toJSON BatchDetachTypedLink' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("TypedLinkSpecifier" Lude..= typedLinkSpecifier)]
      )
