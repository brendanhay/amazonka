-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetLinkAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetLinkAttributes
  ( BatchGetLinkAttributes (..),

    -- * Smart constructor
    mkBatchGetLinkAttributes,

    -- * Lenses
    bglaTypedLinkSpecifier,
    bglaAttributeNames,
  )
where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Retrieves attributes that are associated with a typed link inside a 'BatchRead' operation. For more information, see 'GetLinkAttributes' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchGetLinkAttributes' smart constructor.
data BatchGetLinkAttributes = BatchGetLinkAttributes'
  { typedLinkSpecifier ::
      TypedLinkSpecifier,
    attributeNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetLinkAttributes' with the minimum fields required to make a request.
--
-- * 'attributeNames' - A list of attribute names whose values will be retrieved.
-- * 'typedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
mkBatchGetLinkAttributes ::
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  BatchGetLinkAttributes
mkBatchGetLinkAttributes pTypedLinkSpecifier_ =
  BatchGetLinkAttributes'
    { typedLinkSpecifier =
        pTypedLinkSpecifier_,
      attributeNames = Lude.mempty
    }

-- | Allows a typed link specifier to be accepted as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bglaTypedLinkSpecifier :: Lens.Lens' BatchGetLinkAttributes TypedLinkSpecifier
bglaTypedLinkSpecifier = Lens.lens (typedLinkSpecifier :: BatchGetLinkAttributes -> TypedLinkSpecifier) (\s a -> s {typedLinkSpecifier = a} :: BatchGetLinkAttributes)
{-# DEPRECATED bglaTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

-- | A list of attribute names whose values will be retrieved.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bglaAttributeNames :: Lens.Lens' BatchGetLinkAttributes [Lude.Text]
bglaAttributeNames = Lens.lens (attributeNames :: BatchGetLinkAttributes -> [Lude.Text]) (\s a -> s {attributeNames = a} :: BatchGetLinkAttributes)
{-# DEPRECATED bglaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

instance Lude.ToJSON BatchGetLinkAttributes where
  toJSON BatchGetLinkAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TypedLinkSpecifier" Lude..= typedLinkSpecifier),
            Lude.Just ("AttributeNames" Lude..= attributeNames)
          ]
      )
