{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes
  ( BatchUpdateLinkAttributes (..),

    -- * Smart constructor
    mkBatchUpdateLinkAttributes,

    -- * Lenses
    bulaAttributeUpdates,
    bulaTypedLinkSpecifier,
  )
where

import Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Updates a given typed link’s attributes inside a 'BatchRead' operation. Attributes to be updated must not contribute to the typed link’s identity, as defined by its @IdentityAttributeOrder@ . For more information, see 'UpdateLinkAttributes' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchUpdateLinkAttributes' smart constructor.
data BatchUpdateLinkAttributes = BatchUpdateLinkAttributes'
  { -- | The attributes update structure.
    attributeUpdates :: [LinkAttributeUpdate],
    -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: TypedLinkSpecifier
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchUpdateLinkAttributes' with the minimum fields required to make a request.
--
-- * 'attributeUpdates' - The attributes update structure.
-- * 'typedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
mkBatchUpdateLinkAttributes ::
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  BatchUpdateLinkAttributes
mkBatchUpdateLinkAttributes pTypedLinkSpecifier_ =
  BatchUpdateLinkAttributes'
    { attributeUpdates = Lude.mempty,
      typedLinkSpecifier = pTypedLinkSpecifier_
    }

-- | The attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bulaAttributeUpdates :: Lens.Lens' BatchUpdateLinkAttributes [LinkAttributeUpdate]
bulaAttributeUpdates = Lens.lens (attributeUpdates :: BatchUpdateLinkAttributes -> [LinkAttributeUpdate]) (\s a -> s {attributeUpdates = a} :: BatchUpdateLinkAttributes)
{-# DEPRECATED bulaAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

-- | Allows a typed link specifier to be accepted as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bulaTypedLinkSpecifier :: Lens.Lens' BatchUpdateLinkAttributes TypedLinkSpecifier
bulaTypedLinkSpecifier = Lens.lens (typedLinkSpecifier :: BatchUpdateLinkAttributes -> TypedLinkSpecifier) (\s a -> s {typedLinkSpecifier = a} :: BatchUpdateLinkAttributes)
{-# DEPRECATED bulaTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

instance Lude.ToJSON BatchUpdateLinkAttributes where
  toJSON BatchUpdateLinkAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AttributeUpdates" Lude..= attributeUpdates),
            Lude.Just ("TypedLinkSpecifier" Lude..= typedLinkSpecifier)
          ]
      )
