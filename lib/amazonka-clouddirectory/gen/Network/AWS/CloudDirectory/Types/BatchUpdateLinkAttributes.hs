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
    bulaTypedLinkSpecifier,
    bulaAttributeUpdates,
  )
where

import qualified Network.AWS.CloudDirectory.Types.LinkAttributeUpdate as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkSpecifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Updates a given typed link’s attributes inside a 'BatchRead' operation. Attributes to be updated must not contribute to the typed link’s identity, as defined by its @IdentityAttributeOrder@ . For more information, see 'UpdateLinkAttributes' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchUpdateLinkAttributes' smart constructor.
data BatchUpdateLinkAttributes = BatchUpdateLinkAttributes'
  { -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: Types.TypedLinkSpecifier,
    -- | The attributes update structure.
    attributeUpdates :: [Types.LinkAttributeUpdate]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchUpdateLinkAttributes' value with any optional fields omitted.
mkBatchUpdateLinkAttributes ::
  -- | 'typedLinkSpecifier'
  Types.TypedLinkSpecifier ->
  BatchUpdateLinkAttributes
mkBatchUpdateLinkAttributes typedLinkSpecifier =
  BatchUpdateLinkAttributes'
    { typedLinkSpecifier,
      attributeUpdates = Core.mempty
    }

-- | Allows a typed link specifier to be accepted as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bulaTypedLinkSpecifier :: Lens.Lens' BatchUpdateLinkAttributes Types.TypedLinkSpecifier
bulaTypedLinkSpecifier = Lens.field @"typedLinkSpecifier"
{-# DEPRECATED bulaTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

-- | The attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bulaAttributeUpdates :: Lens.Lens' BatchUpdateLinkAttributes [Types.LinkAttributeUpdate]
bulaAttributeUpdates = Lens.field @"attributeUpdates"
{-# DEPRECATED bulaAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

instance Core.FromJSON BatchUpdateLinkAttributes where
  toJSON BatchUpdateLinkAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TypedLinkSpecifier" Core..= typedLinkSpecifier),
            Core.Just ("AttributeUpdates" Core..= attributeUpdates)
          ]
      )
