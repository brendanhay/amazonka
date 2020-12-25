{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.CloudDirectory.Types.AttributeName as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkSpecifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Retrieves attributes that are associated with a typed link inside a 'BatchRead' operation. For more information, see 'GetLinkAttributes' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchGetLinkAttributes' smart constructor.
data BatchGetLinkAttributes = BatchGetLinkAttributes'
  { -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: Types.TypedLinkSpecifier,
    -- | A list of attribute names whose values will be retrieved.
    attributeNames :: [Types.AttributeName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetLinkAttributes' value with any optional fields omitted.
mkBatchGetLinkAttributes ::
  -- | 'typedLinkSpecifier'
  Types.TypedLinkSpecifier ->
  BatchGetLinkAttributes
mkBatchGetLinkAttributes typedLinkSpecifier =
  BatchGetLinkAttributes'
    { typedLinkSpecifier,
      attributeNames = Core.mempty
    }

-- | Allows a typed link specifier to be accepted as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bglaTypedLinkSpecifier :: Lens.Lens' BatchGetLinkAttributes Types.TypedLinkSpecifier
bglaTypedLinkSpecifier = Lens.field @"typedLinkSpecifier"
{-# DEPRECATED bglaTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

-- | A list of attribute names whose values will be retrieved.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bglaAttributeNames :: Lens.Lens' BatchGetLinkAttributes [Types.AttributeName]
bglaAttributeNames = Lens.field @"attributeNames"
{-# DEPRECATED bglaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

instance Core.FromJSON BatchGetLinkAttributes where
  toJSON BatchGetLinkAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TypedLinkSpecifier" Core..= typedLinkSpecifier),
            Core.Just ("AttributeNames" Core..= attributeNames)
          ]
      )
