{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
  ( TypedLinkAttributeRange (..),

    -- * Smart constructor
    mkTypedLinkAttributeRange,

    -- * Lenses
    tlarRange,
    tlarAttributeName,
  )
where

import qualified Network.AWS.CloudDirectory.Types.AttributeName as Types
import qualified Network.AWS.CloudDirectory.Types.TypedAttributeValueRange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies the range of attributes that are used by a specified filter.
--
-- /See:/ 'mkTypedLinkAttributeRange' smart constructor.
data TypedLinkAttributeRange = TypedLinkAttributeRange'
  { -- | The range of attribute values that are being selected.
    range :: Types.TypedAttributeValueRange,
    -- | The unique name of the typed link attribute.
    attributeName :: Core.Maybe Types.AttributeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TypedLinkAttributeRange' value with any optional fields omitted.
mkTypedLinkAttributeRange ::
  -- | 'range'
  Types.TypedAttributeValueRange ->
  TypedLinkAttributeRange
mkTypedLinkAttributeRange range =
  TypedLinkAttributeRange' {range, attributeName = Core.Nothing}

-- | The range of attribute values that are being selected.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlarRange :: Lens.Lens' TypedLinkAttributeRange Types.TypedAttributeValueRange
tlarRange = Lens.field @"range"
{-# DEPRECATED tlarRange "Use generic-lens or generic-optics with 'range' instead." #-}

-- | The unique name of the typed link attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlarAttributeName :: Lens.Lens' TypedLinkAttributeRange (Core.Maybe Types.AttributeName)
tlarAttributeName = Lens.field @"attributeName"
{-# DEPRECATED tlarAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Core.FromJSON TypedLinkAttributeRange where
  toJSON TypedLinkAttributeRange {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Range" Core..= range),
            ("AttributeName" Core..=) Core.<$> attributeName
          ]
      )
