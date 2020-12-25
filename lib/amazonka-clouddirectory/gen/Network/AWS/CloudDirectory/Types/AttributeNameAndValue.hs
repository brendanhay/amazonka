{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.AttributeNameAndValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.AttributeNameAndValue
  ( AttributeNameAndValue (..),

    -- * Smart constructor
    mkAttributeNameAndValue,

    -- * Lenses
    anavAttributeName,
    anavValue,
  )
where

import qualified Network.AWS.CloudDirectory.Types.AttributeName as Types
import qualified Network.AWS.CloudDirectory.Types.TypedAttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies the attribute name and value for a typed link.
--
-- /See:/ 'mkAttributeNameAndValue' smart constructor.
data AttributeNameAndValue = AttributeNameAndValue'
  { -- | The attribute name of the typed link.
    attributeName :: Types.AttributeName,
    -- | The value for the typed link.
    value :: Types.TypedAttributeValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AttributeNameAndValue' value with any optional fields omitted.
mkAttributeNameAndValue ::
  -- | 'attributeName'
  Types.AttributeName ->
  -- | 'value'
  Types.TypedAttributeValue ->
  AttributeNameAndValue
mkAttributeNameAndValue attributeName value =
  AttributeNameAndValue' {attributeName, value}

-- | The attribute name of the typed link.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anavAttributeName :: Lens.Lens' AttributeNameAndValue Types.AttributeName
anavAttributeName = Lens.field @"attributeName"
{-# DEPRECATED anavAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The value for the typed link.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anavValue :: Lens.Lens' AttributeNameAndValue Types.TypedAttributeValue
anavValue = Lens.field @"value"
{-# DEPRECATED anavValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON AttributeNameAndValue where
  toJSON AttributeNameAndValue {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AttributeName" Core..= attributeName),
            Core.Just ("Value" Core..= value)
          ]
      )

instance Core.FromJSON AttributeNameAndValue where
  parseJSON =
    Core.withObject "AttributeNameAndValue" Core.$
      \x ->
        AttributeNameAndValue'
          Core.<$> (x Core..: "AttributeName") Core.<*> (x Core..: "Value")
