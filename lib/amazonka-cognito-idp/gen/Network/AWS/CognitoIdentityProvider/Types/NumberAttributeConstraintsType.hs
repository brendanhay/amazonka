{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
  ( NumberAttributeConstraintsType (..),

    -- * Smart constructor
    mkNumberAttributeConstraintsType,

    -- * Lenses
    nactMaxValue,
    nactMinValue,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.StringType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The minimum and maximum value of an attribute that is of the number data type.
--
-- /See:/ 'mkNumberAttributeConstraintsType' smart constructor.
data NumberAttributeConstraintsType = NumberAttributeConstraintsType'
  { -- | The maximum value of an attribute that is of the number data type.
    maxValue :: Core.Maybe Types.StringType,
    -- | The minimum value of an attribute that is of the number data type.
    minValue :: Core.Maybe Types.StringType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NumberAttributeConstraintsType' value with any optional fields omitted.
mkNumberAttributeConstraintsType ::
  NumberAttributeConstraintsType
mkNumberAttributeConstraintsType =
  NumberAttributeConstraintsType'
    { maxValue = Core.Nothing,
      minValue = Core.Nothing
    }

-- | The maximum value of an attribute that is of the number data type.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nactMaxValue :: Lens.Lens' NumberAttributeConstraintsType (Core.Maybe Types.StringType)
nactMaxValue = Lens.field @"maxValue"
{-# DEPRECATED nactMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

-- | The minimum value of an attribute that is of the number data type.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nactMinValue :: Lens.Lens' NumberAttributeConstraintsType (Core.Maybe Types.StringType)
nactMinValue = Lens.field @"minValue"
{-# DEPRECATED nactMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

instance Core.FromJSON NumberAttributeConstraintsType where
  toJSON NumberAttributeConstraintsType {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxValue" Core..=) Core.<$> maxValue,
            ("MinValue" Core..=) Core.<$> minValue
          ]
      )

instance Core.FromJSON NumberAttributeConstraintsType where
  parseJSON =
    Core.withObject "NumberAttributeConstraintsType" Core.$
      \x ->
        NumberAttributeConstraintsType'
          Core.<$> (x Core..:? "MaxValue") Core.<*> (x Core..:? "MinValue")
