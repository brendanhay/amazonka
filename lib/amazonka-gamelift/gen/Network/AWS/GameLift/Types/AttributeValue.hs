{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.AttributeValue
  ( AttributeValue (..),

    -- * Smart constructor
    mkAttributeValue,

    -- * Lenses
    avN,
    avS,
    avSDM,
    avSL,
  )
where

import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Values for use in 'Player' attribute key-value pairs. This object lets you specify an attribute value using any of the valid data types: string, number, string array, or data map. Each @AttributeValue@ object can use only one of the available properties.
--
-- /See:/ 'mkAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | For number values, expressed as double.
    n :: Core.Maybe Core.Double,
    -- | For single string values. Maximum string length is 100 characters.
    s :: Core.Maybe Types.NonZeroAndMaxString,
    -- | For a map of up to 10 data type:value pairs. Maximum length for each string value is 100 characters.
    sdm :: Core.Maybe (Core.HashMap Types.NonZeroAndMaxString Core.Double),
    -- | For a list of up to 10 strings. Maximum length for each string is 100 characters. Duplicate values are not recognized; all occurrences of the repeated value after the first of a repeated value are ignored.
    sl :: Core.Maybe [Types.NonZeroAndMaxString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeValue' value with any optional fields omitted.
mkAttributeValue ::
  AttributeValue
mkAttributeValue =
  AttributeValue'
    { n = Core.Nothing,
      s = Core.Nothing,
      sdm = Core.Nothing,
      sl = Core.Nothing
    }

-- | For number values, expressed as double.
--
-- /Note:/ Consider using 'n' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avN :: Lens.Lens' AttributeValue (Core.Maybe Core.Double)
avN = Lens.field @"n"
{-# DEPRECATED avN "Use generic-lens or generic-optics with 'n' instead." #-}

-- | For single string values. Maximum string length is 100 characters.
--
-- /Note:/ Consider using 's' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avS :: Lens.Lens' AttributeValue (Core.Maybe Types.NonZeroAndMaxString)
avS = Lens.field @"s"
{-# DEPRECATED avS "Use generic-lens or generic-optics with 's' instead." #-}

-- | For a map of up to 10 data type:value pairs. Maximum length for each string value is 100 characters.
--
-- /Note:/ Consider using 'sdm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avSDM :: Lens.Lens' AttributeValue (Core.Maybe (Core.HashMap Types.NonZeroAndMaxString Core.Double))
avSDM = Lens.field @"sdm"
{-# DEPRECATED avSDM "Use generic-lens or generic-optics with 'sdm' instead." #-}

-- | For a list of up to 10 strings. Maximum length for each string is 100 characters. Duplicate values are not recognized; all occurrences of the repeated value after the first of a repeated value are ignored.
--
-- /Note:/ Consider using 'sl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avSL :: Lens.Lens' AttributeValue (Core.Maybe [Types.NonZeroAndMaxString])
avSL = Lens.field @"sl"
{-# DEPRECATED avSL "Use generic-lens or generic-optics with 'sl' instead." #-}

instance Core.FromJSON AttributeValue where
  toJSON AttributeValue {..} =
    Core.object
      ( Core.catMaybes
          [ ("N" Core..=) Core.<$> n,
            ("S" Core..=) Core.<$> s,
            ("SDM" Core..=) Core.<$> sdm,
            ("SL" Core..=) Core.<$> sl
          ]
      )

instance Core.FromJSON AttributeValue where
  parseJSON =
    Core.withObject "AttributeValue" Core.$
      \x ->
        AttributeValue'
          Core.<$> (x Core..:? "N")
          Core.<*> (x Core..:? "S")
          Core.<*> (x Core..:? "SDM")
          Core.<*> (x Core..:? "SL")
