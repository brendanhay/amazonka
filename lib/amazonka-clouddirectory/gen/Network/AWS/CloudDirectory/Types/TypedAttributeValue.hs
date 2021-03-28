{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.TypedAttributeValue
  ( TypedAttributeValue (..)
  -- * Smart constructor
  , mkTypedAttributeValue
  -- * Lenses
  , tavBinaryValue
  , tavBooleanValue
  , tavDatetimeValue
  , tavNumberValue
  , tavStringValue
  ) where

import qualified Network.AWS.CloudDirectory.Types.NumberValue as Types
import qualified Network.AWS.CloudDirectory.Types.StringAttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the data for a typed attribute. You can set one, and only one, of the elements. Each attribute in an item is a name-value pair. Attributes have a single value.
--
-- /See:/ 'mkTypedAttributeValue' smart constructor.
data TypedAttributeValue = TypedAttributeValue'
  { binaryValue :: Core.Maybe Core.Base64
    -- ^ A binary data value.
  , booleanValue :: Core.Maybe Core.Bool
    -- ^ A Boolean data value.
  , datetimeValue :: Core.Maybe Core.NominalDiffTime
    -- ^ A date and time value.
  , numberValue :: Core.Maybe Types.NumberValue
    -- ^ A number data value.
  , stringValue :: Core.Maybe Types.StringAttributeValue
    -- ^ A string data value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TypedAttributeValue' value with any optional fields omitted.
mkTypedAttributeValue
    :: TypedAttributeValue
mkTypedAttributeValue
  = TypedAttributeValue'{binaryValue = Core.Nothing,
                         booleanValue = Core.Nothing, datetimeValue = Core.Nothing,
                         numberValue = Core.Nothing, stringValue = Core.Nothing}

-- | A binary data value.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'binaryValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavBinaryValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Core.Base64)
tavBinaryValue = Lens.field @"binaryValue"
{-# INLINEABLE tavBinaryValue #-}
{-# DEPRECATED binaryValue "Use generic-lens or generic-optics with 'binaryValue' instead"  #-}

-- | A Boolean data value.
--
-- /Note:/ Consider using 'booleanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavBooleanValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Core.Bool)
tavBooleanValue = Lens.field @"booleanValue"
{-# INLINEABLE tavBooleanValue #-}
{-# DEPRECATED booleanValue "Use generic-lens or generic-optics with 'booleanValue' instead"  #-}

-- | A date and time value.
--
-- /Note:/ Consider using 'datetimeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavDatetimeValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Core.NominalDiffTime)
tavDatetimeValue = Lens.field @"datetimeValue"
{-# INLINEABLE tavDatetimeValue #-}
{-# DEPRECATED datetimeValue "Use generic-lens or generic-optics with 'datetimeValue' instead"  #-}

-- | A number data value.
--
-- /Note:/ Consider using 'numberValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavNumberValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Types.NumberValue)
tavNumberValue = Lens.field @"numberValue"
{-# INLINEABLE tavNumberValue #-}
{-# DEPRECATED numberValue "Use generic-lens or generic-optics with 'numberValue' instead"  #-}

-- | A string data value.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavStringValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Types.StringAttributeValue)
tavStringValue = Lens.field @"stringValue"
{-# INLINEABLE tavStringValue #-}
{-# DEPRECATED stringValue "Use generic-lens or generic-optics with 'stringValue' instead"  #-}

instance Core.FromJSON TypedAttributeValue where
        toJSON TypedAttributeValue{..}
          = Core.object
              (Core.catMaybes
                 [("BinaryValue" Core..=) Core.<$> binaryValue,
                  ("BooleanValue" Core..=) Core.<$> booleanValue,
                  ("DatetimeValue" Core..=) Core.<$> datetimeValue,
                  ("NumberValue" Core..=) Core.<$> numberValue,
                  ("StringValue" Core..=) Core.<$> stringValue])

instance Core.FromJSON TypedAttributeValue where
        parseJSON
          = Core.withObject "TypedAttributeValue" Core.$
              \ x ->
                TypedAttributeValue' Core.<$>
                  (x Core..:? "BinaryValue") Core.<*> x Core..:? "BooleanValue"
                    Core.<*> x Core..:? "DatetimeValue"
                    Core.<*> x Core..:? "NumberValue"
                    Core.<*> x Core..:? "StringValue"
