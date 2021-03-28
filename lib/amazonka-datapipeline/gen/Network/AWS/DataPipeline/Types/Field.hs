{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Field
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.Field
  ( Field (..)
  -- * Smart constructor
  , mkField
  -- * Lenses
  , fKey
  , fRefValue
  , fStringValue
  ) where

import qualified Network.AWS.DataPipeline.Types.Key as Types
import qualified Network.AWS.DataPipeline.Types.RefValue as Types
import qualified Network.AWS.DataPipeline.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A key-value pair that describes a property of a pipeline object. The value is specified as either a string value (@StringValue@ ) or a reference to another object (@RefValue@ ) but not as both.
--
-- /See:/ 'mkField' smart constructor.
data Field = Field'
  { key :: Types.Key
    -- ^ The field identifier.
  , refValue :: Core.Maybe Types.RefValue
    -- ^ The field value, expressed as the identifier of another object.
  , stringValue :: Core.Maybe Types.StringValue
    -- ^ The field value, expressed as a String.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Field' value with any optional fields omitted.
mkField
    :: Types.Key -- ^ 'key'
    -> Field
mkField key
  = Field'{key, refValue = Core.Nothing, stringValue = Core.Nothing}

-- | The field identifier.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKey :: Lens.Lens' Field Types.Key
fKey = Lens.field @"key"
{-# INLINEABLE fKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The field value, expressed as the identifier of another object.
--
-- /Note:/ Consider using 'refValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRefValue :: Lens.Lens' Field (Core.Maybe Types.RefValue)
fRefValue = Lens.field @"refValue"
{-# INLINEABLE fRefValue #-}
{-# DEPRECATED refValue "Use generic-lens or generic-optics with 'refValue' instead"  #-}

-- | The field value, expressed as a String.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fStringValue :: Lens.Lens' Field (Core.Maybe Types.StringValue)
fStringValue = Lens.field @"stringValue"
{-# INLINEABLE fStringValue #-}
{-# DEPRECATED stringValue "Use generic-lens or generic-optics with 'stringValue' instead"  #-}

instance Core.FromJSON Field where
        toJSON Field{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("key" Core..= key),
                  ("refValue" Core..=) Core.<$> refValue,
                  ("stringValue" Core..=) Core.<$> stringValue])

instance Core.FromJSON Field where
        parseJSON
          = Core.withObject "Field" Core.$
              \ x ->
                Field' Core.<$>
                  (x Core..: "key") Core.<*> x Core..:? "refValue" Core.<*>
                    x Core..:? "stringValue"
