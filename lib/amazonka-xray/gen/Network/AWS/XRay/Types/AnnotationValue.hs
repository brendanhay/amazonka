{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.AnnotationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.AnnotationValue
  ( AnnotationValue (..)
  -- * Smart constructor
  , mkAnnotationValue
  -- * Lenses
  , avBooleanValue
  , avNumberValue
  , avStringValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Value of a segment annotation. Has one of three value types: Number, Boolean, or String.
--
-- /See:/ 'mkAnnotationValue' smart constructor.
data AnnotationValue = AnnotationValue'
  { booleanValue :: Core.Maybe Core.Bool
    -- ^ Value for a Boolean annotation.
  , numberValue :: Core.Maybe Core.Double
    -- ^ Value for a Number annotation.
  , stringValue :: Core.Maybe Core.Text
    -- ^ Value for a String annotation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnnotationValue' value with any optional fields omitted.
mkAnnotationValue
    :: AnnotationValue
mkAnnotationValue
  = AnnotationValue'{booleanValue = Core.Nothing,
                     numberValue = Core.Nothing, stringValue = Core.Nothing}

-- | Value for a Boolean annotation.
--
-- /Note:/ Consider using 'booleanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avBooleanValue :: Lens.Lens' AnnotationValue (Core.Maybe Core.Bool)
avBooleanValue = Lens.field @"booleanValue"
{-# INLINEABLE avBooleanValue #-}
{-# DEPRECATED booleanValue "Use generic-lens or generic-optics with 'booleanValue' instead"  #-}

-- | Value for a Number annotation.
--
-- /Note:/ Consider using 'numberValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avNumberValue :: Lens.Lens' AnnotationValue (Core.Maybe Core.Double)
avNumberValue = Lens.field @"numberValue"
{-# INLINEABLE avNumberValue #-}
{-# DEPRECATED numberValue "Use generic-lens or generic-optics with 'numberValue' instead"  #-}

-- | Value for a String annotation.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avStringValue :: Lens.Lens' AnnotationValue (Core.Maybe Core.Text)
avStringValue = Lens.field @"stringValue"
{-# INLINEABLE avStringValue #-}
{-# DEPRECATED stringValue "Use generic-lens or generic-optics with 'stringValue' instead"  #-}

instance Core.FromJSON AnnotationValue where
        parseJSON
          = Core.withObject "AnnotationValue" Core.$
              \ x ->
                AnnotationValue' Core.<$>
                  (x Core..:? "BooleanValue") Core.<*> x Core..:? "NumberValue"
                    Core.<*> x Core..:? "StringValue"
