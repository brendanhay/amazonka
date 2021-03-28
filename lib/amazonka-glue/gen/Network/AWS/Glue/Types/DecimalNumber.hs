{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DecimalNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.DecimalNumber
  ( DecimalNumber (..)
  -- * Smart constructor
  , mkDecimalNumber
  -- * Lenses
  , dnUnscaledValue
  , dnScale
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a numeric value in decimal format.
--
-- /See:/ 'mkDecimalNumber' smart constructor.
data DecimalNumber = DecimalNumber'
  { unscaledValue :: Core.Base64
    -- ^ The unscaled numeric value.
  , scale :: Core.Int
    -- ^ The scale that determines where the decimal point falls in the unscaled value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecimalNumber' value with any optional fields omitted.
mkDecimalNumber
    :: Core.Base64 -- ^ 'unscaledValue'
    -> Core.Int -- ^ 'scale'
    -> DecimalNumber
mkDecimalNumber unscaledValue scale
  = DecimalNumber'{unscaledValue, scale}

-- | The unscaled numeric value.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'unscaledValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnUnscaledValue :: Lens.Lens' DecimalNumber Core.Base64
dnUnscaledValue = Lens.field @"unscaledValue"
{-# INLINEABLE dnUnscaledValue #-}
{-# DEPRECATED unscaledValue "Use generic-lens or generic-optics with 'unscaledValue' instead"  #-}

-- | The scale that determines where the decimal point falls in the unscaled value.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnScale :: Lens.Lens' DecimalNumber Core.Int
dnScale = Lens.field @"scale"
{-# INLINEABLE dnScale #-}
{-# DEPRECATED scale "Use generic-lens or generic-optics with 'scale' instead"  #-}

instance Core.FromJSON DecimalNumber where
        toJSON DecimalNumber{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UnscaledValue" Core..= unscaledValue),
                  Core.Just ("Scale" Core..= scale)])

instance Core.FromJSON DecimalNumber where
        parseJSON
          = Core.withObject "DecimalNumber" Core.$
              \ x ->
                DecimalNumber' Core.<$>
                  (x Core..: "UnscaledValue") Core.<*> x Core..: "Scale"
