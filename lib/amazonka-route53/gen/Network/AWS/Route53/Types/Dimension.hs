{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Dimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.Dimension
  ( Dimension (..)
  -- * Smart constructor
  , mkDimension
  -- * Lenses
  , dName
  , dValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.DimensionField as Types

-- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about one dimension.
--
-- /See:/ 'mkDimension' smart constructor.
data Dimension = Dimension'
  { name :: Types.DimensionField
    -- ^ For the metric that the CloudWatch alarm is associated with, the name of one dimension.
  , value :: Types.DimensionField
    -- ^ For the metric that the CloudWatch alarm is associated with, the value of one dimension.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Dimension' value with any optional fields omitted.
mkDimension
    :: Types.DimensionField -- ^ 'name'
    -> Types.DimensionField -- ^ 'value'
    -> Dimension
mkDimension name value = Dimension'{name, value}

-- | For the metric that the CloudWatch alarm is associated with, the name of one dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Dimension Types.DimensionField
dName = Lens.field @"name"
{-# INLINEABLE dName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | For the metric that the CloudWatch alarm is associated with, the value of one dimension.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dValue :: Lens.Lens' Dimension Types.DimensionField
dValue = Lens.field @"value"
{-# INLINEABLE dValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML Dimension where
        parseXML x
          = Dimension' Core.<$> (x Core..@ "Name") Core.<*> x Core..@ "Value"
