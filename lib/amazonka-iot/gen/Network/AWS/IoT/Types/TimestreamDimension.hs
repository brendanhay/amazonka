{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimestreamDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TimestreamDimension
  ( TimestreamDimension (..)
  -- * Smart constructor
  , mkTimestreamDimension
  -- * Lenses
  , tdName
  , tdValue
  ) where

import qualified Network.AWS.IoT.Types.TimestreamDimensionName as Types
import qualified Network.AWS.IoT.Types.TimestreamDimensionValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Metadata attributes of the time series that are written in each measure record.
--
-- /See:/ 'mkTimestreamDimension' smart constructor.
data TimestreamDimension = TimestreamDimension'
  { name :: Types.TimestreamDimensionName
    -- ^ The metadata dimension name. This is the name of the column in the Amazon Timestream database table record.
--
-- Dimensions cannot be named: @measure_name@ , @measure_value@ , or @time@ . These names are reserved. Dimension names cannot start with @ts_@ or @measure_value@ and they cannot contain the colon (@:@ ) character.
  , value :: Types.TimestreamDimensionValue
    -- ^ The value to write in this column of the database record.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimestreamDimension' value with any optional fields omitted.
mkTimestreamDimension
    :: Types.TimestreamDimensionName -- ^ 'name'
    -> Types.TimestreamDimensionValue -- ^ 'value'
    -> TimestreamDimension
mkTimestreamDimension name value
  = TimestreamDimension'{name, value}

-- | The metadata dimension name. This is the name of the column in the Amazon Timestream database table record.
--
-- Dimensions cannot be named: @measure_name@ , @measure_value@ , or @time@ . These names are reserved. Dimension names cannot start with @ts_@ or @measure_value@ and they cannot contain the colon (@:@ ) character.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdName :: Lens.Lens' TimestreamDimension Types.TimestreamDimensionName
tdName = Lens.field @"name"
{-# INLINEABLE tdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value to write in this column of the database record.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdValue :: Lens.Lens' TimestreamDimension Types.TimestreamDimensionValue
tdValue = Lens.field @"value"
{-# INLINEABLE tdValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON TimestreamDimension where
        toJSON TimestreamDimension{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("value" Core..= value)])

instance Core.FromJSON TimestreamDimension where
        parseJSON
          = Core.withObject "TimestreamDimension" Core.$
              \ x ->
                TimestreamDimension' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "value"
