{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EngineDefaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.EngineDefaults
  ( EngineDefaults (..)
  -- * Smart constructor
  , mkEngineDefaults
  -- * Lenses
  , edDBParameterGroupFamily
  , edMarker
  , edParameters
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.Parameter as Types

-- | Contains the result of a successful invocation of the @DescribeEngineDefaultParameters@ action. 
--
-- /See:/ 'mkEngineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { dBParameterGroupFamily :: Core.Maybe Core.Text
    -- ^ Specifies the name of the DB parameter group family that the engine default parameters apply to.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ Contains a list of engine default parameters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EngineDefaults' value with any optional fields omitted.
mkEngineDefaults
    :: EngineDefaults
mkEngineDefaults
  = EngineDefaults'{dBParameterGroupFamily = Core.Nothing,
                    marker = Core.Nothing, parameters = Core.Nothing}

-- | Specifies the name of the DB parameter group family that the engine default parameters apply to.
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edDBParameterGroupFamily :: Lens.Lens' EngineDefaults (Core.Maybe Core.Text)
edDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# INLINEABLE edDBParameterGroupFamily #-}
{-# DEPRECATED dBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead"  #-}

-- | An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMarker :: Lens.Lens' EngineDefaults (Core.Maybe Core.Text)
edMarker = Lens.field @"marker"
{-# INLINEABLE edMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Contains a list of engine default parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edParameters :: Lens.Lens' EngineDefaults (Core.Maybe [Types.Parameter])
edParameters = Lens.field @"parameters"
{-# INLINEABLE edParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.FromXML EngineDefaults where
        parseXML x
          = EngineDefaults' Core.<$>
              (x Core..@? "DBParameterGroupFamily") Core.<*> x Core..@? "Marker"
                Core.<*>
                x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter"
