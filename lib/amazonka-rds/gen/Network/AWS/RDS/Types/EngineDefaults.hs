{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EngineDefaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EngineDefaults
  ( EngineDefaults (..),

    -- * Smart constructor
    mkEngineDefaults,

    -- * Lenses
    edDBParameterGroupFamily,
    edMarker,
    edParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.Parameter as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | Contains the result of a successful invocation of the @DescribeEngineDefaultParameters@ action.
--
-- /See:/ 'mkEngineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { -- | Specifies the name of the DB parameter group family that the engine default parameters apply to.
    dBParameterGroupFamily :: Core.Maybe Types.String,
    -- | An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | Contains a list of engine default parameters.
    parameters :: Core.Maybe [Types.Parameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EngineDefaults' value with any optional fields omitted.
mkEngineDefaults ::
  EngineDefaults
mkEngineDefaults =
  EngineDefaults'
    { dBParameterGroupFamily = Core.Nothing,
      marker = Core.Nothing,
      parameters = Core.Nothing
    }

-- | Specifies the name of the DB parameter group family that the engine default parameters apply to.
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edDBParameterGroupFamily :: Lens.Lens' EngineDefaults (Core.Maybe Types.String)
edDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# DEPRECATED edDBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead." #-}

-- | An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMarker :: Lens.Lens' EngineDefaults (Core.Maybe Types.String)
edMarker = Lens.field @"marker"
{-# DEPRECATED edMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Contains a list of engine default parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edParameters :: Lens.Lens' EngineDefaults (Core.Maybe [Types.Parameter])
edParameters = Lens.field @"parameters"
{-# DEPRECATED edParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromXML EngineDefaults where
  parseXML x =
    EngineDefaults'
      Core.<$> (x Core..@? "DBParameterGroupFamily")
      Core.<*> (x Core..@? "Marker")
      Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter")
