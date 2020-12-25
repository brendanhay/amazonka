{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DefaultClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DefaultClusterParameters
  ( DefaultClusterParameters (..),

    -- * Smart constructor
    mkDefaultClusterParameters,

    -- * Lenses
    dcpMarker,
    dcpParameterGroupFamily,
    dcpParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Parameter as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes the default cluster parameters for a parameter group family.
--
-- /See:/ 'mkDefaultClusterParameters' smart constructor.
data DefaultClusterParameters = DefaultClusterParameters'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | The name of the cluster parameter group family to which the engine default parameters apply.
    parameterGroupFamily :: Core.Maybe Types.String,
    -- | The list of cluster default parameters.
    parameters :: Core.Maybe [Types.Parameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefaultClusterParameters' value with any optional fields omitted.
mkDefaultClusterParameters ::
  DefaultClusterParameters
mkDefaultClusterParameters =
  DefaultClusterParameters'
    { marker = Core.Nothing,
      parameterGroupFamily = Core.Nothing,
      parameters = Core.Nothing
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMarker :: Lens.Lens' DefaultClusterParameters (Core.Maybe Types.String)
dcpMarker = Lens.field @"marker"
{-# DEPRECATED dcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The name of the cluster parameter group family to which the engine default parameters apply.
--
-- /Note:/ Consider using 'parameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpParameterGroupFamily :: Lens.Lens' DefaultClusterParameters (Core.Maybe Types.String)
dcpParameterGroupFamily = Lens.field @"parameterGroupFamily"
{-# DEPRECATED dcpParameterGroupFamily "Use generic-lens or generic-optics with 'parameterGroupFamily' instead." #-}

-- | The list of cluster default parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpParameters :: Lens.Lens' DefaultClusterParameters (Core.Maybe [Types.Parameter])
dcpParameters = Lens.field @"parameters"
{-# DEPRECATED dcpParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromXML DefaultClusterParameters where
  parseXML x =
    DefaultClusterParameters'
      Core.<$> (x Core..@? "Marker")
      Core.<*> (x Core..@? "ParameterGroupFamily")
      Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter")
