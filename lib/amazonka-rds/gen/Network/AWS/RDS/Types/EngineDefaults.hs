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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.Parameter

-- | Contains the result of a successful invocation of the @DescribeEngineDefaultParameters@ action.
--
-- /See:/ 'mkEngineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { -- | Specifies the name of the DB parameter group family that the engine default parameters apply to.
    dbParameterGroupFamily :: Lude.Maybe Lude.Text,
    -- | An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | Contains a list of engine default parameters.
    parameters :: Lude.Maybe [Parameter]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EngineDefaults' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupFamily' - Specifies the name of the DB parameter group family that the engine default parameters apply to.
-- * 'marker' - An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'parameters' - Contains a list of engine default parameters.
mkEngineDefaults ::
  EngineDefaults
mkEngineDefaults =
  EngineDefaults'
    { dbParameterGroupFamily = Lude.Nothing,
      marker = Lude.Nothing,
      parameters = Lude.Nothing
    }

-- | Specifies the name of the DB parameter group family that the engine default parameters apply to.
--
-- /Note:/ Consider using 'dbParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edDBParameterGroupFamily :: Lens.Lens' EngineDefaults (Lude.Maybe Lude.Text)
edDBParameterGroupFamily = Lens.lens (dbParameterGroupFamily :: EngineDefaults -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupFamily = a} :: EngineDefaults)
{-# DEPRECATED edDBParameterGroupFamily "Use generic-lens or generic-optics with 'dbParameterGroupFamily' instead." #-}

-- | An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMarker :: Lens.Lens' EngineDefaults (Lude.Maybe Lude.Text)
edMarker = Lens.lens (marker :: EngineDefaults -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: EngineDefaults)
{-# DEPRECATED edMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Contains a list of engine default parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edParameters :: Lens.Lens' EngineDefaults (Lude.Maybe [Parameter])
edParameters = Lens.lens (parameters :: EngineDefaults -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: EngineDefaults)
{-# DEPRECATED edParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Lude.FromXML EngineDefaults where
  parseXML x =
    EngineDefaults'
      Lude.<$> (x Lude..@? "DBParameterGroupFamily")
      Lude.<*> (x Lude..@? "Marker")
      Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Parameter")
               )
