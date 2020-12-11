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
    dcpParameters,
    dcpParameterGroupFamily,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Parameter

-- | Describes the default cluster parameters for a parameter group family.
--
-- /See:/ 'mkDefaultClusterParameters' smart constructor.
data DefaultClusterParameters = DefaultClusterParameters'
  { marker ::
      Lude.Maybe Lude.Text,
    parameters :: Lude.Maybe [Parameter],
    parameterGroupFamily ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefaultClusterParameters' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'parameterGroupFamily' - The name of the cluster parameter group family to which the engine default parameters apply.
-- * 'parameters' - The list of cluster default parameters.
mkDefaultClusterParameters ::
  DefaultClusterParameters
mkDefaultClusterParameters =
  DefaultClusterParameters'
    { marker = Lude.Nothing,
      parameters = Lude.Nothing,
      parameterGroupFamily = Lude.Nothing
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMarker :: Lens.Lens' DefaultClusterParameters (Lude.Maybe Lude.Text)
dcpMarker = Lens.lens (marker :: DefaultClusterParameters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DefaultClusterParameters)
{-# DEPRECATED dcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of cluster default parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpParameters :: Lens.Lens' DefaultClusterParameters (Lude.Maybe [Parameter])
dcpParameters = Lens.lens (parameters :: DefaultClusterParameters -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: DefaultClusterParameters)
{-# DEPRECATED dcpParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The name of the cluster parameter group family to which the engine default parameters apply.
--
-- /Note:/ Consider using 'parameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpParameterGroupFamily :: Lens.Lens' DefaultClusterParameters (Lude.Maybe Lude.Text)
dcpParameterGroupFamily = Lens.lens (parameterGroupFamily :: DefaultClusterParameters -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupFamily = a} :: DefaultClusterParameters)
{-# DEPRECATED dcpParameterGroupFamily "Use generic-lens or generic-optics with 'parameterGroupFamily' instead." #-}

instance Lude.FromXML DefaultClusterParameters where
  parseXML x =
    DefaultClusterParameters'
      Lude.<$> (x Lude..@? "Marker")
      Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Parameter")
               )
      Lude.<*> (x Lude..@? "ParameterGroupFamily")
