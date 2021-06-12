{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DefaultClusterParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DefaultClusterParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Parameter

-- | Describes the default cluster parameters for a parameter group family.
--
-- /See:/ 'newDefaultClusterParameters' smart constructor.
data DefaultClusterParameters = DefaultClusterParameters'
  { -- | The name of the cluster parameter group family to which the engine
    -- default parameters apply.
    parameterGroupFamily :: Core.Maybe Core.Text,
    -- | The list of cluster default parameters.
    parameters :: Core.Maybe [Parameter],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DefaultClusterParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupFamily', 'defaultClusterParameters_parameterGroupFamily' - The name of the cluster parameter group family to which the engine
-- default parameters apply.
--
-- 'parameters', 'defaultClusterParameters_parameters' - The list of cluster default parameters.
--
-- 'marker', 'defaultClusterParameters_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
newDefaultClusterParameters ::
  DefaultClusterParameters
newDefaultClusterParameters =
  DefaultClusterParameters'
    { parameterGroupFamily =
        Core.Nothing,
      parameters = Core.Nothing,
      marker = Core.Nothing
    }

-- | The name of the cluster parameter group family to which the engine
-- default parameters apply.
defaultClusterParameters_parameterGroupFamily :: Lens.Lens' DefaultClusterParameters (Core.Maybe Core.Text)
defaultClusterParameters_parameterGroupFamily = Lens.lens (\DefaultClusterParameters' {parameterGroupFamily} -> parameterGroupFamily) (\s@DefaultClusterParameters' {} a -> s {parameterGroupFamily = a} :: DefaultClusterParameters)

-- | The list of cluster default parameters.
defaultClusterParameters_parameters :: Lens.Lens' DefaultClusterParameters (Core.Maybe [Parameter])
defaultClusterParameters_parameters = Lens.lens (\DefaultClusterParameters' {parameters} -> parameters) (\s@DefaultClusterParameters' {} a -> s {parameters = a} :: DefaultClusterParameters) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
defaultClusterParameters_marker :: Lens.Lens' DefaultClusterParameters (Core.Maybe Core.Text)
defaultClusterParameters_marker = Lens.lens (\DefaultClusterParameters' {marker} -> marker) (\s@DefaultClusterParameters' {} a -> s {marker = a} :: DefaultClusterParameters)

instance Core.FromXML DefaultClusterParameters where
  parseXML x =
    DefaultClusterParameters'
      Core.<$> (x Core..@? "ParameterGroupFamily")
      Core.<*> ( x Core..@? "Parameters" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Parameter")
               )
      Core.<*> (x Core..@? "Marker")

instance Core.Hashable DefaultClusterParameters

instance Core.NFData DefaultClusterParameters
