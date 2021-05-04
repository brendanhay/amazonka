{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Parameter

-- | Describes the default cluster parameters for a parameter group family.
--
-- /See:/ 'newDefaultClusterParameters' smart constructor.
data DefaultClusterParameters = DefaultClusterParameters'
  { -- | The name of the cluster parameter group family to which the engine
    -- default parameters apply.
    parameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The list of cluster default parameters.
    parameters :: Prelude.Maybe [Parameter],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The name of the cluster parameter group family to which the engine
-- default parameters apply.
defaultClusterParameters_parameterGroupFamily :: Lens.Lens' DefaultClusterParameters (Prelude.Maybe Prelude.Text)
defaultClusterParameters_parameterGroupFamily = Lens.lens (\DefaultClusterParameters' {parameterGroupFamily} -> parameterGroupFamily) (\s@DefaultClusterParameters' {} a -> s {parameterGroupFamily = a} :: DefaultClusterParameters)

-- | The list of cluster default parameters.
defaultClusterParameters_parameters :: Lens.Lens' DefaultClusterParameters (Prelude.Maybe [Parameter])
defaultClusterParameters_parameters = Lens.lens (\DefaultClusterParameters' {parameters} -> parameters) (\s@DefaultClusterParameters' {} a -> s {parameters = a} :: DefaultClusterParameters) Prelude.. Lens.mapping Prelude._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
defaultClusterParameters_marker :: Lens.Lens' DefaultClusterParameters (Prelude.Maybe Prelude.Text)
defaultClusterParameters_marker = Lens.lens (\DefaultClusterParameters' {marker} -> marker) (\s@DefaultClusterParameters' {} a -> s {marker = a} :: DefaultClusterParameters)

instance Prelude.FromXML DefaultClusterParameters where
  parseXML x =
    DefaultClusterParameters'
      Prelude.<$> (x Prelude..@? "ParameterGroupFamily")
      Prelude.<*> ( x Prelude..@? "Parameters"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Parameter")
                  )
      Prelude.<*> (x Prelude..@? "Marker")

instance Prelude.Hashable DefaultClusterParameters

instance Prelude.NFData DefaultClusterParameters
