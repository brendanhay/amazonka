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
-- Module      : Amazonka.Redshift.Types.DefaultClusterParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.DefaultClusterParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Parameter

-- | Describes the default cluster parameters for a parameter group family.
--
-- /See:/ 'newDefaultClusterParameters' smart constructor.
data DefaultClusterParameters = DefaultClusterParameters'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster parameter group family to which the engine
    -- default parameters apply.
    parameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The list of cluster default parameters.
    parameters :: Prelude.Maybe [Parameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultClusterParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'defaultClusterParameters_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'parameterGroupFamily', 'defaultClusterParameters_parameterGroupFamily' - The name of the cluster parameter group family to which the engine
-- default parameters apply.
--
-- 'parameters', 'defaultClusterParameters_parameters' - The list of cluster default parameters.
newDefaultClusterParameters ::
  DefaultClusterParameters
newDefaultClusterParameters =
  DefaultClusterParameters'
    { marker = Prelude.Nothing,
      parameterGroupFamily = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
defaultClusterParameters_marker :: Lens.Lens' DefaultClusterParameters (Prelude.Maybe Prelude.Text)
defaultClusterParameters_marker = Lens.lens (\DefaultClusterParameters' {marker} -> marker) (\s@DefaultClusterParameters' {} a -> s {marker = a} :: DefaultClusterParameters)

-- | The name of the cluster parameter group family to which the engine
-- default parameters apply.
defaultClusterParameters_parameterGroupFamily :: Lens.Lens' DefaultClusterParameters (Prelude.Maybe Prelude.Text)
defaultClusterParameters_parameterGroupFamily = Lens.lens (\DefaultClusterParameters' {parameterGroupFamily} -> parameterGroupFamily) (\s@DefaultClusterParameters' {} a -> s {parameterGroupFamily = a} :: DefaultClusterParameters)

-- | The list of cluster default parameters.
defaultClusterParameters_parameters :: Lens.Lens' DefaultClusterParameters (Prelude.Maybe [Parameter])
defaultClusterParameters_parameters = Lens.lens (\DefaultClusterParameters' {parameters} -> parameters) (\s@DefaultClusterParameters' {} a -> s {parameters = a} :: DefaultClusterParameters) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML DefaultClusterParameters where
  parseXML x =
    DefaultClusterParameters'
      Prelude.<$> (x Data..@? "Marker")
      Prelude.<*> (x Data..@? "ParameterGroupFamily")
      Prelude.<*> ( x
                      Data..@? "Parameters"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Parameter")
                  )

instance Prelude.Hashable DefaultClusterParameters where
  hashWithSalt _salt DefaultClusterParameters' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` parameterGroupFamily
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData DefaultClusterParameters where
  rnf DefaultClusterParameters' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf parameterGroupFamily
      `Prelude.seq` Prelude.rnf parameters
