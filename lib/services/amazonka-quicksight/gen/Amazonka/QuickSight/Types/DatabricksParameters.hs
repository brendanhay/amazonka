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
-- Module      : Amazonka.QuickSight.Types.DatabricksParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DatabricksParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The required parameters that are needed to connect to a Databricks data
-- source.
--
-- /See:/ 'newDatabricksParameters' smart constructor.
data DatabricksParameters = DatabricksParameters'
  { -- | The host name of the Databricks data source.
    host :: Prelude.Text,
    -- | The port for the Databricks data source.
    port :: Prelude.Natural,
    -- | The HTTP path of the Databricks data source.
    sqlEndpointPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabricksParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'databricksParameters_host' - The host name of the Databricks data source.
--
-- 'port', 'databricksParameters_port' - The port for the Databricks data source.
--
-- 'sqlEndpointPath', 'databricksParameters_sqlEndpointPath' - The HTTP path of the Databricks data source.
newDatabricksParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'sqlEndpointPath'
  Prelude.Text ->
  DatabricksParameters
newDatabricksParameters
  pHost_
  pPort_
  pSqlEndpointPath_ =
    DatabricksParameters'
      { host = pHost_,
        port = pPort_,
        sqlEndpointPath = pSqlEndpointPath_
      }

-- | The host name of the Databricks data source.
databricksParameters_host :: Lens.Lens' DatabricksParameters Prelude.Text
databricksParameters_host = Lens.lens (\DatabricksParameters' {host} -> host) (\s@DatabricksParameters' {} a -> s {host = a} :: DatabricksParameters)

-- | The port for the Databricks data source.
databricksParameters_port :: Lens.Lens' DatabricksParameters Prelude.Natural
databricksParameters_port = Lens.lens (\DatabricksParameters' {port} -> port) (\s@DatabricksParameters' {} a -> s {port = a} :: DatabricksParameters)

-- | The HTTP path of the Databricks data source.
databricksParameters_sqlEndpointPath :: Lens.Lens' DatabricksParameters Prelude.Text
databricksParameters_sqlEndpointPath = Lens.lens (\DatabricksParameters' {sqlEndpointPath} -> sqlEndpointPath) (\s@DatabricksParameters' {} a -> s {sqlEndpointPath = a} :: DatabricksParameters)

instance Data.FromJSON DatabricksParameters where
  parseJSON =
    Data.withObject
      "DatabricksParameters"
      ( \x ->
          DatabricksParameters'
            Prelude.<$> (x Data..: "Host")
            Prelude.<*> (x Data..: "Port")
            Prelude.<*> (x Data..: "SqlEndpointPath")
      )

instance Prelude.Hashable DatabricksParameters where
  hashWithSalt _salt DatabricksParameters' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` sqlEndpointPath

instance Prelude.NFData DatabricksParameters where
  rnf DatabricksParameters' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf sqlEndpointPath

instance Data.ToJSON DatabricksParameters where
  toJSON DatabricksParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Port" Data..= port),
            Prelude.Just
              ("SqlEndpointPath" Data..= sqlEndpointPath)
          ]
      )
