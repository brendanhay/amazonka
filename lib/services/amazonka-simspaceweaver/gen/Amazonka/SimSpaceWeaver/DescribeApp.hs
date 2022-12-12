{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SimSpaceWeaver.DescribeApp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the state of the given custom app.
module Amazonka.SimSpaceWeaver.DescribeApp
  ( -- * Creating a Request
    DescribeApp (..),
    newDescribeApp,

    -- * Request Lenses
    describeApp_app,
    describeApp_domain,
    describeApp_simulation,

    -- * Destructuring the Response
    DescribeAppResponse (..),
    newDescribeAppResponse,

    -- * Response Lenses
    describeAppResponse_description,
    describeAppResponse_domain,
    describeAppResponse_endpointInfo,
    describeAppResponse_launchOverrides,
    describeAppResponse_name,
    describeAppResponse_simulation,
    describeAppResponse_status,
    describeAppResponse_targetStatus,
    describeAppResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newDescribeApp' smart constructor.
data DescribeApp = DescribeApp'
  { -- | The name of the app.
    app :: Prelude.Text,
    -- | The name of the domain of the app.
    domain :: Prelude.Text,
    -- | The name of the simulation of the app.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'app', 'describeApp_app' - The name of the app.
--
-- 'domain', 'describeApp_domain' - The name of the domain of the app.
--
-- 'simulation', 'describeApp_simulation' - The name of the simulation of the app.
newDescribeApp ::
  -- | 'app'
  Prelude.Text ->
  -- | 'domain'
  Prelude.Text ->
  -- | 'simulation'
  Prelude.Text ->
  DescribeApp
newDescribeApp pApp_ pDomain_ pSimulation_ =
  DescribeApp'
    { app = pApp_,
      domain = pDomain_,
      simulation = pSimulation_
    }

-- | The name of the app.
describeApp_app :: Lens.Lens' DescribeApp Prelude.Text
describeApp_app = Lens.lens (\DescribeApp' {app} -> app) (\s@DescribeApp' {} a -> s {app = a} :: DescribeApp)

-- | The name of the domain of the app.
describeApp_domain :: Lens.Lens' DescribeApp Prelude.Text
describeApp_domain = Lens.lens (\DescribeApp' {domain} -> domain) (\s@DescribeApp' {} a -> s {domain = a} :: DescribeApp)

-- | The name of the simulation of the app.
describeApp_simulation :: Lens.Lens' DescribeApp Prelude.Text
describeApp_simulation = Lens.lens (\DescribeApp' {simulation} -> simulation) (\s@DescribeApp' {} a -> s {simulation = a} :: DescribeApp)

instance Core.AWSRequest DescribeApp where
  type AWSResponse DescribeApp = DescribeAppResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Domain")
            Prelude.<*> (x Data..?> "EndpointInfo")
            Prelude.<*> (x Data..?> "LaunchOverrides")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Simulation")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TargetStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApp where
  hashWithSalt _salt DescribeApp' {..} =
    _salt `Prelude.hashWithSalt` app
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` simulation

instance Prelude.NFData DescribeApp where
  rnf DescribeApp' {..} =
    Prelude.rnf app
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf simulation

instance Data.ToHeaders DescribeApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeApp where
  toPath = Prelude.const "/describeapp"

instance Data.ToQuery DescribeApp where
  toQuery DescribeApp' {..} =
    Prelude.mconcat
      [ "app" Data.=: app,
        "domain" Data.=: domain,
        "simulation" Data.=: simulation
      ]

-- | /See:/ 'newDescribeAppResponse' smart constructor.
data DescribeAppResponse = DescribeAppResponse'
  { -- | The description of the app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain of the app.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Information about the network endpoint for the custom app. You can use
    -- the endpoint to connect to the custom app.
    endpointInfo :: Prelude.Maybe SimulationAppEndpointInfo,
    launchOverrides :: Prelude.Maybe LaunchOverrides,
    -- | The name of the app.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the simulation of the app.
    simulation :: Prelude.Maybe Prelude.Text,
    -- | The current lifecycle state of the custom app.
    status :: Prelude.Maybe SimulationAppStatus,
    -- | The desired lifecycle state of the custom app.
    targetStatus :: Prelude.Maybe SimulationAppTargetStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'describeAppResponse_description' - The description of the app.
--
-- 'domain', 'describeAppResponse_domain' - The name of the domain of the app.
--
-- 'endpointInfo', 'describeAppResponse_endpointInfo' - Information about the network endpoint for the custom app. You can use
-- the endpoint to connect to the custom app.
--
-- 'launchOverrides', 'describeAppResponse_launchOverrides' - Undocumented member.
--
-- 'name', 'describeAppResponse_name' - The name of the app.
--
-- 'simulation', 'describeAppResponse_simulation' - The name of the simulation of the app.
--
-- 'status', 'describeAppResponse_status' - The current lifecycle state of the custom app.
--
-- 'targetStatus', 'describeAppResponse_targetStatus' - The desired lifecycle state of the custom app.
--
-- 'httpStatus', 'describeAppResponse_httpStatus' - The response's http status code.
newDescribeAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAppResponse
newDescribeAppResponse pHttpStatus_ =
  DescribeAppResponse'
    { description = Prelude.Nothing,
      domain = Prelude.Nothing,
      endpointInfo = Prelude.Nothing,
      launchOverrides = Prelude.Nothing,
      name = Prelude.Nothing,
      simulation = Prelude.Nothing,
      status = Prelude.Nothing,
      targetStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the app.
describeAppResponse_description :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_description = Lens.lens (\DescribeAppResponse' {description} -> description) (\s@DescribeAppResponse' {} a -> s {description = a} :: DescribeAppResponse)

-- | The name of the domain of the app.
describeAppResponse_domain :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_domain = Lens.lens (\DescribeAppResponse' {domain} -> domain) (\s@DescribeAppResponse' {} a -> s {domain = a} :: DescribeAppResponse)

-- | Information about the network endpoint for the custom app. You can use
-- the endpoint to connect to the custom app.
describeAppResponse_endpointInfo :: Lens.Lens' DescribeAppResponse (Prelude.Maybe SimulationAppEndpointInfo)
describeAppResponse_endpointInfo = Lens.lens (\DescribeAppResponse' {endpointInfo} -> endpointInfo) (\s@DescribeAppResponse' {} a -> s {endpointInfo = a} :: DescribeAppResponse)

-- | Undocumented member.
describeAppResponse_launchOverrides :: Lens.Lens' DescribeAppResponse (Prelude.Maybe LaunchOverrides)
describeAppResponse_launchOverrides = Lens.lens (\DescribeAppResponse' {launchOverrides} -> launchOverrides) (\s@DescribeAppResponse' {} a -> s {launchOverrides = a} :: DescribeAppResponse)

-- | The name of the app.
describeAppResponse_name :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_name = Lens.lens (\DescribeAppResponse' {name} -> name) (\s@DescribeAppResponse' {} a -> s {name = a} :: DescribeAppResponse)

-- | The name of the simulation of the app.
describeAppResponse_simulation :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_simulation = Lens.lens (\DescribeAppResponse' {simulation} -> simulation) (\s@DescribeAppResponse' {} a -> s {simulation = a} :: DescribeAppResponse)

-- | The current lifecycle state of the custom app.
describeAppResponse_status :: Lens.Lens' DescribeAppResponse (Prelude.Maybe SimulationAppStatus)
describeAppResponse_status = Lens.lens (\DescribeAppResponse' {status} -> status) (\s@DescribeAppResponse' {} a -> s {status = a} :: DescribeAppResponse)

-- | The desired lifecycle state of the custom app.
describeAppResponse_targetStatus :: Lens.Lens' DescribeAppResponse (Prelude.Maybe SimulationAppTargetStatus)
describeAppResponse_targetStatus = Lens.lens (\DescribeAppResponse' {targetStatus} -> targetStatus) (\s@DescribeAppResponse' {} a -> s {targetStatus = a} :: DescribeAppResponse)

-- | The response's http status code.
describeAppResponse_httpStatus :: Lens.Lens' DescribeAppResponse Prelude.Int
describeAppResponse_httpStatus = Lens.lens (\DescribeAppResponse' {httpStatus} -> httpStatus) (\s@DescribeAppResponse' {} a -> s {httpStatus = a} :: DescribeAppResponse)

instance Prelude.NFData DescribeAppResponse where
  rnf DescribeAppResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf endpointInfo
      `Prelude.seq` Prelude.rnf launchOverrides
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf simulation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetStatus
      `Prelude.seq` Prelude.rnf httpStatus
