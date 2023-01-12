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
-- Module      : Amazonka.SimSpaceWeaver.StartApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a custom app with the configuration specified in the simulation
-- schema.
module Amazonka.SimSpaceWeaver.StartApp
  ( -- * Creating a Request
    StartApp (..),
    newStartApp,

    -- * Request Lenses
    startApp_clientToken,
    startApp_description,
    startApp_launchOverrides,
    startApp_domain,
    startApp_name,
    startApp_simulation,

    -- * Destructuring the Response
    StartAppResponse (..),
    newStartAppResponse,

    -- * Response Lenses
    startAppResponse_domain,
    startAppResponse_name,
    startAppResponse_simulation,
    startAppResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newStartApp' smart constructor.
data StartApp = StartApp'
  { -- | A value that you provide to ensure that repeated calls to this API
    -- operation using the same parameters complete only once. A @ClientToken@
    -- is also known as an /idempotency token/. A @ClientToken@ expires after
    -- 24 hours.
    clientToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The description of the app.
    description :: Prelude.Maybe Prelude.Text,
    launchOverrides :: Prelude.Maybe LaunchOverrides,
    -- | The name of the domain of the app.
    domain :: Prelude.Text,
    -- | The name of the app.
    name :: Prelude.Text,
    -- | The name of the simulation of the app.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startApp_clientToken' - A value that you provide to ensure that repeated calls to this API
-- operation using the same parameters complete only once. A @ClientToken@
-- is also known as an /idempotency token/. A @ClientToken@ expires after
-- 24 hours.
--
-- 'description', 'startApp_description' - The description of the app.
--
-- 'launchOverrides', 'startApp_launchOverrides' - Undocumented member.
--
-- 'domain', 'startApp_domain' - The name of the domain of the app.
--
-- 'name', 'startApp_name' - The name of the app.
--
-- 'simulation', 'startApp_simulation' - The name of the simulation of the app.
newStartApp ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'simulation'
  Prelude.Text ->
  StartApp
newStartApp pDomain_ pName_ pSimulation_ =
  StartApp'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      launchOverrides = Prelude.Nothing,
      domain = pDomain_,
      name = pName_,
      simulation = pSimulation_
    }

-- | A value that you provide to ensure that repeated calls to this API
-- operation using the same parameters complete only once. A @ClientToken@
-- is also known as an /idempotency token/. A @ClientToken@ expires after
-- 24 hours.
startApp_clientToken :: Lens.Lens' StartApp (Prelude.Maybe Prelude.Text)
startApp_clientToken = Lens.lens (\StartApp' {clientToken} -> clientToken) (\s@StartApp' {} a -> s {clientToken = a} :: StartApp) Prelude.. Lens.mapping Data._Sensitive

-- | The description of the app.
startApp_description :: Lens.Lens' StartApp (Prelude.Maybe Prelude.Text)
startApp_description = Lens.lens (\StartApp' {description} -> description) (\s@StartApp' {} a -> s {description = a} :: StartApp)

-- | Undocumented member.
startApp_launchOverrides :: Lens.Lens' StartApp (Prelude.Maybe LaunchOverrides)
startApp_launchOverrides = Lens.lens (\StartApp' {launchOverrides} -> launchOverrides) (\s@StartApp' {} a -> s {launchOverrides = a} :: StartApp)

-- | The name of the domain of the app.
startApp_domain :: Lens.Lens' StartApp Prelude.Text
startApp_domain = Lens.lens (\StartApp' {domain} -> domain) (\s@StartApp' {} a -> s {domain = a} :: StartApp)

-- | The name of the app.
startApp_name :: Lens.Lens' StartApp Prelude.Text
startApp_name = Lens.lens (\StartApp' {name} -> name) (\s@StartApp' {} a -> s {name = a} :: StartApp)

-- | The name of the simulation of the app.
startApp_simulation :: Lens.Lens' StartApp Prelude.Text
startApp_simulation = Lens.lens (\StartApp' {simulation} -> simulation) (\s@StartApp' {} a -> s {simulation = a} :: StartApp)

instance Core.AWSRequest StartApp where
  type AWSResponse StartApp = StartAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAppResponse'
            Prelude.<$> (x Data..?> "Domain")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Simulation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartApp where
  hashWithSalt _salt StartApp' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` launchOverrides
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` simulation

instance Prelude.NFData StartApp where
  rnf StartApp' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf launchOverrides
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf simulation

instance Data.ToHeaders StartApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartApp where
  toJSON StartApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("LaunchOverrides" Data..=)
              Prelude.<$> launchOverrides,
            Prelude.Just ("Domain" Data..= domain),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Simulation" Data..= simulation)
          ]
      )

instance Data.ToPath StartApp where
  toPath = Prelude.const "/startapp"

instance Data.ToQuery StartApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAppResponse' smart constructor.
data StartAppResponse = StartAppResponse'
  { -- | The name of the domain of the app.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The name of the app.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the simulation of the app.
    simulation :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'startAppResponse_domain' - The name of the domain of the app.
--
-- 'name', 'startAppResponse_name' - The name of the app.
--
-- 'simulation', 'startAppResponse_simulation' - The name of the simulation of the app.
--
-- 'httpStatus', 'startAppResponse_httpStatus' - The response's http status code.
newStartAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartAppResponse
newStartAppResponse pHttpStatus_ =
  StartAppResponse'
    { domain = Prelude.Nothing,
      name = Prelude.Nothing,
      simulation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the domain of the app.
startAppResponse_domain :: Lens.Lens' StartAppResponse (Prelude.Maybe Prelude.Text)
startAppResponse_domain = Lens.lens (\StartAppResponse' {domain} -> domain) (\s@StartAppResponse' {} a -> s {domain = a} :: StartAppResponse)

-- | The name of the app.
startAppResponse_name :: Lens.Lens' StartAppResponse (Prelude.Maybe Prelude.Text)
startAppResponse_name = Lens.lens (\StartAppResponse' {name} -> name) (\s@StartAppResponse' {} a -> s {name = a} :: StartAppResponse)

-- | The name of the simulation of the app.
startAppResponse_simulation :: Lens.Lens' StartAppResponse (Prelude.Maybe Prelude.Text)
startAppResponse_simulation = Lens.lens (\StartAppResponse' {simulation} -> simulation) (\s@StartAppResponse' {} a -> s {simulation = a} :: StartAppResponse)

-- | The response's http status code.
startAppResponse_httpStatus :: Lens.Lens' StartAppResponse Prelude.Int
startAppResponse_httpStatus = Lens.lens (\StartAppResponse' {httpStatus} -> httpStatus) (\s@StartAppResponse' {} a -> s {httpStatus = a} :: StartAppResponse)

instance Prelude.NFData StartAppResponse where
  rnf StartAppResponse' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf simulation
      `Prelude.seq` Prelude.rnf httpStatus
