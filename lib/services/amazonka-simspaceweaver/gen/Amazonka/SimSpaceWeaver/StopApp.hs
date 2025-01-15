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
-- Module      : Amazonka.SimSpaceWeaver.StopApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the given custom app and shuts down all of its allocated compute
-- resources.
module Amazonka.SimSpaceWeaver.StopApp
  ( -- * Creating a Request
    StopApp (..),
    newStopApp,

    -- * Request Lenses
    stopApp_app,
    stopApp_domain,
    stopApp_simulation,

    -- * Destructuring the Response
    StopAppResponse (..),
    newStopAppResponse,

    -- * Response Lenses
    stopAppResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newStopApp' smart constructor.
data StopApp = StopApp'
  { -- | The name of the app.
    app :: Prelude.Text,
    -- | The name of the domain of the app.
    domain :: Prelude.Text,
    -- | The name of the simulation of the app.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'app', 'stopApp_app' - The name of the app.
--
-- 'domain', 'stopApp_domain' - The name of the domain of the app.
--
-- 'simulation', 'stopApp_simulation' - The name of the simulation of the app.
newStopApp ::
  -- | 'app'
  Prelude.Text ->
  -- | 'domain'
  Prelude.Text ->
  -- | 'simulation'
  Prelude.Text ->
  StopApp
newStopApp pApp_ pDomain_ pSimulation_ =
  StopApp'
    { app = pApp_,
      domain = pDomain_,
      simulation = pSimulation_
    }

-- | The name of the app.
stopApp_app :: Lens.Lens' StopApp Prelude.Text
stopApp_app = Lens.lens (\StopApp' {app} -> app) (\s@StopApp' {} a -> s {app = a} :: StopApp)

-- | The name of the domain of the app.
stopApp_domain :: Lens.Lens' StopApp Prelude.Text
stopApp_domain = Lens.lens (\StopApp' {domain} -> domain) (\s@StopApp' {} a -> s {domain = a} :: StopApp)

-- | The name of the simulation of the app.
stopApp_simulation :: Lens.Lens' StopApp Prelude.Text
stopApp_simulation = Lens.lens (\StopApp' {simulation} -> simulation) (\s@StopApp' {} a -> s {simulation = a} :: StopApp)

instance Core.AWSRequest StopApp where
  type AWSResponse StopApp = StopAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopApp where
  hashWithSalt _salt StopApp' {..} =
    _salt
      `Prelude.hashWithSalt` app
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` simulation

instance Prelude.NFData StopApp where
  rnf StopApp' {..} =
    Prelude.rnf app `Prelude.seq`
      Prelude.rnf domain `Prelude.seq`
        Prelude.rnf simulation

instance Data.ToHeaders StopApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopApp where
  toJSON StopApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("App" Data..= app),
            Prelude.Just ("Domain" Data..= domain),
            Prelude.Just ("Simulation" Data..= simulation)
          ]
      )

instance Data.ToPath StopApp where
  toPath = Prelude.const "/stopapp"

instance Data.ToQuery StopApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopAppResponse' smart constructor.
data StopAppResponse = StopAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopAppResponse_httpStatus' - The response's http status code.
newStopAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopAppResponse
newStopAppResponse pHttpStatus_ =
  StopAppResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopAppResponse_httpStatus :: Lens.Lens' StopAppResponse Prelude.Int
stopAppResponse_httpStatus = Lens.lens (\StopAppResponse' {httpStatus} -> httpStatus) (\s@StopAppResponse' {} a -> s {httpStatus = a} :: StopAppResponse)

instance Prelude.NFData StopAppResponse where
  rnf StopAppResponse' {..} = Prelude.rnf httpStatus
