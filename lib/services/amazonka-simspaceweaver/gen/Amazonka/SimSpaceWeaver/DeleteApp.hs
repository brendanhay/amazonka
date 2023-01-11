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
-- Module      : Amazonka.SimSpaceWeaver.DeleteApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the instance of the given custom app.
module Amazonka.SimSpaceWeaver.DeleteApp
  ( -- * Creating a Request
    DeleteApp (..),
    newDeleteApp,

    -- * Request Lenses
    deleteApp_app,
    deleteApp_domain,
    deleteApp_simulation,

    -- * Destructuring the Response
    DeleteAppResponse (..),
    newDeleteAppResponse,

    -- * Response Lenses
    deleteAppResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | The name of the app.
    app :: Prelude.Text,
    -- | The name of the domain of the app.
    domain :: Prelude.Text,
    -- | The name of the simulation of the app.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'app', 'deleteApp_app' - The name of the app.
--
-- 'domain', 'deleteApp_domain' - The name of the domain of the app.
--
-- 'simulation', 'deleteApp_simulation' - The name of the simulation of the app.
newDeleteApp ::
  -- | 'app'
  Prelude.Text ->
  -- | 'domain'
  Prelude.Text ->
  -- | 'simulation'
  Prelude.Text ->
  DeleteApp
newDeleteApp pApp_ pDomain_ pSimulation_ =
  DeleteApp'
    { app = pApp_,
      domain = pDomain_,
      simulation = pSimulation_
    }

-- | The name of the app.
deleteApp_app :: Lens.Lens' DeleteApp Prelude.Text
deleteApp_app = Lens.lens (\DeleteApp' {app} -> app) (\s@DeleteApp' {} a -> s {app = a} :: DeleteApp)

-- | The name of the domain of the app.
deleteApp_domain :: Lens.Lens' DeleteApp Prelude.Text
deleteApp_domain = Lens.lens (\DeleteApp' {domain} -> domain) (\s@DeleteApp' {} a -> s {domain = a} :: DeleteApp)

-- | The name of the simulation of the app.
deleteApp_simulation :: Lens.Lens' DeleteApp Prelude.Text
deleteApp_simulation = Lens.lens (\DeleteApp' {simulation} -> simulation) (\s@DeleteApp' {} a -> s {simulation = a} :: DeleteApp)

instance Core.AWSRequest DeleteApp where
  type AWSResponse DeleteApp = DeleteAppResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApp where
  hashWithSalt _salt DeleteApp' {..} =
    _salt `Prelude.hashWithSalt` app
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` simulation

instance Prelude.NFData DeleteApp where
  rnf DeleteApp' {..} =
    Prelude.rnf app
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf simulation

instance Data.ToHeaders DeleteApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteApp where
  toPath = Prelude.const "/deleteapp"

instance Data.ToQuery DeleteApp where
  toQuery DeleteApp' {..} =
    Prelude.mconcat
      [ "app" Data.=: app,
        "domain" Data.=: domain,
        "simulation" Data.=: simulation
      ]

-- | /See:/ 'newDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAppResponse_httpStatus' - The response's http status code.
newDeleteAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAppResponse
newDeleteAppResponse pHttpStatus_ =
  DeleteAppResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteAppResponse_httpStatus :: Lens.Lens' DeleteAppResponse Prelude.Int
deleteAppResponse_httpStatus = Lens.lens (\DeleteAppResponse' {httpStatus} -> httpStatus) (\s@DeleteAppResponse' {} a -> s {httpStatus = a} :: DeleteAppResponse)

instance Prelude.NFData DeleteAppResponse where
  rnf DeleteAppResponse' {..} = Prelude.rnf httpStatus
