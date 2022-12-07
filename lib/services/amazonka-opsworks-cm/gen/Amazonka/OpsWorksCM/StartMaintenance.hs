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
-- Module      : Amazonka.OpsWorksCM.StartMaintenance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Manually starts server maintenance. This command can be useful if an
-- earlier maintenance attempt failed, and the underlying cause of
-- maintenance failure has been resolved. The server is in an
-- @UNDER_MAINTENANCE@ state while maintenance is in progress.
--
-- Maintenance can only be started on servers in @HEALTHY@ and @UNHEALTHY@
-- states. Otherwise, an @InvalidStateException@ is thrown. A
-- @ResourceNotFoundException@ is thrown when the server does not exist. A
-- @ValidationException@ is raised when parameters of the request are not
-- valid.
module Amazonka.OpsWorksCM.StartMaintenance
  ( -- * Creating a Request
    StartMaintenance (..),
    newStartMaintenance,

    -- * Request Lenses
    startMaintenance_engineAttributes,
    startMaintenance_serverName,

    -- * Destructuring the Response
    StartMaintenanceResponse (..),
    newStartMaintenanceResponse,

    -- * Response Lenses
    startMaintenanceResponse_server,
    startMaintenanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartMaintenance' smart constructor.
data StartMaintenance = StartMaintenance'
  { -- | Engine attributes that are specific to the server on which you want to
    -- run maintenance.
    --
    -- __Attributes accepted in a StartMaintenance request for Chef__
    --
    -- -   @CHEF_MAJOR_UPGRADE@: If a Chef Automate server is eligible for
    --     upgrade to Chef Automate 2, add this engine attribute to a
    --     @StartMaintenance@ request and set the value to @true@ to upgrade
    --     the server to Chef Automate 2. For more information, see
    --     <https://docs.aws.amazon.com/opsworks/latest/userguide/opscm-a2upgrade.html Upgrade an AWS OpsWorks for Chef Automate Server to Chef Automate 2>.
    engineAttributes :: Prelude.Maybe [EngineAttribute],
    -- | The name of the server on which to run maintenance.
    serverName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMaintenance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineAttributes', 'startMaintenance_engineAttributes' - Engine attributes that are specific to the server on which you want to
-- run maintenance.
--
-- __Attributes accepted in a StartMaintenance request for Chef__
--
-- -   @CHEF_MAJOR_UPGRADE@: If a Chef Automate server is eligible for
--     upgrade to Chef Automate 2, add this engine attribute to a
--     @StartMaintenance@ request and set the value to @true@ to upgrade
--     the server to Chef Automate 2. For more information, see
--     <https://docs.aws.amazon.com/opsworks/latest/userguide/opscm-a2upgrade.html Upgrade an AWS OpsWorks for Chef Automate Server to Chef Automate 2>.
--
-- 'serverName', 'startMaintenance_serverName' - The name of the server on which to run maintenance.
newStartMaintenance ::
  -- | 'serverName'
  Prelude.Text ->
  StartMaintenance
newStartMaintenance pServerName_ =
  StartMaintenance'
    { engineAttributes =
        Prelude.Nothing,
      serverName = pServerName_
    }

-- | Engine attributes that are specific to the server on which you want to
-- run maintenance.
--
-- __Attributes accepted in a StartMaintenance request for Chef__
--
-- -   @CHEF_MAJOR_UPGRADE@: If a Chef Automate server is eligible for
--     upgrade to Chef Automate 2, add this engine attribute to a
--     @StartMaintenance@ request and set the value to @true@ to upgrade
--     the server to Chef Automate 2. For more information, see
--     <https://docs.aws.amazon.com/opsworks/latest/userguide/opscm-a2upgrade.html Upgrade an AWS OpsWorks for Chef Automate Server to Chef Automate 2>.
startMaintenance_engineAttributes :: Lens.Lens' StartMaintenance (Prelude.Maybe [EngineAttribute])
startMaintenance_engineAttributes = Lens.lens (\StartMaintenance' {engineAttributes} -> engineAttributes) (\s@StartMaintenance' {} a -> s {engineAttributes = a} :: StartMaintenance) Prelude.. Lens.mapping Lens.coerced

-- | The name of the server on which to run maintenance.
startMaintenance_serverName :: Lens.Lens' StartMaintenance Prelude.Text
startMaintenance_serverName = Lens.lens (\StartMaintenance' {serverName} -> serverName) (\s@StartMaintenance' {} a -> s {serverName = a} :: StartMaintenance)

instance Core.AWSRequest StartMaintenance where
  type
    AWSResponse StartMaintenance =
      StartMaintenanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMaintenanceResponse'
            Prelude.<$> (x Data..?> "Server")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMaintenance where
  hashWithSalt _salt StartMaintenance' {..} =
    _salt `Prelude.hashWithSalt` engineAttributes
      `Prelude.hashWithSalt` serverName

instance Prelude.NFData StartMaintenance where
  rnf StartMaintenance' {..} =
    Prelude.rnf engineAttributes
      `Prelude.seq` Prelude.rnf serverName

instance Data.ToHeaders StartMaintenance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorksCM_V2016_11_01.StartMaintenance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMaintenance where
  toJSON StartMaintenance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EngineAttributes" Data..=)
              Prelude.<$> engineAttributes,
            Prelude.Just ("ServerName" Data..= serverName)
          ]
      )

instance Data.ToPath StartMaintenance where
  toPath = Prelude.const "/"

instance Data.ToQuery StartMaintenance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMaintenanceResponse' smart constructor.
data StartMaintenanceResponse = StartMaintenanceResponse'
  { -- | Contains the response to a @StartMaintenance@ request.
    server :: Prelude.Maybe Server,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMaintenanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'server', 'startMaintenanceResponse_server' - Contains the response to a @StartMaintenance@ request.
--
-- 'httpStatus', 'startMaintenanceResponse_httpStatus' - The response's http status code.
newStartMaintenanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMaintenanceResponse
newStartMaintenanceResponse pHttpStatus_ =
  StartMaintenanceResponse'
    { server = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the response to a @StartMaintenance@ request.
startMaintenanceResponse_server :: Lens.Lens' StartMaintenanceResponse (Prelude.Maybe Server)
startMaintenanceResponse_server = Lens.lens (\StartMaintenanceResponse' {server} -> server) (\s@StartMaintenanceResponse' {} a -> s {server = a} :: StartMaintenanceResponse)

-- | The response's http status code.
startMaintenanceResponse_httpStatus :: Lens.Lens' StartMaintenanceResponse Prelude.Int
startMaintenanceResponse_httpStatus = Lens.lens (\StartMaintenanceResponse' {httpStatus} -> httpStatus) (\s@StartMaintenanceResponse' {} a -> s {httpStatus = a} :: StartMaintenanceResponse)

instance Prelude.NFData StartMaintenanceResponse where
  rnf StartMaintenanceResponse' {..} =
    Prelude.rnf server
      `Prelude.seq` Prelude.rnf httpStatus
