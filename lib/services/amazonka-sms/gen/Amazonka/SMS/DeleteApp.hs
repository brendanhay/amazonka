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
-- Module      : Amazonka.SMS.DeleteApp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Optionally deletes the launched stack
-- associated with the application and all Server Migration Service
-- replication jobs for servers in the application.
module Amazonka.SMS.DeleteApp
  ( -- * Creating a Request
    DeleteApp (..),
    newDeleteApp,

    -- * Request Lenses
    deleteApp_forceStopAppReplication,
    deleteApp_forceTerminateApp,
    deleteApp_appId,

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
import Amazonka.SMS.Types

-- | /See:/ 'newDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | Indicates whether to stop all replication jobs corresponding to the
    -- servers in the application while deleting the application.
    forceStopAppReplication :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to terminate the stack corresponding to the
    -- application while deleting the application.
    forceTerminateApp :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
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
-- 'forceStopAppReplication', 'deleteApp_forceStopAppReplication' - Indicates whether to stop all replication jobs corresponding to the
-- servers in the application while deleting the application.
--
-- 'forceTerminateApp', 'deleteApp_forceTerminateApp' - Indicates whether to terminate the stack corresponding to the
-- application while deleting the application.
--
-- 'appId', 'deleteApp_appId' - The ID of the application.
newDeleteApp ::
  DeleteApp
newDeleteApp =
  DeleteApp'
    { forceStopAppReplication =
        Prelude.Nothing,
      forceTerminateApp = Prelude.Nothing,
      appId = Prelude.Nothing
    }

-- | Indicates whether to stop all replication jobs corresponding to the
-- servers in the application while deleting the application.
deleteApp_forceStopAppReplication :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Bool)
deleteApp_forceStopAppReplication = Lens.lens (\DeleteApp' {forceStopAppReplication} -> forceStopAppReplication) (\s@DeleteApp' {} a -> s {forceStopAppReplication = a} :: DeleteApp)

-- | Indicates whether to terminate the stack corresponding to the
-- application while deleting the application.
deleteApp_forceTerminateApp :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Bool)
deleteApp_forceTerminateApp = Lens.lens (\DeleteApp' {forceTerminateApp} -> forceTerminateApp) (\s@DeleteApp' {} a -> s {forceTerminateApp = a} :: DeleteApp)

-- | The ID of the application.
deleteApp_appId :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Text)
deleteApp_appId = Lens.lens (\DeleteApp' {appId} -> appId) (\s@DeleteApp' {} a -> s {appId = a} :: DeleteApp)

instance Core.AWSRequest DeleteApp where
  type AWSResponse DeleteApp = DeleteAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApp where
  hashWithSalt _salt DeleteApp' {..} =
    _salt
      `Prelude.hashWithSalt` forceStopAppReplication
      `Prelude.hashWithSalt` forceTerminateApp
      `Prelude.hashWithSalt` appId

instance Prelude.NFData DeleteApp where
  rnf DeleteApp' {..} =
    Prelude.rnf forceStopAppReplication
      `Prelude.seq` Prelude.rnf forceTerminateApp
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders DeleteApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.DeleteApp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("forceStopAppReplication" Data..=)
              Prelude.<$> forceStopAppReplication,
            ("forceTerminateApp" Data..=)
              Prelude.<$> forceTerminateApp,
            ("appId" Data..=) Prelude.<$> appId
          ]
      )

instance Data.ToPath DeleteApp where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteApp where
  toQuery = Prelude.const Prelude.mempty

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
