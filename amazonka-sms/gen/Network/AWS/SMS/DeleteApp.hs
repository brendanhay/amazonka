{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SMS.DeleteApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Optionally deletes the launched stack
-- associated with the application and all AWS SMS replication jobs for
-- servers in the application.
module Network.AWS.SMS.DeleteApp
  ( -- * Creating a Request
    DeleteApp (..),
    newDeleteApp,

    -- * Request Lenses
    deleteApp_appId,
    deleteApp_forceStopAppReplication,
    deleteApp_forceTerminateApp,

    -- * Destructuring the Response
    DeleteAppResponse (..),
    newDeleteAppResponse,

    -- * Response Lenses
    deleteAppResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to stop all replication jobs corresponding to the
    -- servers in the application while deleting the application.
    forceStopAppReplication :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to terminate the stack corresponding to the
    -- application while deleting the application.
    forceTerminateApp :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteApp_appId' - The ID of the application.
--
-- 'forceStopAppReplication', 'deleteApp_forceStopAppReplication' - Indicates whether to stop all replication jobs corresponding to the
-- servers in the application while deleting the application.
--
-- 'forceTerminateApp', 'deleteApp_forceTerminateApp' - Indicates whether to terminate the stack corresponding to the
-- application while deleting the application.
newDeleteApp ::
  DeleteApp
newDeleteApp =
  DeleteApp'
    { appId = Prelude.Nothing,
      forceStopAppReplication = Prelude.Nothing,
      forceTerminateApp = Prelude.Nothing
    }

-- | The ID of the application.
deleteApp_appId :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Text)
deleteApp_appId = Lens.lens (\DeleteApp' {appId} -> appId) (\s@DeleteApp' {} a -> s {appId = a} :: DeleteApp)

-- | Indicates whether to stop all replication jobs corresponding to the
-- servers in the application while deleting the application.
deleteApp_forceStopAppReplication :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Bool)
deleteApp_forceStopAppReplication = Lens.lens (\DeleteApp' {forceStopAppReplication} -> forceStopAppReplication) (\s@DeleteApp' {} a -> s {forceStopAppReplication = a} :: DeleteApp)

-- | Indicates whether to terminate the stack corresponding to the
-- application while deleting the application.
deleteApp_forceTerminateApp :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Bool)
deleteApp_forceTerminateApp = Lens.lens (\DeleteApp' {forceTerminateApp} -> forceTerminateApp) (\s@DeleteApp' {} a -> s {forceTerminateApp = a} :: DeleteApp)

instance Prelude.AWSRequest DeleteApp where
  type Rs DeleteApp = DeleteAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApp

instance Prelude.NFData DeleteApp

instance Prelude.ToHeaders DeleteApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteApp" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("appId" Prelude..=) Prelude.<$> appId,
            ("forceStopAppReplication" Prelude..=)
              Prelude.<$> forceStopAppReplication,
            ("forceTerminateApp" Prelude..=)
              Prelude.<$> forceTerminateApp
          ]
      )

instance Prelude.ToPath DeleteApp where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteAppResponse
