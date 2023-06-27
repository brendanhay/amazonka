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
-- Module      : Amazonka.Proton.DeleteServiceSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the Proton Ops file.
module Amazonka.Proton.DeleteServiceSyncConfig
  ( -- * Creating a Request
    DeleteServiceSyncConfig (..),
    newDeleteServiceSyncConfig,

    -- * Request Lenses
    deleteServiceSyncConfig_serviceName,

    -- * Destructuring the Response
    DeleteServiceSyncConfigResponse (..),
    newDeleteServiceSyncConfigResponse,

    -- * Response Lenses
    deleteServiceSyncConfigResponse_serviceSyncConfig,
    deleteServiceSyncConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteServiceSyncConfig' smart constructor.
data DeleteServiceSyncConfig = DeleteServiceSyncConfig'
  { -- | The name of the service that you want to delete the service sync
    -- configuration for.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'deleteServiceSyncConfig_serviceName' - The name of the service that you want to delete the service sync
-- configuration for.
newDeleteServiceSyncConfig ::
  -- | 'serviceName'
  Prelude.Text ->
  DeleteServiceSyncConfig
newDeleteServiceSyncConfig pServiceName_ =
  DeleteServiceSyncConfig'
    { serviceName =
        pServiceName_
    }

-- | The name of the service that you want to delete the service sync
-- configuration for.
deleteServiceSyncConfig_serviceName :: Lens.Lens' DeleteServiceSyncConfig Prelude.Text
deleteServiceSyncConfig_serviceName = Lens.lens (\DeleteServiceSyncConfig' {serviceName} -> serviceName) (\s@DeleteServiceSyncConfig' {} a -> s {serviceName = a} :: DeleteServiceSyncConfig)

instance Core.AWSRequest DeleteServiceSyncConfig where
  type
    AWSResponse DeleteServiceSyncConfig =
      DeleteServiceSyncConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteServiceSyncConfigResponse'
            Prelude.<$> (x Data..?> "serviceSyncConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteServiceSyncConfig where
  hashWithSalt _salt DeleteServiceSyncConfig' {..} =
    _salt `Prelude.hashWithSalt` serviceName

instance Prelude.NFData DeleteServiceSyncConfig where
  rnf DeleteServiceSyncConfig' {..} =
    Prelude.rnf serviceName

instance Data.ToHeaders DeleteServiceSyncConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.DeleteServiceSyncConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteServiceSyncConfig where
  toJSON DeleteServiceSyncConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("serviceName" Data..= serviceName)]
      )

instance Data.ToPath DeleteServiceSyncConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteServiceSyncConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceSyncConfigResponse' smart constructor.
data DeleteServiceSyncConfigResponse = DeleteServiceSyncConfigResponse'
  { -- | The detailed data for the service sync config.
    serviceSyncConfig :: Prelude.Maybe ServiceSyncConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceSyncConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSyncConfig', 'deleteServiceSyncConfigResponse_serviceSyncConfig' - The detailed data for the service sync config.
--
-- 'httpStatus', 'deleteServiceSyncConfigResponse_httpStatus' - The response's http status code.
newDeleteServiceSyncConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceSyncConfigResponse
newDeleteServiceSyncConfigResponse pHttpStatus_ =
  DeleteServiceSyncConfigResponse'
    { serviceSyncConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data for the service sync config.
deleteServiceSyncConfigResponse_serviceSyncConfig :: Lens.Lens' DeleteServiceSyncConfigResponse (Prelude.Maybe ServiceSyncConfig)
deleteServiceSyncConfigResponse_serviceSyncConfig = Lens.lens (\DeleteServiceSyncConfigResponse' {serviceSyncConfig} -> serviceSyncConfig) (\s@DeleteServiceSyncConfigResponse' {} a -> s {serviceSyncConfig = a} :: DeleteServiceSyncConfigResponse)

-- | The response's http status code.
deleteServiceSyncConfigResponse_httpStatus :: Lens.Lens' DeleteServiceSyncConfigResponse Prelude.Int
deleteServiceSyncConfigResponse_httpStatus = Lens.lens (\DeleteServiceSyncConfigResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceSyncConfigResponse' {} a -> s {httpStatus = a} :: DeleteServiceSyncConfigResponse)

instance
  Prelude.NFData
    DeleteServiceSyncConfigResponse
  where
  rnf DeleteServiceSyncConfigResponse' {..} =
    Prelude.rnf serviceSyncConfig
      `Prelude.seq` Prelude.rnf httpStatus
