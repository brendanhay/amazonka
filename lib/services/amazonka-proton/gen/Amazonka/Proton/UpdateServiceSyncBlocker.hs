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
-- Module      : Amazonka.Proton.UpdateServiceSyncBlocker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the service sync blocker by resolving it.
module Amazonka.Proton.UpdateServiceSyncBlocker
  ( -- * Creating a Request
    UpdateServiceSyncBlocker (..),
    newUpdateServiceSyncBlocker,

    -- * Request Lenses
    updateServiceSyncBlocker_id,
    updateServiceSyncBlocker_resolvedReason,

    -- * Destructuring the Response
    UpdateServiceSyncBlockerResponse (..),
    newUpdateServiceSyncBlockerResponse,

    -- * Response Lenses
    updateServiceSyncBlockerResponse_serviceInstanceName,
    updateServiceSyncBlockerResponse_httpStatus,
    updateServiceSyncBlockerResponse_serviceName,
    updateServiceSyncBlockerResponse_serviceSyncBlocker,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServiceSyncBlocker' smart constructor.
data UpdateServiceSyncBlocker = UpdateServiceSyncBlocker'
  { -- | The ID of the service sync blocker.
    id :: Prelude.Text,
    -- | The reason the service sync blocker was resolved.
    resolvedReason :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSyncBlocker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateServiceSyncBlocker_id' - The ID of the service sync blocker.
--
-- 'resolvedReason', 'updateServiceSyncBlocker_resolvedReason' - The reason the service sync blocker was resolved.
newUpdateServiceSyncBlocker ::
  -- | 'id'
  Prelude.Text ->
  -- | 'resolvedReason'
  Prelude.Text ->
  UpdateServiceSyncBlocker
newUpdateServiceSyncBlocker pId_ pResolvedReason_ =
  UpdateServiceSyncBlocker'
    { id = pId_,
      resolvedReason = pResolvedReason_
    }

-- | The ID of the service sync blocker.
updateServiceSyncBlocker_id :: Lens.Lens' UpdateServiceSyncBlocker Prelude.Text
updateServiceSyncBlocker_id = Lens.lens (\UpdateServiceSyncBlocker' {id} -> id) (\s@UpdateServiceSyncBlocker' {} a -> s {id = a} :: UpdateServiceSyncBlocker)

-- | The reason the service sync blocker was resolved.
updateServiceSyncBlocker_resolvedReason :: Lens.Lens' UpdateServiceSyncBlocker Prelude.Text
updateServiceSyncBlocker_resolvedReason = Lens.lens (\UpdateServiceSyncBlocker' {resolvedReason} -> resolvedReason) (\s@UpdateServiceSyncBlocker' {} a -> s {resolvedReason = a} :: UpdateServiceSyncBlocker)

instance Core.AWSRequest UpdateServiceSyncBlocker where
  type
    AWSResponse UpdateServiceSyncBlocker =
      UpdateServiceSyncBlockerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceSyncBlockerResponse'
            Prelude.<$> (x Data..?> "serviceInstanceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "serviceName")
            Prelude.<*> (x Data..:> "serviceSyncBlocker")
      )

instance Prelude.Hashable UpdateServiceSyncBlocker where
  hashWithSalt _salt UpdateServiceSyncBlocker' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` resolvedReason

instance Prelude.NFData UpdateServiceSyncBlocker where
  rnf UpdateServiceSyncBlocker' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf resolvedReason

instance Data.ToHeaders UpdateServiceSyncBlocker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateServiceSyncBlocker" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServiceSyncBlocker where
  toJSON UpdateServiceSyncBlocker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Data..= id),
            Prelude.Just
              ("resolvedReason" Data..= resolvedReason)
          ]
      )

instance Data.ToPath UpdateServiceSyncBlocker where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateServiceSyncBlocker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceSyncBlockerResponse' smart constructor.
data UpdateServiceSyncBlockerResponse = UpdateServiceSyncBlockerResponse'
  { -- | The name of the service instance that you want to update the service
    -- sync blocker for.
    serviceInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the service that you want to update the service sync blocker
    -- for.
    serviceName :: Prelude.Text,
    -- | The detailed data on the service sync blocker that was updated.
    serviceSyncBlocker :: SyncBlocker
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSyncBlockerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceInstanceName', 'updateServiceSyncBlockerResponse_serviceInstanceName' - The name of the service instance that you want to update the service
-- sync blocker for.
--
-- 'httpStatus', 'updateServiceSyncBlockerResponse_httpStatus' - The response's http status code.
--
-- 'serviceName', 'updateServiceSyncBlockerResponse_serviceName' - The name of the service that you want to update the service sync blocker
-- for.
--
-- 'serviceSyncBlocker', 'updateServiceSyncBlockerResponse_serviceSyncBlocker' - The detailed data on the service sync blocker that was updated.
newUpdateServiceSyncBlockerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'serviceSyncBlocker'
  SyncBlocker ->
  UpdateServiceSyncBlockerResponse
newUpdateServiceSyncBlockerResponse
  pHttpStatus_
  pServiceName_
  pServiceSyncBlocker_ =
    UpdateServiceSyncBlockerResponse'
      { serviceInstanceName =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        serviceName = pServiceName_,
        serviceSyncBlocker = pServiceSyncBlocker_
      }

-- | The name of the service instance that you want to update the service
-- sync blocker for.
updateServiceSyncBlockerResponse_serviceInstanceName :: Lens.Lens' UpdateServiceSyncBlockerResponse (Prelude.Maybe Prelude.Text)
updateServiceSyncBlockerResponse_serviceInstanceName = Lens.lens (\UpdateServiceSyncBlockerResponse' {serviceInstanceName} -> serviceInstanceName) (\s@UpdateServiceSyncBlockerResponse' {} a -> s {serviceInstanceName = a} :: UpdateServiceSyncBlockerResponse)

-- | The response's http status code.
updateServiceSyncBlockerResponse_httpStatus :: Lens.Lens' UpdateServiceSyncBlockerResponse Prelude.Int
updateServiceSyncBlockerResponse_httpStatus = Lens.lens (\UpdateServiceSyncBlockerResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceSyncBlockerResponse' {} a -> s {httpStatus = a} :: UpdateServiceSyncBlockerResponse)

-- | The name of the service that you want to update the service sync blocker
-- for.
updateServiceSyncBlockerResponse_serviceName :: Lens.Lens' UpdateServiceSyncBlockerResponse Prelude.Text
updateServiceSyncBlockerResponse_serviceName = Lens.lens (\UpdateServiceSyncBlockerResponse' {serviceName} -> serviceName) (\s@UpdateServiceSyncBlockerResponse' {} a -> s {serviceName = a} :: UpdateServiceSyncBlockerResponse)

-- | The detailed data on the service sync blocker that was updated.
updateServiceSyncBlockerResponse_serviceSyncBlocker :: Lens.Lens' UpdateServiceSyncBlockerResponse SyncBlocker
updateServiceSyncBlockerResponse_serviceSyncBlocker = Lens.lens (\UpdateServiceSyncBlockerResponse' {serviceSyncBlocker} -> serviceSyncBlocker) (\s@UpdateServiceSyncBlockerResponse' {} a -> s {serviceSyncBlocker = a} :: UpdateServiceSyncBlockerResponse)

instance
  Prelude.NFData
    UpdateServiceSyncBlockerResponse
  where
  rnf UpdateServiceSyncBlockerResponse' {..} =
    Prelude.rnf serviceInstanceName
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceSyncBlocker
