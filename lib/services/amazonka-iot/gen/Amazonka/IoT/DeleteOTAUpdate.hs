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
-- Module      : Amazonka.IoT.DeleteOTAUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an OTA update.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteOTAUpdate>
-- action.
module Amazonka.IoT.DeleteOTAUpdate
  ( -- * Creating a Request
    DeleteOTAUpdate (..),
    newDeleteOTAUpdate,

    -- * Request Lenses
    deleteOTAUpdate_forceDeleteAWSJob,
    deleteOTAUpdate_deleteStream,
    deleteOTAUpdate_otaUpdateId,

    -- * Destructuring the Response
    DeleteOTAUpdateResponse (..),
    newDeleteOTAUpdateResponse,

    -- * Response Lenses
    deleteOTAUpdateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOTAUpdate' smart constructor.
data DeleteOTAUpdate = DeleteOTAUpdate'
  { -- | When true, deletes the IoT job created by the OTAUpdate process even if
    -- it is \"IN_PROGRESS\". Otherwise, if the job is not in a terminal state
    -- (\"COMPLETED\" or \"CANCELED\") an exception will occur. The default is
    -- false.
    forceDeleteAWSJob :: Prelude.Maybe Prelude.Bool,
    -- | When true, the stream created by the OTAUpdate process is deleted when
    -- the OTA update is deleted. Ignored if the stream specified in the
    -- OTAUpdate is supplied by the user.
    deleteStream :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the OTA update to delete.
    otaUpdateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOTAUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDeleteAWSJob', 'deleteOTAUpdate_forceDeleteAWSJob' - When true, deletes the IoT job created by the OTAUpdate process even if
-- it is \"IN_PROGRESS\". Otherwise, if the job is not in a terminal state
-- (\"COMPLETED\" or \"CANCELED\") an exception will occur. The default is
-- false.
--
-- 'deleteStream', 'deleteOTAUpdate_deleteStream' - When true, the stream created by the OTAUpdate process is deleted when
-- the OTA update is deleted. Ignored if the stream specified in the
-- OTAUpdate is supplied by the user.
--
-- 'otaUpdateId', 'deleteOTAUpdate_otaUpdateId' - The ID of the OTA update to delete.
newDeleteOTAUpdate ::
  -- | 'otaUpdateId'
  Prelude.Text ->
  DeleteOTAUpdate
newDeleteOTAUpdate pOtaUpdateId_ =
  DeleteOTAUpdate'
    { forceDeleteAWSJob =
        Prelude.Nothing,
      deleteStream = Prelude.Nothing,
      otaUpdateId = pOtaUpdateId_
    }

-- | When true, deletes the IoT job created by the OTAUpdate process even if
-- it is \"IN_PROGRESS\". Otherwise, if the job is not in a terminal state
-- (\"COMPLETED\" or \"CANCELED\") an exception will occur. The default is
-- false.
deleteOTAUpdate_forceDeleteAWSJob :: Lens.Lens' DeleteOTAUpdate (Prelude.Maybe Prelude.Bool)
deleteOTAUpdate_forceDeleteAWSJob = Lens.lens (\DeleteOTAUpdate' {forceDeleteAWSJob} -> forceDeleteAWSJob) (\s@DeleteOTAUpdate' {} a -> s {forceDeleteAWSJob = a} :: DeleteOTAUpdate)

-- | When true, the stream created by the OTAUpdate process is deleted when
-- the OTA update is deleted. Ignored if the stream specified in the
-- OTAUpdate is supplied by the user.
deleteOTAUpdate_deleteStream :: Lens.Lens' DeleteOTAUpdate (Prelude.Maybe Prelude.Bool)
deleteOTAUpdate_deleteStream = Lens.lens (\DeleteOTAUpdate' {deleteStream} -> deleteStream) (\s@DeleteOTAUpdate' {} a -> s {deleteStream = a} :: DeleteOTAUpdate)

-- | The ID of the OTA update to delete.
deleteOTAUpdate_otaUpdateId :: Lens.Lens' DeleteOTAUpdate Prelude.Text
deleteOTAUpdate_otaUpdateId = Lens.lens (\DeleteOTAUpdate' {otaUpdateId} -> otaUpdateId) (\s@DeleteOTAUpdate' {} a -> s {otaUpdateId = a} :: DeleteOTAUpdate)

instance Core.AWSRequest DeleteOTAUpdate where
  type
    AWSResponse DeleteOTAUpdate =
      DeleteOTAUpdateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOTAUpdateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOTAUpdate where
  hashWithSalt _salt DeleteOTAUpdate' {..} =
    _salt `Prelude.hashWithSalt` forceDeleteAWSJob
      `Prelude.hashWithSalt` deleteStream
      `Prelude.hashWithSalt` otaUpdateId

instance Prelude.NFData DeleteOTAUpdate where
  rnf DeleteOTAUpdate' {..} =
    Prelude.rnf forceDeleteAWSJob
      `Prelude.seq` Prelude.rnf deleteStream
      `Prelude.seq` Prelude.rnf otaUpdateId

instance Data.ToHeaders DeleteOTAUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteOTAUpdate where
  toPath DeleteOTAUpdate' {..} =
    Prelude.mconcat
      ["/otaUpdates/", Data.toBS otaUpdateId]

instance Data.ToQuery DeleteOTAUpdate where
  toQuery DeleteOTAUpdate' {..} =
    Prelude.mconcat
      [ "forceDeleteAWSJob" Data.=: forceDeleteAWSJob,
        "deleteStream" Data.=: deleteStream
      ]

-- | /See:/ 'newDeleteOTAUpdateResponse' smart constructor.
data DeleteOTAUpdateResponse = DeleteOTAUpdateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOTAUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteOTAUpdateResponse_httpStatus' - The response's http status code.
newDeleteOTAUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOTAUpdateResponse
newDeleteOTAUpdateResponse pHttpStatus_ =
  DeleteOTAUpdateResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteOTAUpdateResponse_httpStatus :: Lens.Lens' DeleteOTAUpdateResponse Prelude.Int
deleteOTAUpdateResponse_httpStatus = Lens.lens (\DeleteOTAUpdateResponse' {httpStatus} -> httpStatus) (\s@DeleteOTAUpdateResponse' {} a -> s {httpStatus = a} :: DeleteOTAUpdateResponse)

instance Prelude.NFData DeleteOTAUpdateResponse where
  rnf DeleteOTAUpdateResponse' {..} =
    Prelude.rnf httpStatus
