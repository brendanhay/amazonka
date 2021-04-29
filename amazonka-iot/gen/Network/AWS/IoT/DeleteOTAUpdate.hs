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
-- Module      : Network.AWS.IoT.DeleteOTAUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an OTA update.
module Network.AWS.IoT.DeleteOTAUpdate
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteOTAUpdate' smart constructor.
data DeleteOTAUpdate = DeleteOTAUpdate'
  { -- | When true, deletes the AWS job created by the OTAUpdate process even if
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOTAUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDeleteAWSJob', 'deleteOTAUpdate_forceDeleteAWSJob' - When true, deletes the AWS job created by the OTAUpdate process even if
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

-- | When true, deletes the AWS job created by the OTAUpdate process even if
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

instance Prelude.AWSRequest DeleteOTAUpdate where
  type Rs DeleteOTAUpdate = DeleteOTAUpdateResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOTAUpdateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOTAUpdate

instance Prelude.NFData DeleteOTAUpdate

instance Prelude.ToHeaders DeleteOTAUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteOTAUpdate where
  toPath DeleteOTAUpdate' {..} =
    Prelude.mconcat
      ["/otaUpdates/", Prelude.toBS otaUpdateId]

instance Prelude.ToQuery DeleteOTAUpdate where
  toQuery DeleteOTAUpdate' {..} =
    Prelude.mconcat
      [ "forceDeleteAWSJob" Prelude.=: forceDeleteAWSJob,
        "deleteStream" Prelude.=: deleteStream
      ]

-- | /See:/ 'newDeleteOTAUpdateResponse' smart constructor.
data DeleteOTAUpdateResponse = DeleteOTAUpdateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteOTAUpdateResponse
