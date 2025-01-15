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
-- Module      : Amazonka.KinesisAnalyticsV2.DeleteApplicationSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot of application state.
module Amazonka.KinesisAnalyticsV2.DeleteApplicationSnapshot
  ( -- * Creating a Request
    DeleteApplicationSnapshot (..),
    newDeleteApplicationSnapshot,

    -- * Request Lenses
    deleteApplicationSnapshot_applicationName,
    deleteApplicationSnapshot_snapshotName,
    deleteApplicationSnapshot_snapshotCreationTimestamp,

    -- * Destructuring the Response
    DeleteApplicationSnapshotResponse (..),
    newDeleteApplicationSnapshotResponse,

    -- * Response Lenses
    deleteApplicationSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplicationSnapshot' smart constructor.
data DeleteApplicationSnapshot = DeleteApplicationSnapshot'
  { -- | The name of an existing application.
    applicationName :: Prelude.Text,
    -- | The identifier for the snapshot delete.
    snapshotName :: Prelude.Text,
    -- | The creation timestamp of the application snapshot to delete. You can
    -- retrieve this value using or .
    snapshotCreationTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteApplicationSnapshot_applicationName' - The name of an existing application.
--
-- 'snapshotName', 'deleteApplicationSnapshot_snapshotName' - The identifier for the snapshot delete.
--
-- 'snapshotCreationTimestamp', 'deleteApplicationSnapshot_snapshotCreationTimestamp' - The creation timestamp of the application snapshot to delete. You can
-- retrieve this value using or .
newDeleteApplicationSnapshot ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  -- | 'snapshotCreationTimestamp'
  Prelude.UTCTime ->
  DeleteApplicationSnapshot
newDeleteApplicationSnapshot
  pApplicationName_
  pSnapshotName_
  pSnapshotCreationTimestamp_ =
    DeleteApplicationSnapshot'
      { applicationName =
          pApplicationName_,
        snapshotName = pSnapshotName_,
        snapshotCreationTimestamp =
          Data._Time Lens.# pSnapshotCreationTimestamp_
      }

-- | The name of an existing application.
deleteApplicationSnapshot_applicationName :: Lens.Lens' DeleteApplicationSnapshot Prelude.Text
deleteApplicationSnapshot_applicationName = Lens.lens (\DeleteApplicationSnapshot' {applicationName} -> applicationName) (\s@DeleteApplicationSnapshot' {} a -> s {applicationName = a} :: DeleteApplicationSnapshot)

-- | The identifier for the snapshot delete.
deleteApplicationSnapshot_snapshotName :: Lens.Lens' DeleteApplicationSnapshot Prelude.Text
deleteApplicationSnapshot_snapshotName = Lens.lens (\DeleteApplicationSnapshot' {snapshotName} -> snapshotName) (\s@DeleteApplicationSnapshot' {} a -> s {snapshotName = a} :: DeleteApplicationSnapshot)

-- | The creation timestamp of the application snapshot to delete. You can
-- retrieve this value using or .
deleteApplicationSnapshot_snapshotCreationTimestamp :: Lens.Lens' DeleteApplicationSnapshot Prelude.UTCTime
deleteApplicationSnapshot_snapshotCreationTimestamp = Lens.lens (\DeleteApplicationSnapshot' {snapshotCreationTimestamp} -> snapshotCreationTimestamp) (\s@DeleteApplicationSnapshot' {} a -> s {snapshotCreationTimestamp = a} :: DeleteApplicationSnapshot) Prelude.. Data._Time

instance Core.AWSRequest DeleteApplicationSnapshot where
  type
    AWSResponse DeleteApplicationSnapshot =
      DeleteApplicationSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationSnapshotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApplicationSnapshot where
  hashWithSalt _salt DeleteApplicationSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` snapshotCreationTimestamp

instance Prelude.NFData DeleteApplicationSnapshot where
  rnf DeleteApplicationSnapshot' {..} =
    Prelude.rnf applicationName `Prelude.seq`
      Prelude.rnf snapshotName `Prelude.seq`
        Prelude.rnf snapshotCreationTimestamp

instance Data.ToHeaders DeleteApplicationSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.DeleteApplicationSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteApplicationSnapshot where
  toJSON DeleteApplicationSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just ("SnapshotName" Data..= snapshotName),
            Prelude.Just
              ( "SnapshotCreationTimestamp"
                  Data..= snapshotCreationTimestamp
              )
          ]
      )

instance Data.ToPath DeleteApplicationSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteApplicationSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationSnapshotResponse' smart constructor.
data DeleteApplicationSnapshotResponse = DeleteApplicationSnapshotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApplicationSnapshotResponse_httpStatus' - The response's http status code.
newDeleteApplicationSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationSnapshotResponse
newDeleteApplicationSnapshotResponse pHttpStatus_ =
  DeleteApplicationSnapshotResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteApplicationSnapshotResponse_httpStatus :: Lens.Lens' DeleteApplicationSnapshotResponse Prelude.Int
deleteApplicationSnapshotResponse_httpStatus = Lens.lens (\DeleteApplicationSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteApplicationSnapshotResponse)

instance
  Prelude.NFData
    DeleteApplicationSnapshotResponse
  where
  rnf DeleteApplicationSnapshotResponse' {..} =
    Prelude.rnf httpStatus
