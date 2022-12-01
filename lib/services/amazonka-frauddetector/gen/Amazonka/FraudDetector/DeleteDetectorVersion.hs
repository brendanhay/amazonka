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
-- Module      : Amazonka.FraudDetector.DeleteDetectorVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the detector version. You cannot delete detector versions that
-- are in @ACTIVE@ status.
--
-- When you delete a detector version, Amazon Fraud Detector permanently
-- deletes the detector and the data is no longer stored in Amazon Fraud
-- Detector.
module Amazonka.FraudDetector.DeleteDetectorVersion
  ( -- * Creating a Request
    DeleteDetectorVersion (..),
    newDeleteDetectorVersion,

    -- * Request Lenses
    deleteDetectorVersion_detectorId,
    deleteDetectorVersion_detectorVersionId,

    -- * Destructuring the Response
    DeleteDetectorVersionResponse (..),
    newDeleteDetectorVersionResponse,

    -- * Response Lenses
    deleteDetectorVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDetectorVersion' smart constructor.
data DeleteDetectorVersion = DeleteDetectorVersion'
  { -- | The ID of the parent detector for the detector version to delete.
    detectorId :: Prelude.Text,
    -- | The ID of the detector version to delete.
    detectorVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDetectorVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'deleteDetectorVersion_detectorId' - The ID of the parent detector for the detector version to delete.
--
-- 'detectorVersionId', 'deleteDetectorVersion_detectorVersionId' - The ID of the detector version to delete.
newDeleteDetectorVersion ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'detectorVersionId'
  Prelude.Text ->
  DeleteDetectorVersion
newDeleteDetectorVersion
  pDetectorId_
  pDetectorVersionId_ =
    DeleteDetectorVersion'
      { detectorId = pDetectorId_,
        detectorVersionId = pDetectorVersionId_
      }

-- | The ID of the parent detector for the detector version to delete.
deleteDetectorVersion_detectorId :: Lens.Lens' DeleteDetectorVersion Prelude.Text
deleteDetectorVersion_detectorId = Lens.lens (\DeleteDetectorVersion' {detectorId} -> detectorId) (\s@DeleteDetectorVersion' {} a -> s {detectorId = a} :: DeleteDetectorVersion)

-- | The ID of the detector version to delete.
deleteDetectorVersion_detectorVersionId :: Lens.Lens' DeleteDetectorVersion Prelude.Text
deleteDetectorVersion_detectorVersionId = Lens.lens (\DeleteDetectorVersion' {detectorVersionId} -> detectorVersionId) (\s@DeleteDetectorVersion' {} a -> s {detectorVersionId = a} :: DeleteDetectorVersion)

instance Core.AWSRequest DeleteDetectorVersion where
  type
    AWSResponse DeleteDetectorVersion =
      DeleteDetectorVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDetectorVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDetectorVersion where
  hashWithSalt _salt DeleteDetectorVersion' {..} =
    _salt `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` detectorVersionId

instance Prelude.NFData DeleteDetectorVersion where
  rnf DeleteDetectorVersion' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf detectorVersionId

instance Core.ToHeaders DeleteDetectorVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.DeleteDetectorVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteDetectorVersion where
  toJSON DeleteDetectorVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("detectorId" Core..= detectorId),
            Prelude.Just
              ("detectorVersionId" Core..= detectorVersionId)
          ]
      )

instance Core.ToPath DeleteDetectorVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDetectorVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDetectorVersionResponse' smart constructor.
data DeleteDetectorVersionResponse = DeleteDetectorVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDetectorVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDetectorVersionResponse_httpStatus' - The response's http status code.
newDeleteDetectorVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDetectorVersionResponse
newDeleteDetectorVersionResponse pHttpStatus_ =
  DeleteDetectorVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDetectorVersionResponse_httpStatus :: Lens.Lens' DeleteDetectorVersionResponse Prelude.Int
deleteDetectorVersionResponse_httpStatus = Lens.lens (\DeleteDetectorVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteDetectorVersionResponse' {} a -> s {httpStatus = a} :: DeleteDetectorVersionResponse)

instance Prelude.NFData DeleteDetectorVersionResponse where
  rnf DeleteDetectorVersionResponse' {..} =
    Prelude.rnf httpStatus
