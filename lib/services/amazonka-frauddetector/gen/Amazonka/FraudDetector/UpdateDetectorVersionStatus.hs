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
-- Module      : Amazonka.FraudDetector.UpdateDetectorVersionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the detector version’s status. You can perform the following
-- promotions or demotions using @UpdateDetectorVersionStatus@: @DRAFT@ to
-- @ACTIVE@, @ACTIVE@ to @INACTIVE@, and @INACTIVE@ to @ACTIVE@.
module Amazonka.FraudDetector.UpdateDetectorVersionStatus
  ( -- * Creating a Request
    UpdateDetectorVersionStatus (..),
    newUpdateDetectorVersionStatus,

    -- * Request Lenses
    updateDetectorVersionStatus_detectorId,
    updateDetectorVersionStatus_detectorVersionId,
    updateDetectorVersionStatus_status,

    -- * Destructuring the Response
    UpdateDetectorVersionStatusResponse (..),
    newUpdateDetectorVersionStatusResponse,

    -- * Response Lenses
    updateDetectorVersionStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDetectorVersionStatus' smart constructor.
data UpdateDetectorVersionStatus = UpdateDetectorVersionStatus'
  { -- | The detector ID.
    detectorId :: Prelude.Text,
    -- | The detector version ID.
    detectorVersionId :: Prelude.Text,
    -- | The new status.
    --
    -- The only supported values are @ACTIVE@ and @INACTIVE@
    status :: DetectorVersionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetectorVersionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'updateDetectorVersionStatus_detectorId' - The detector ID.
--
-- 'detectorVersionId', 'updateDetectorVersionStatus_detectorVersionId' - The detector version ID.
--
-- 'status', 'updateDetectorVersionStatus_status' - The new status.
--
-- The only supported values are @ACTIVE@ and @INACTIVE@
newUpdateDetectorVersionStatus ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'detectorVersionId'
  Prelude.Text ->
  -- | 'status'
  DetectorVersionStatus ->
  UpdateDetectorVersionStatus
newUpdateDetectorVersionStatus
  pDetectorId_
  pDetectorVersionId_
  pStatus_ =
    UpdateDetectorVersionStatus'
      { detectorId =
          pDetectorId_,
        detectorVersionId = pDetectorVersionId_,
        status = pStatus_
      }

-- | The detector ID.
updateDetectorVersionStatus_detectorId :: Lens.Lens' UpdateDetectorVersionStatus Prelude.Text
updateDetectorVersionStatus_detectorId = Lens.lens (\UpdateDetectorVersionStatus' {detectorId} -> detectorId) (\s@UpdateDetectorVersionStatus' {} a -> s {detectorId = a} :: UpdateDetectorVersionStatus)

-- | The detector version ID.
updateDetectorVersionStatus_detectorVersionId :: Lens.Lens' UpdateDetectorVersionStatus Prelude.Text
updateDetectorVersionStatus_detectorVersionId = Lens.lens (\UpdateDetectorVersionStatus' {detectorVersionId} -> detectorVersionId) (\s@UpdateDetectorVersionStatus' {} a -> s {detectorVersionId = a} :: UpdateDetectorVersionStatus)

-- | The new status.
--
-- The only supported values are @ACTIVE@ and @INACTIVE@
updateDetectorVersionStatus_status :: Lens.Lens' UpdateDetectorVersionStatus DetectorVersionStatus
updateDetectorVersionStatus_status = Lens.lens (\UpdateDetectorVersionStatus' {status} -> status) (\s@UpdateDetectorVersionStatus' {} a -> s {status = a} :: UpdateDetectorVersionStatus)

instance Core.AWSRequest UpdateDetectorVersionStatus where
  type
    AWSResponse UpdateDetectorVersionStatus =
      UpdateDetectorVersionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDetectorVersionStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDetectorVersionStatus where
  hashWithSalt _salt UpdateDetectorVersionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` detectorVersionId
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdateDetectorVersionStatus where
  rnf UpdateDetectorVersionStatus' {..} =
    Prelude.rnf detectorId `Prelude.seq`
      Prelude.rnf detectorVersionId `Prelude.seq`
        Prelude.rnf status

instance Data.ToHeaders UpdateDetectorVersionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateDetectorVersionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDetectorVersionStatus where
  toJSON UpdateDetectorVersionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("detectorId" Data..= detectorId),
            Prelude.Just
              ("detectorVersionId" Data..= detectorVersionId),
            Prelude.Just ("status" Data..= status)
          ]
      )

instance Data.ToPath UpdateDetectorVersionStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDetectorVersionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDetectorVersionStatusResponse' smart constructor.
data UpdateDetectorVersionStatusResponse = UpdateDetectorVersionStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetectorVersionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDetectorVersionStatusResponse_httpStatus' - The response's http status code.
newUpdateDetectorVersionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDetectorVersionStatusResponse
newUpdateDetectorVersionStatusResponse pHttpStatus_ =
  UpdateDetectorVersionStatusResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDetectorVersionStatusResponse_httpStatus :: Lens.Lens' UpdateDetectorVersionStatusResponse Prelude.Int
updateDetectorVersionStatusResponse_httpStatus = Lens.lens (\UpdateDetectorVersionStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateDetectorVersionStatusResponse' {} a -> s {httpStatus = a} :: UpdateDetectorVersionStatusResponse)

instance
  Prelude.NFData
    UpdateDetectorVersionStatusResponse
  where
  rnf UpdateDetectorVersionStatusResponse' {..} =
    Prelude.rnf httpStatus
