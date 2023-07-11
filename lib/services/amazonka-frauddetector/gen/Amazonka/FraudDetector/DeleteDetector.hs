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
-- Module      : Amazonka.FraudDetector.DeleteDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the detector. Before deleting a detector, you must first delete
-- all detector versions and rule versions associated with the detector.
--
-- When you delete a detector, Amazon Fraud Detector permanently deletes
-- the detector and the data is no longer stored in Amazon Fraud Detector.
module Amazonka.FraudDetector.DeleteDetector
  ( -- * Creating a Request
    DeleteDetector (..),
    newDeleteDetector,

    -- * Request Lenses
    deleteDetector_detectorId,

    -- * Destructuring the Response
    DeleteDetectorResponse (..),
    newDeleteDetectorResponse,

    -- * Response Lenses
    deleteDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDetector' smart constructor.
data DeleteDetector = DeleteDetector'
  { -- | The ID of the detector to delete.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'deleteDetector_detectorId' - The ID of the detector to delete.
newDeleteDetector ::
  -- | 'detectorId'
  Prelude.Text ->
  DeleteDetector
newDeleteDetector pDetectorId_ =
  DeleteDetector' {detectorId = pDetectorId_}

-- | The ID of the detector to delete.
deleteDetector_detectorId :: Lens.Lens' DeleteDetector Prelude.Text
deleteDetector_detectorId = Lens.lens (\DeleteDetector' {detectorId} -> detectorId) (\s@DeleteDetector' {} a -> s {detectorId = a} :: DeleteDetector)

instance Core.AWSRequest DeleteDetector where
  type
    AWSResponse DeleteDetector =
      DeleteDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDetector where
  hashWithSalt _salt DeleteDetector' {..} =
    _salt `Prelude.hashWithSalt` detectorId

instance Prelude.NFData DeleteDetector where
  rnf DeleteDetector' {..} = Prelude.rnf detectorId

instance Data.ToHeaders DeleteDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.DeleteDetector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDetector where
  toJSON DeleteDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("detectorId" Data..= detectorId)]
      )

instance Data.ToPath DeleteDetector where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDetectorResponse' smart constructor.
data DeleteDetectorResponse = DeleteDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDetectorResponse_httpStatus' - The response's http status code.
newDeleteDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDetectorResponse
newDeleteDetectorResponse pHttpStatus_ =
  DeleteDetectorResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDetectorResponse_httpStatus :: Lens.Lens' DeleteDetectorResponse Prelude.Int
deleteDetectorResponse_httpStatus = Lens.lens (\DeleteDetectorResponse' {httpStatus} -> httpStatus) (\s@DeleteDetectorResponse' {} a -> s {httpStatus = a} :: DeleteDetectorResponse)

instance Prelude.NFData DeleteDetectorResponse where
  rnf DeleteDetectorResponse' {..} =
    Prelude.rnf httpStatus
