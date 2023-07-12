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
-- Module      : Amazonka.LookoutMetrics.DeleteAnomalyDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a detector. Deleting an anomaly detector will delete all of its
-- corresponding resources including any configured datasets and alerts.
module Amazonka.LookoutMetrics.DeleteAnomalyDetector
  ( -- * Creating a Request
    DeleteAnomalyDetector (..),
    newDeleteAnomalyDetector,

    -- * Request Lenses
    deleteAnomalyDetector_anomalyDetectorArn,

    -- * Destructuring the Response
    DeleteAnomalyDetectorResponse (..),
    newDeleteAnomalyDetectorResponse,

    -- * Response Lenses
    deleteAnomalyDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAnomalyDetector' smart constructor.
data DeleteAnomalyDetector = DeleteAnomalyDetector'
  { -- | The ARN of the detector to delete.
    anomalyDetectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorArn', 'deleteAnomalyDetector_anomalyDetectorArn' - The ARN of the detector to delete.
newDeleteAnomalyDetector ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  DeleteAnomalyDetector
newDeleteAnomalyDetector pAnomalyDetectorArn_ =
  DeleteAnomalyDetector'
    { anomalyDetectorArn =
        pAnomalyDetectorArn_
    }

-- | The ARN of the detector to delete.
deleteAnomalyDetector_anomalyDetectorArn :: Lens.Lens' DeleteAnomalyDetector Prelude.Text
deleteAnomalyDetector_anomalyDetectorArn = Lens.lens (\DeleteAnomalyDetector' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@DeleteAnomalyDetector' {} a -> s {anomalyDetectorArn = a} :: DeleteAnomalyDetector)

instance Core.AWSRequest DeleteAnomalyDetector where
  type
    AWSResponse DeleteAnomalyDetector =
      DeleteAnomalyDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAnomalyDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAnomalyDetector where
  hashWithSalt _salt DeleteAnomalyDetector' {..} =
    _salt `Prelude.hashWithSalt` anomalyDetectorArn

instance Prelude.NFData DeleteAnomalyDetector where
  rnf DeleteAnomalyDetector' {..} =
    Prelude.rnf anomalyDetectorArn

instance Data.ToHeaders DeleteAnomalyDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAnomalyDetector where
  toJSON DeleteAnomalyDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn)
          ]
      )

instance Data.ToPath DeleteAnomalyDetector where
  toPath = Prelude.const "/DeleteAnomalyDetector"

instance Data.ToQuery DeleteAnomalyDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAnomalyDetectorResponse' smart constructor.
data DeleteAnomalyDetectorResponse = DeleteAnomalyDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnomalyDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAnomalyDetectorResponse_httpStatus' - The response's http status code.
newDeleteAnomalyDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAnomalyDetectorResponse
newDeleteAnomalyDetectorResponse pHttpStatus_ =
  DeleteAnomalyDetectorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAnomalyDetectorResponse_httpStatus :: Lens.Lens' DeleteAnomalyDetectorResponse Prelude.Int
deleteAnomalyDetectorResponse_httpStatus = Lens.lens (\DeleteAnomalyDetectorResponse' {httpStatus} -> httpStatus) (\s@DeleteAnomalyDetectorResponse' {} a -> s {httpStatus = a} :: DeleteAnomalyDetectorResponse)

instance Prelude.NFData DeleteAnomalyDetectorResponse where
  rnf DeleteAnomalyDetectorResponse' {..} =
    Prelude.rnf httpStatus
