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
-- Module      : Amazonka.GuardDuty.DeleteDetector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon GuardDuty detector that is specified by the detector
-- ID.
module Amazonka.GuardDuty.DeleteDetector
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
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDetector' smart constructor.
data DeleteDetector = DeleteDetector'
  { -- | The unique ID of the detector that you want to delete.
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
-- 'detectorId', 'deleteDetector_detectorId' - The unique ID of the detector that you want to delete.
newDeleteDetector ::
  -- | 'detectorId'
  Prelude.Text ->
  DeleteDetector
newDeleteDetector pDetectorId_ =
  DeleteDetector' {detectorId = pDetectorId_}

-- | The unique ID of the detector that you want to delete.
deleteDetector_detectorId :: Lens.Lens' DeleteDetector Prelude.Text
deleteDetector_detectorId = Lens.lens (\DeleteDetector' {detectorId} -> detectorId) (\s@DeleteDetector' {} a -> s {detectorId = a} :: DeleteDetector)

instance Core.AWSRequest DeleteDetector where
  type
    AWSResponse DeleteDetector =
      DeleteDetectorResponse
  request overrides =
    Request.delete (overrides defaultService)
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

instance Core.ToHeaders DeleteDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteDetector where
  toPath DeleteDetector' {..} =
    Prelude.mconcat
      ["/detector/", Core.toBS detectorId]

instance Core.ToQuery DeleteDetector where
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
