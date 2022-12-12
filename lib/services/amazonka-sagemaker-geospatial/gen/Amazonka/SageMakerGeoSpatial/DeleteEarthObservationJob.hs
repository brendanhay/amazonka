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
-- Module      : Amazonka.SageMakerGeoSpatial.DeleteEarthObservationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete an Earth Observation job.
module Amazonka.SageMakerGeoSpatial.DeleteEarthObservationJob
  ( -- * Creating a Request
    DeleteEarthObservationJob (..),
    newDeleteEarthObservationJob,

    -- * Request Lenses
    deleteEarthObservationJob_arn,

    -- * Destructuring the Response
    DeleteEarthObservationJobResponse (..),
    newDeleteEarthObservationJobResponse,

    -- * Response Lenses
    deleteEarthObservationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newDeleteEarthObservationJob' smart constructor.
data DeleteEarthObservationJob = DeleteEarthObservationJob'
  { -- | The Amazon Resource Name (ARN) of the Earth Observation job being
    -- deleted.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEarthObservationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteEarthObservationJob_arn' - The Amazon Resource Name (ARN) of the Earth Observation job being
-- deleted.
newDeleteEarthObservationJob ::
  -- | 'arn'
  Prelude.Text ->
  DeleteEarthObservationJob
newDeleteEarthObservationJob pArn_ =
  DeleteEarthObservationJob' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the Earth Observation job being
-- deleted.
deleteEarthObservationJob_arn :: Lens.Lens' DeleteEarthObservationJob Prelude.Text
deleteEarthObservationJob_arn = Lens.lens (\DeleteEarthObservationJob' {arn} -> arn) (\s@DeleteEarthObservationJob' {} a -> s {arn = a} :: DeleteEarthObservationJob)

instance Core.AWSRequest DeleteEarthObservationJob where
  type
    AWSResponse DeleteEarthObservationJob =
      DeleteEarthObservationJobResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEarthObservationJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEarthObservationJob where
  hashWithSalt _salt DeleteEarthObservationJob' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteEarthObservationJob where
  rnf DeleteEarthObservationJob' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteEarthObservationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEarthObservationJob where
  toPath DeleteEarthObservationJob' {..} =
    Prelude.mconcat
      ["/earth-observation-jobs/", Data.toBS arn]

instance Data.ToQuery DeleteEarthObservationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEarthObservationJobResponse' smart constructor.
data DeleteEarthObservationJobResponse = DeleteEarthObservationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEarthObservationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEarthObservationJobResponse_httpStatus' - The response's http status code.
newDeleteEarthObservationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEarthObservationJobResponse
newDeleteEarthObservationJobResponse pHttpStatus_ =
  DeleteEarthObservationJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEarthObservationJobResponse_httpStatus :: Lens.Lens' DeleteEarthObservationJobResponse Prelude.Int
deleteEarthObservationJobResponse_httpStatus = Lens.lens (\DeleteEarthObservationJobResponse' {httpStatus} -> httpStatus) (\s@DeleteEarthObservationJobResponse' {} a -> s {httpStatus = a} :: DeleteEarthObservationJobResponse)

instance
  Prelude.NFData
    DeleteEarthObservationJobResponse
  where
  rnf DeleteEarthObservationJobResponse' {..} =
    Prelude.rnf httpStatus
