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
-- Module      : Amazonka.SageMakerGeoSpatial.DeleteVectorEnrichmentJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a Vector Enrichment job.
module Amazonka.SageMakerGeoSpatial.DeleteVectorEnrichmentJob
  ( -- * Creating a Request
    DeleteVectorEnrichmentJob (..),
    newDeleteVectorEnrichmentJob,

    -- * Request Lenses
    deleteVectorEnrichmentJob_arn,

    -- * Destructuring the Response
    DeleteVectorEnrichmentJobResponse (..),
    newDeleteVectorEnrichmentJobResponse,

    -- * Response Lenses
    deleteVectorEnrichmentJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newDeleteVectorEnrichmentJob' smart constructor.
data DeleteVectorEnrichmentJob = DeleteVectorEnrichmentJob'
  { -- | The Amazon Resource Name (ARN) of the Vector Enrichment job being
    -- deleted.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVectorEnrichmentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteVectorEnrichmentJob_arn' - The Amazon Resource Name (ARN) of the Vector Enrichment job being
-- deleted.
newDeleteVectorEnrichmentJob ::
  -- | 'arn'
  Prelude.Text ->
  DeleteVectorEnrichmentJob
newDeleteVectorEnrichmentJob pArn_ =
  DeleteVectorEnrichmentJob' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the Vector Enrichment job being
-- deleted.
deleteVectorEnrichmentJob_arn :: Lens.Lens' DeleteVectorEnrichmentJob Prelude.Text
deleteVectorEnrichmentJob_arn = Lens.lens (\DeleteVectorEnrichmentJob' {arn} -> arn) (\s@DeleteVectorEnrichmentJob' {} a -> s {arn = a} :: DeleteVectorEnrichmentJob)

instance Core.AWSRequest DeleteVectorEnrichmentJob where
  type
    AWSResponse DeleteVectorEnrichmentJob =
      DeleteVectorEnrichmentJobResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVectorEnrichmentJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVectorEnrichmentJob where
  hashWithSalt _salt DeleteVectorEnrichmentJob' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteVectorEnrichmentJob where
  rnf DeleteVectorEnrichmentJob' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteVectorEnrichmentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVectorEnrichmentJob where
  toPath DeleteVectorEnrichmentJob' {..} =
    Prelude.mconcat
      ["/vector-enrichment-jobs/", Data.toBS arn]

instance Data.ToQuery DeleteVectorEnrichmentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVectorEnrichmentJobResponse' smart constructor.
data DeleteVectorEnrichmentJobResponse = DeleteVectorEnrichmentJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVectorEnrichmentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVectorEnrichmentJobResponse_httpStatus' - The response's http status code.
newDeleteVectorEnrichmentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVectorEnrichmentJobResponse
newDeleteVectorEnrichmentJobResponse pHttpStatus_ =
  DeleteVectorEnrichmentJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteVectorEnrichmentJobResponse_httpStatus :: Lens.Lens' DeleteVectorEnrichmentJobResponse Prelude.Int
deleteVectorEnrichmentJobResponse_httpStatus = Lens.lens (\DeleteVectorEnrichmentJobResponse' {httpStatus} -> httpStatus) (\s@DeleteVectorEnrichmentJobResponse' {} a -> s {httpStatus = a} :: DeleteVectorEnrichmentJobResponse)

instance
  Prelude.NFData
    DeleteVectorEnrichmentJobResponse
  where
  rnf DeleteVectorEnrichmentJobResponse' {..} =
    Prelude.rnf httpStatus
