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
-- Module      : Amazonka.Batch.DeleteComputeEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Batch compute environment.
--
-- Before you can delete a compute environment, you must set its state to
-- @DISABLED@ with the UpdateComputeEnvironment API operation and
-- disassociate it from any job queues with the UpdateJobQueue API
-- operation. Compute environments that use Fargate resources must
-- terminate all active jobs on that compute environment before deleting
-- the compute environment. If this isn\'t done, the compute environment
-- enters an invalid state.
module Amazonka.Batch.DeleteComputeEnvironment
  ( -- * Creating a Request
    DeleteComputeEnvironment (..),
    newDeleteComputeEnvironment,

    -- * Request Lenses
    deleteComputeEnvironment_computeEnvironment,

    -- * Destructuring the Response
    DeleteComputeEnvironmentResponse (..),
    newDeleteComputeEnvironmentResponse,

    -- * Response Lenses
    deleteComputeEnvironmentResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @DeleteComputeEnvironment@.
--
-- /See:/ 'newDeleteComputeEnvironment' smart constructor.
data DeleteComputeEnvironment = DeleteComputeEnvironment'
  { -- | The name or Amazon Resource Name (ARN) of the compute environment to
    -- delete.
    computeEnvironment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComputeEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeEnvironment', 'deleteComputeEnvironment_computeEnvironment' - The name or Amazon Resource Name (ARN) of the compute environment to
-- delete.
newDeleteComputeEnvironment ::
  -- | 'computeEnvironment'
  Prelude.Text ->
  DeleteComputeEnvironment
newDeleteComputeEnvironment pComputeEnvironment_ =
  DeleteComputeEnvironment'
    { computeEnvironment =
        pComputeEnvironment_
    }

-- | The name or Amazon Resource Name (ARN) of the compute environment to
-- delete.
deleteComputeEnvironment_computeEnvironment :: Lens.Lens' DeleteComputeEnvironment Prelude.Text
deleteComputeEnvironment_computeEnvironment = Lens.lens (\DeleteComputeEnvironment' {computeEnvironment} -> computeEnvironment) (\s@DeleteComputeEnvironment' {} a -> s {computeEnvironment = a} :: DeleteComputeEnvironment)

instance Core.AWSRequest DeleteComputeEnvironment where
  type
    AWSResponse DeleteComputeEnvironment =
      DeleteComputeEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteComputeEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteComputeEnvironment where
  hashWithSalt _salt DeleteComputeEnvironment' {..} =
    _salt `Prelude.hashWithSalt` computeEnvironment

instance Prelude.NFData DeleteComputeEnvironment where
  rnf DeleteComputeEnvironment' {..} =
    Prelude.rnf computeEnvironment

instance Data.ToHeaders DeleteComputeEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteComputeEnvironment where
  toJSON DeleteComputeEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("computeEnvironment" Data..= computeEnvironment)
          ]
      )

instance Data.ToPath DeleteComputeEnvironment where
  toPath = Prelude.const "/v1/deletecomputeenvironment"

instance Data.ToQuery DeleteComputeEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteComputeEnvironmentResponse' smart constructor.
data DeleteComputeEnvironmentResponse = DeleteComputeEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComputeEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteComputeEnvironmentResponse_httpStatus' - The response's http status code.
newDeleteComputeEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteComputeEnvironmentResponse
newDeleteComputeEnvironmentResponse pHttpStatus_ =
  DeleteComputeEnvironmentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteComputeEnvironmentResponse_httpStatus :: Lens.Lens' DeleteComputeEnvironmentResponse Prelude.Int
deleteComputeEnvironmentResponse_httpStatus = Lens.lens (\DeleteComputeEnvironmentResponse' {httpStatus} -> httpStatus) (\s@DeleteComputeEnvironmentResponse' {} a -> s {httpStatus = a} :: DeleteComputeEnvironmentResponse)

instance
  Prelude.NFData
    DeleteComputeEnvironmentResponse
  where
  rnf DeleteComputeEnvironmentResponse' {..} =
    Prelude.rnf httpStatus
