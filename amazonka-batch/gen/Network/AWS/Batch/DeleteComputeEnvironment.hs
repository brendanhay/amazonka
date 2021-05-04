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
-- Module      : Network.AWS.Batch.DeleteComputeEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Batch compute environment.
--
-- Before you can delete a compute environment, you must set its state to
-- @DISABLED@ with the UpdateComputeEnvironment API operation and
-- disassociate it from any job queues with the UpdateJobQueue API
-- operation. Compute environments that use AWS Fargate resources must
-- terminate all active jobs on that compute environment before deleting
-- the compute environment. If this isn\'t done, the compute environment
-- will end up in an invalid state.
module Network.AWS.Batch.DeleteComputeEnvironment
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

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @DeleteComputeEnvironment@.
--
-- /See:/ 'newDeleteComputeEnvironment' smart constructor.
data DeleteComputeEnvironment = DeleteComputeEnvironment'
  { -- | The name or Amazon Resource Name (ARN) of the compute environment to
    -- delete.
    computeEnvironment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteComputeEnvironment where
  type
    Rs DeleteComputeEnvironment =
      DeleteComputeEnvironmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteComputeEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteComputeEnvironment

instance Prelude.NFData DeleteComputeEnvironment

instance Prelude.ToHeaders DeleteComputeEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteComputeEnvironment where
  toJSON DeleteComputeEnvironment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "computeEnvironment"
                  Prelude..= computeEnvironment
              )
          ]
      )

instance Prelude.ToPath DeleteComputeEnvironment where
  toPath = Prelude.const "/v1/deletecomputeenvironment"

instance Prelude.ToQuery DeleteComputeEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteComputeEnvironmentResponse' smart constructor.
data DeleteComputeEnvironmentResponse = DeleteComputeEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
