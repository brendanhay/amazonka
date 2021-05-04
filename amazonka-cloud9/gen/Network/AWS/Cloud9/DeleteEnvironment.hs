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
-- Module      : Network.AWS.Cloud9.DeleteEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Cloud9 development environment. If an Amazon EC2 instance
-- is connected to the environment, also terminates the instance.
module Network.AWS.Cloud9.DeleteEnvironment
  ( -- * Creating a Request
    DeleteEnvironment (..),
    newDeleteEnvironment,

    -- * Request Lenses
    deleteEnvironment_environmentId,

    -- * Destructuring the Response
    DeleteEnvironmentResponse (..),
    newDeleteEnvironmentResponse,

    -- * Response Lenses
    deleteEnvironmentResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEnvironment' smart constructor.
data DeleteEnvironment = DeleteEnvironment'
  { -- | The ID of the environment to delete.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'deleteEnvironment_environmentId' - The ID of the environment to delete.
newDeleteEnvironment ::
  -- | 'environmentId'
  Prelude.Text ->
  DeleteEnvironment
newDeleteEnvironment pEnvironmentId_ =
  DeleteEnvironment' {environmentId = pEnvironmentId_}

-- | The ID of the environment to delete.
deleteEnvironment_environmentId :: Lens.Lens' DeleteEnvironment Prelude.Text
deleteEnvironment_environmentId = Lens.lens (\DeleteEnvironment' {environmentId} -> environmentId) (\s@DeleteEnvironment' {} a -> s {environmentId = a} :: DeleteEnvironment)

instance Prelude.AWSRequest DeleteEnvironment where
  type Rs DeleteEnvironment = DeleteEnvironmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEnvironment

instance Prelude.NFData DeleteEnvironment

instance Prelude.ToHeaders DeleteEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCloud9WorkspaceManagementService.DeleteEnvironment" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteEnvironment where
  toJSON DeleteEnvironment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("environmentId" Prelude..= environmentId)
          ]
      )

instance Prelude.ToPath DeleteEnvironment where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEnvironmentResponse' smart constructor.
data DeleteEnvironmentResponse = DeleteEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEnvironmentResponse_httpStatus' - The response's http status code.
newDeleteEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEnvironmentResponse
newDeleteEnvironmentResponse pHttpStatus_ =
  DeleteEnvironmentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEnvironmentResponse_httpStatus :: Lens.Lens' DeleteEnvironmentResponse Prelude.Int
deleteEnvironmentResponse_httpStatus = Lens.lens (\DeleteEnvironmentResponse' {httpStatus} -> httpStatus) (\s@DeleteEnvironmentResponse' {} a -> s {httpStatus = a} :: DeleteEnvironmentResponse)

instance Prelude.NFData DeleteEnvironmentResponse
