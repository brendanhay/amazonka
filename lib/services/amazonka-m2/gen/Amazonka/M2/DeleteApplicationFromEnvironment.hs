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
-- Module      : Amazonka.M2.DeleteApplicationFromEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific application from the specific runtime environment
-- where it was previously deployed. You cannot delete a runtime
-- environment using DeleteEnvironment if any application has ever been
-- deployed to it. This API removes the association of the application with
-- the runtime environment so you can delete the environment smoothly.
module Amazonka.M2.DeleteApplicationFromEnvironment
  ( -- * Creating a Request
    DeleteApplicationFromEnvironment (..),
    newDeleteApplicationFromEnvironment,

    -- * Request Lenses
    deleteApplicationFromEnvironment_applicationId,
    deleteApplicationFromEnvironment_environmentId,

    -- * Destructuring the Response
    DeleteApplicationFromEnvironmentResponse (..),
    newDeleteApplicationFromEnvironmentResponse,

    -- * Response Lenses
    deleteApplicationFromEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplicationFromEnvironment' smart constructor.
data DeleteApplicationFromEnvironment = DeleteApplicationFromEnvironment'
  { -- | The unique identifier of the application you want to delete.
    applicationId :: Prelude.Text,
    -- | The unique identifier of the runtime environment where the application
    -- was previously deployed.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationFromEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApplicationFromEnvironment_applicationId' - The unique identifier of the application you want to delete.
--
-- 'environmentId', 'deleteApplicationFromEnvironment_environmentId' - The unique identifier of the runtime environment where the application
-- was previously deployed.
newDeleteApplicationFromEnvironment ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  DeleteApplicationFromEnvironment
newDeleteApplicationFromEnvironment
  pApplicationId_
  pEnvironmentId_ =
    DeleteApplicationFromEnvironment'
      { applicationId =
          pApplicationId_,
        environmentId = pEnvironmentId_
      }

-- | The unique identifier of the application you want to delete.
deleteApplicationFromEnvironment_applicationId :: Lens.Lens' DeleteApplicationFromEnvironment Prelude.Text
deleteApplicationFromEnvironment_applicationId = Lens.lens (\DeleteApplicationFromEnvironment' {applicationId} -> applicationId) (\s@DeleteApplicationFromEnvironment' {} a -> s {applicationId = a} :: DeleteApplicationFromEnvironment)

-- | The unique identifier of the runtime environment where the application
-- was previously deployed.
deleteApplicationFromEnvironment_environmentId :: Lens.Lens' DeleteApplicationFromEnvironment Prelude.Text
deleteApplicationFromEnvironment_environmentId = Lens.lens (\DeleteApplicationFromEnvironment' {environmentId} -> environmentId) (\s@DeleteApplicationFromEnvironment' {} a -> s {environmentId = a} :: DeleteApplicationFromEnvironment)

instance
  Core.AWSRequest
    DeleteApplicationFromEnvironment
  where
  type
    AWSResponse DeleteApplicationFromEnvironment =
      DeleteApplicationFromEnvironmentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationFromEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteApplicationFromEnvironment
  where
  hashWithSalt
    _salt
    DeleteApplicationFromEnvironment' {..} =
      _salt
        `Prelude.hashWithSalt` applicationId
        `Prelude.hashWithSalt` environmentId

instance
  Prelude.NFData
    DeleteApplicationFromEnvironment
  where
  rnf DeleteApplicationFromEnvironment' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf environmentId

instance
  Data.ToHeaders
    DeleteApplicationFromEnvironment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteApplicationFromEnvironment where
  toPath DeleteApplicationFromEnvironment' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/environment/",
        Data.toBS environmentId
      ]

instance
  Data.ToQuery
    DeleteApplicationFromEnvironment
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationFromEnvironmentResponse' smart constructor.
data DeleteApplicationFromEnvironmentResponse = DeleteApplicationFromEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationFromEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApplicationFromEnvironmentResponse_httpStatus' - The response's http status code.
newDeleteApplicationFromEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationFromEnvironmentResponse
newDeleteApplicationFromEnvironmentResponse
  pHttpStatus_ =
    DeleteApplicationFromEnvironmentResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteApplicationFromEnvironmentResponse_httpStatus :: Lens.Lens' DeleteApplicationFromEnvironmentResponse Prelude.Int
deleteApplicationFromEnvironmentResponse_httpStatus = Lens.lens (\DeleteApplicationFromEnvironmentResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationFromEnvironmentResponse' {} a -> s {httpStatus = a} :: DeleteApplicationFromEnvironmentResponse)

instance
  Prelude.NFData
    DeleteApplicationFromEnvironmentResponse
  where
  rnf DeleteApplicationFromEnvironmentResponse' {..} =
    Prelude.rnf httpStatus
