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
-- Module      : Amazonka.Pinpoint.DeleteUserEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all the endpoints that are associated with a specific user ID.
module Amazonka.Pinpoint.DeleteUserEndpoints
  ( -- * Creating a Request
    DeleteUserEndpoints (..),
    newDeleteUserEndpoints,

    -- * Request Lenses
    deleteUserEndpoints_applicationId,
    deleteUserEndpoints_userId,

    -- * Destructuring the Response
    DeleteUserEndpointsResponse (..),
    newDeleteUserEndpointsResponse,

    -- * Response Lenses
    deleteUserEndpointsResponse_httpStatus,
    deleteUserEndpointsResponse_endpointsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUserEndpoints' smart constructor.
data DeleteUserEndpoints = DeleteUserEndpoints'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteUserEndpoints_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'userId', 'deleteUserEndpoints_userId' - The unique identifier for the user.
newDeleteUserEndpoints ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DeleteUserEndpoints
newDeleteUserEndpoints pApplicationId_ pUserId_ =
  DeleteUserEndpoints'
    { applicationId =
        pApplicationId_,
      userId = pUserId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteUserEndpoints_applicationId :: Lens.Lens' DeleteUserEndpoints Prelude.Text
deleteUserEndpoints_applicationId = Lens.lens (\DeleteUserEndpoints' {applicationId} -> applicationId) (\s@DeleteUserEndpoints' {} a -> s {applicationId = a} :: DeleteUserEndpoints)

-- | The unique identifier for the user.
deleteUserEndpoints_userId :: Lens.Lens' DeleteUserEndpoints Prelude.Text
deleteUserEndpoints_userId = Lens.lens (\DeleteUserEndpoints' {userId} -> userId) (\s@DeleteUserEndpoints' {} a -> s {userId = a} :: DeleteUserEndpoints)

instance Core.AWSRequest DeleteUserEndpoints where
  type
    AWSResponse DeleteUserEndpoints =
      DeleteUserEndpointsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteUserEndpointsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteUserEndpoints where
  hashWithSalt _salt DeleteUserEndpoints' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DeleteUserEndpoints where
  rnf DeleteUserEndpoints' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf userId

instance Core.ToHeaders DeleteUserEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteUserEndpoints where
  toPath DeleteUserEndpoints' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/users/",
        Core.toBS userId
      ]

instance Core.ToQuery DeleteUserEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserEndpointsResponse' smart constructor.
data DeleteUserEndpointsResponse = DeleteUserEndpointsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    endpointsResponse :: EndpointsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserEndpointsResponse_httpStatus' - The response's http status code.
--
-- 'endpointsResponse', 'deleteUserEndpointsResponse_endpointsResponse' - Undocumented member.
newDeleteUserEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'endpointsResponse'
  EndpointsResponse ->
  DeleteUserEndpointsResponse
newDeleteUserEndpointsResponse
  pHttpStatus_
  pEndpointsResponse_ =
    DeleteUserEndpointsResponse'
      { httpStatus =
          pHttpStatus_,
        endpointsResponse = pEndpointsResponse_
      }

-- | The response's http status code.
deleteUserEndpointsResponse_httpStatus :: Lens.Lens' DeleteUserEndpointsResponse Prelude.Int
deleteUserEndpointsResponse_httpStatus = Lens.lens (\DeleteUserEndpointsResponse' {httpStatus} -> httpStatus) (\s@DeleteUserEndpointsResponse' {} a -> s {httpStatus = a} :: DeleteUserEndpointsResponse)

-- | Undocumented member.
deleteUserEndpointsResponse_endpointsResponse :: Lens.Lens' DeleteUserEndpointsResponse EndpointsResponse
deleteUserEndpointsResponse_endpointsResponse = Lens.lens (\DeleteUserEndpointsResponse' {endpointsResponse} -> endpointsResponse) (\s@DeleteUserEndpointsResponse' {} a -> s {endpointsResponse = a} :: DeleteUserEndpointsResponse)

instance Prelude.NFData DeleteUserEndpointsResponse where
  rnf DeleteUserEndpointsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpointsResponse
