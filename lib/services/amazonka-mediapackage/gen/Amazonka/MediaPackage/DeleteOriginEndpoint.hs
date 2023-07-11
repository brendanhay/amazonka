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
-- Module      : Amazonka.MediaPackage.DeleteOriginEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing OriginEndpoint.
module Amazonka.MediaPackage.DeleteOriginEndpoint
  ( -- * Creating a Request
    DeleteOriginEndpoint (..),
    newDeleteOriginEndpoint,

    -- * Request Lenses
    deleteOriginEndpoint_id,

    -- * Destructuring the Response
    DeleteOriginEndpointResponse (..),
    newDeleteOriginEndpointResponse,

    -- * Response Lenses
    deleteOriginEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOriginEndpoint' smart constructor.
data DeleteOriginEndpoint = DeleteOriginEndpoint'
  { -- | The ID of the OriginEndpoint to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteOriginEndpoint_id' - The ID of the OriginEndpoint to delete.
newDeleteOriginEndpoint ::
  -- | 'id'
  Prelude.Text ->
  DeleteOriginEndpoint
newDeleteOriginEndpoint pId_ =
  DeleteOriginEndpoint' {id = pId_}

-- | The ID of the OriginEndpoint to delete.
deleteOriginEndpoint_id :: Lens.Lens' DeleteOriginEndpoint Prelude.Text
deleteOriginEndpoint_id = Lens.lens (\DeleteOriginEndpoint' {id} -> id) (\s@DeleteOriginEndpoint' {} a -> s {id = a} :: DeleteOriginEndpoint)

instance Core.AWSRequest DeleteOriginEndpoint where
  type
    AWSResponse DeleteOriginEndpoint =
      DeleteOriginEndpointResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOriginEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOriginEndpoint where
  hashWithSalt _salt DeleteOriginEndpoint' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteOriginEndpoint where
  rnf DeleteOriginEndpoint' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteOriginEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteOriginEndpoint where
  toPath DeleteOriginEndpoint' {..} =
    Prelude.mconcat
      ["/origin_endpoints/", Data.toBS id]

instance Data.ToQuery DeleteOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOriginEndpointResponse' smart constructor.
data DeleteOriginEndpointResponse = DeleteOriginEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteOriginEndpointResponse_httpStatus' - The response's http status code.
newDeleteOriginEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOriginEndpointResponse
newDeleteOriginEndpointResponse pHttpStatus_ =
  DeleteOriginEndpointResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteOriginEndpointResponse_httpStatus :: Lens.Lens' DeleteOriginEndpointResponse Prelude.Int
deleteOriginEndpointResponse_httpStatus = Lens.lens (\DeleteOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteOriginEndpointResponse' {} a -> s {httpStatus = a} :: DeleteOriginEndpointResponse)

instance Prelude.NFData DeleteOriginEndpointResponse where
  rnf DeleteOriginEndpointResponse' {..} =
    Prelude.rnf httpStatus
