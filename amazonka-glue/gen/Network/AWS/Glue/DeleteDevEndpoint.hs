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
-- Module      : Network.AWS.Glue.DeleteDevEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified development endpoint.
module Network.AWS.Glue.DeleteDevEndpoint
  ( -- * Creating a Request
    DeleteDevEndpoint (..),
    newDeleteDevEndpoint,

    -- * Request Lenses
    deleteDevEndpoint_endpointName,

    -- * Destructuring the Response
    DeleteDevEndpointResponse (..),
    newDeleteDevEndpointResponse,

    -- * Response Lenses
    deleteDevEndpointResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDevEndpoint' smart constructor.
data DeleteDevEndpoint = DeleteDevEndpoint'
  { -- | The name of the @DevEndpoint@.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDevEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'deleteDevEndpoint_endpointName' - The name of the @DevEndpoint@.
newDeleteDevEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  DeleteDevEndpoint
newDeleteDevEndpoint pEndpointName_ =
  DeleteDevEndpoint' {endpointName = pEndpointName_}

-- | The name of the @DevEndpoint@.
deleteDevEndpoint_endpointName :: Lens.Lens' DeleteDevEndpoint Prelude.Text
deleteDevEndpoint_endpointName = Lens.lens (\DeleteDevEndpoint' {endpointName} -> endpointName) (\s@DeleteDevEndpoint' {} a -> s {endpointName = a} :: DeleteDevEndpoint)

instance Prelude.AWSRequest DeleteDevEndpoint where
  type Rs DeleteDevEndpoint = DeleteDevEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDevEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDevEndpoint

instance Prelude.NFData DeleteDevEndpoint

instance Prelude.ToHeaders DeleteDevEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.DeleteDevEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDevEndpoint where
  toJSON DeleteDevEndpoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointName" Prelude..= endpointName)
          ]
      )

instance Prelude.ToPath DeleteDevEndpoint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDevEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDevEndpointResponse' smart constructor.
data DeleteDevEndpointResponse = DeleteDevEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDevEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDevEndpointResponse_httpStatus' - The response's http status code.
newDeleteDevEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDevEndpointResponse
newDeleteDevEndpointResponse pHttpStatus_ =
  DeleteDevEndpointResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDevEndpointResponse_httpStatus :: Lens.Lens' DeleteDevEndpointResponse Prelude.Int
deleteDevEndpointResponse_httpStatus = Lens.lens (\DeleteDevEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteDevEndpointResponse' {} a -> s {httpStatus = a} :: DeleteDevEndpointResponse)

instance Prelude.NFData DeleteDevEndpointResponse
