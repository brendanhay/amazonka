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
-- Module      : Amazonka.EMRContainers.DeleteManagedEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a managed endpoint. A managed endpoint is a gateway that
-- connects EMR Studio to Amazon EMR on EKS so that EMR Studio can
-- communicate with your virtual cluster.
module Amazonka.EMRContainers.DeleteManagedEndpoint
  ( -- * Creating a Request
    DeleteManagedEndpoint (..),
    newDeleteManagedEndpoint,

    -- * Request Lenses
    deleteManagedEndpoint_id,
    deleteManagedEndpoint_virtualClusterId,

    -- * Destructuring the Response
    DeleteManagedEndpointResponse (..),
    newDeleteManagedEndpointResponse,

    -- * Response Lenses
    deleteManagedEndpointResponse_id,
    deleteManagedEndpointResponse_virtualClusterId,
    deleteManagedEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteManagedEndpoint' smart constructor.
data DeleteManagedEndpoint = DeleteManagedEndpoint'
  { -- | The ID of the managed endpoint.
    id :: Prelude.Text,
    -- | The ID of the endpoint\'s virtual cluster.
    virtualClusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteManagedEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteManagedEndpoint_id' - The ID of the managed endpoint.
--
-- 'virtualClusterId', 'deleteManagedEndpoint_virtualClusterId' - The ID of the endpoint\'s virtual cluster.
newDeleteManagedEndpoint ::
  -- | 'id'
  Prelude.Text ->
  -- | 'virtualClusterId'
  Prelude.Text ->
  DeleteManagedEndpoint
newDeleteManagedEndpoint pId_ pVirtualClusterId_ =
  DeleteManagedEndpoint'
    { id = pId_,
      virtualClusterId = pVirtualClusterId_
    }

-- | The ID of the managed endpoint.
deleteManagedEndpoint_id :: Lens.Lens' DeleteManagedEndpoint Prelude.Text
deleteManagedEndpoint_id = Lens.lens (\DeleteManagedEndpoint' {id} -> id) (\s@DeleteManagedEndpoint' {} a -> s {id = a} :: DeleteManagedEndpoint)

-- | The ID of the endpoint\'s virtual cluster.
deleteManagedEndpoint_virtualClusterId :: Lens.Lens' DeleteManagedEndpoint Prelude.Text
deleteManagedEndpoint_virtualClusterId = Lens.lens (\DeleteManagedEndpoint' {virtualClusterId} -> virtualClusterId) (\s@DeleteManagedEndpoint' {} a -> s {virtualClusterId = a} :: DeleteManagedEndpoint)

instance Core.AWSRequest DeleteManagedEndpoint where
  type
    AWSResponse DeleteManagedEndpoint =
      DeleteManagedEndpointResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteManagedEndpointResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (x Data..?> "virtualClusterId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteManagedEndpoint where
  hashWithSalt _salt DeleteManagedEndpoint' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` virtualClusterId

instance Prelude.NFData DeleteManagedEndpoint where
  rnf DeleteManagedEndpoint' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf virtualClusterId

instance Data.ToHeaders DeleteManagedEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteManagedEndpoint where
  toPath DeleteManagedEndpoint' {..} =
    Prelude.mconcat
      [ "/virtualclusters/",
        Data.toBS virtualClusterId,
        "/endpoints/",
        Data.toBS id
      ]

instance Data.ToQuery DeleteManagedEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteManagedEndpointResponse' smart constructor.
data DeleteManagedEndpointResponse = DeleteManagedEndpointResponse'
  { -- | The output displays the ID of the managed endpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | The output displays the ID of the endpoint\'s virtual cluster.
    virtualClusterId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteManagedEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteManagedEndpointResponse_id' - The output displays the ID of the managed endpoint.
--
-- 'virtualClusterId', 'deleteManagedEndpointResponse_virtualClusterId' - The output displays the ID of the endpoint\'s virtual cluster.
--
-- 'httpStatus', 'deleteManagedEndpointResponse_httpStatus' - The response's http status code.
newDeleteManagedEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteManagedEndpointResponse
newDeleteManagedEndpointResponse pHttpStatus_ =
  DeleteManagedEndpointResponse'
    { id =
        Prelude.Nothing,
      virtualClusterId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The output displays the ID of the managed endpoint.
deleteManagedEndpointResponse_id :: Lens.Lens' DeleteManagedEndpointResponse (Prelude.Maybe Prelude.Text)
deleteManagedEndpointResponse_id = Lens.lens (\DeleteManagedEndpointResponse' {id} -> id) (\s@DeleteManagedEndpointResponse' {} a -> s {id = a} :: DeleteManagedEndpointResponse)

-- | The output displays the ID of the endpoint\'s virtual cluster.
deleteManagedEndpointResponse_virtualClusterId :: Lens.Lens' DeleteManagedEndpointResponse (Prelude.Maybe Prelude.Text)
deleteManagedEndpointResponse_virtualClusterId = Lens.lens (\DeleteManagedEndpointResponse' {virtualClusterId} -> virtualClusterId) (\s@DeleteManagedEndpointResponse' {} a -> s {virtualClusterId = a} :: DeleteManagedEndpointResponse)

-- | The response's http status code.
deleteManagedEndpointResponse_httpStatus :: Lens.Lens' DeleteManagedEndpointResponse Prelude.Int
deleteManagedEndpointResponse_httpStatus = Lens.lens (\DeleteManagedEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteManagedEndpointResponse' {} a -> s {httpStatus = a} :: DeleteManagedEndpointResponse)

instance Prelude.NFData DeleteManagedEndpointResponse where
  rnf DeleteManagedEndpointResponse' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf virtualClusterId
      `Prelude.seq` Prelude.rnf httpStatus
