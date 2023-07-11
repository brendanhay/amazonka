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
-- Module      : Amazonka.EMRContainers.DescribeManagedEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays detailed information about a managed endpoint. A managed
-- endpoint is a gateway that connects EMR Studio to Amazon EMR on EKS so
-- that EMR Studio can communicate with your virtual cluster.
module Amazonka.EMRContainers.DescribeManagedEndpoint
  ( -- * Creating a Request
    DescribeManagedEndpoint (..),
    newDescribeManagedEndpoint,

    -- * Request Lenses
    describeManagedEndpoint_id,
    describeManagedEndpoint_virtualClusterId,

    -- * Destructuring the Response
    DescribeManagedEndpointResponse (..),
    newDescribeManagedEndpointResponse,

    -- * Response Lenses
    describeManagedEndpointResponse_endpoint,
    describeManagedEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeManagedEndpoint' smart constructor.
data DescribeManagedEndpoint = DescribeManagedEndpoint'
  { -- | This output displays ID of the managed endpoint.
    id :: Prelude.Text,
    -- | The ID of the endpoint\'s virtual cluster.
    virtualClusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeManagedEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeManagedEndpoint_id' - This output displays ID of the managed endpoint.
--
-- 'virtualClusterId', 'describeManagedEndpoint_virtualClusterId' - The ID of the endpoint\'s virtual cluster.
newDescribeManagedEndpoint ::
  -- | 'id'
  Prelude.Text ->
  -- | 'virtualClusterId'
  Prelude.Text ->
  DescribeManagedEndpoint
newDescribeManagedEndpoint pId_ pVirtualClusterId_ =
  DescribeManagedEndpoint'
    { id = pId_,
      virtualClusterId = pVirtualClusterId_
    }

-- | This output displays ID of the managed endpoint.
describeManagedEndpoint_id :: Lens.Lens' DescribeManagedEndpoint Prelude.Text
describeManagedEndpoint_id = Lens.lens (\DescribeManagedEndpoint' {id} -> id) (\s@DescribeManagedEndpoint' {} a -> s {id = a} :: DescribeManagedEndpoint)

-- | The ID of the endpoint\'s virtual cluster.
describeManagedEndpoint_virtualClusterId :: Lens.Lens' DescribeManagedEndpoint Prelude.Text
describeManagedEndpoint_virtualClusterId = Lens.lens (\DescribeManagedEndpoint' {virtualClusterId} -> virtualClusterId) (\s@DescribeManagedEndpoint' {} a -> s {virtualClusterId = a} :: DescribeManagedEndpoint)

instance Core.AWSRequest DescribeManagedEndpoint where
  type
    AWSResponse DescribeManagedEndpoint =
      DescribeManagedEndpointResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeManagedEndpointResponse'
            Prelude.<$> (x Data..?> "endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeManagedEndpoint where
  hashWithSalt _salt DescribeManagedEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` virtualClusterId

instance Prelude.NFData DescribeManagedEndpoint where
  rnf DescribeManagedEndpoint' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf virtualClusterId

instance Data.ToHeaders DescribeManagedEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeManagedEndpoint where
  toPath DescribeManagedEndpoint' {..} =
    Prelude.mconcat
      [ "/virtualclusters/",
        Data.toBS virtualClusterId,
        "/endpoints/",
        Data.toBS id
      ]

instance Data.ToQuery DescribeManagedEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeManagedEndpointResponse' smart constructor.
data DescribeManagedEndpointResponse = DescribeManagedEndpointResponse'
  { -- | This output displays information about a managed endpoint.
    endpoint :: Prelude.Maybe Endpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeManagedEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'describeManagedEndpointResponse_endpoint' - This output displays information about a managed endpoint.
--
-- 'httpStatus', 'describeManagedEndpointResponse_httpStatus' - The response's http status code.
newDescribeManagedEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeManagedEndpointResponse
newDescribeManagedEndpointResponse pHttpStatus_ =
  DescribeManagedEndpointResponse'
    { endpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output displays information about a managed endpoint.
describeManagedEndpointResponse_endpoint :: Lens.Lens' DescribeManagedEndpointResponse (Prelude.Maybe Endpoint)
describeManagedEndpointResponse_endpoint = Lens.lens (\DescribeManagedEndpointResponse' {endpoint} -> endpoint) (\s@DescribeManagedEndpointResponse' {} a -> s {endpoint = a} :: DescribeManagedEndpointResponse)

-- | The response's http status code.
describeManagedEndpointResponse_httpStatus :: Lens.Lens' DescribeManagedEndpointResponse Prelude.Int
describeManagedEndpointResponse_httpStatus = Lens.lens (\DescribeManagedEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeManagedEndpointResponse' {} a -> s {httpStatus = a} :: DescribeManagedEndpointResponse)

instance
  Prelude.NFData
    DescribeManagedEndpointResponse
  where
  rnf DescribeManagedEndpointResponse' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf httpStatus
