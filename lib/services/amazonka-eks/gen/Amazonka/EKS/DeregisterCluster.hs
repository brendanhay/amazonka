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
-- Module      : Amazonka.EKS.DeregisterCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a connected cluster to remove it from the Amazon EKS control
-- plane.
module Amazonka.EKS.DeregisterCluster
  ( -- * Creating a Request
    DeregisterCluster (..),
    newDeregisterCluster,

    -- * Request Lenses
    deregisterCluster_name,

    -- * Destructuring the Response
    DeregisterClusterResponse (..),
    newDeregisterClusterResponse,

    -- * Response Lenses
    deregisterClusterResponse_cluster,
    deregisterClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterCluster' smart constructor.
data DeregisterCluster = DeregisterCluster'
  { -- | The name of the connected cluster to deregister.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deregisterCluster_name' - The name of the connected cluster to deregister.
newDeregisterCluster ::
  -- | 'name'
  Prelude.Text ->
  DeregisterCluster
newDeregisterCluster pName_ =
  DeregisterCluster' {name = pName_}

-- | The name of the connected cluster to deregister.
deregisterCluster_name :: Lens.Lens' DeregisterCluster Prelude.Text
deregisterCluster_name = Lens.lens (\DeregisterCluster' {name} -> name) (\s@DeregisterCluster' {} a -> s {name = a} :: DeregisterCluster)

instance Core.AWSRequest DeregisterCluster where
  type
    AWSResponse DeregisterCluster =
      DeregisterClusterResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterClusterResponse'
            Prelude.<$> (x Core..?> "cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterCluster where
  hashWithSalt _salt DeregisterCluster' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeregisterCluster where
  rnf DeregisterCluster' {..} = Prelude.rnf name

instance Core.ToHeaders DeregisterCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeregisterCluster where
  toPath DeregisterCluster' {..} =
    Prelude.mconcat
      ["/cluster-registrations/", Core.toBS name]

instance Core.ToQuery DeregisterCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterClusterResponse' smart constructor.
data DeregisterClusterResponse = DeregisterClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'deregisterClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'deregisterClusterResponse_httpStatus' - The response's http status code.
newDeregisterClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterClusterResponse
newDeregisterClusterResponse pHttpStatus_ =
  DeregisterClusterResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deregisterClusterResponse_cluster :: Lens.Lens' DeregisterClusterResponse (Prelude.Maybe Cluster)
deregisterClusterResponse_cluster = Lens.lens (\DeregisterClusterResponse' {cluster} -> cluster) (\s@DeregisterClusterResponse' {} a -> s {cluster = a} :: DeregisterClusterResponse)

-- | The response's http status code.
deregisterClusterResponse_httpStatus :: Lens.Lens' DeregisterClusterResponse Prelude.Int
deregisterClusterResponse_httpStatus = Lens.lens (\DeregisterClusterResponse' {httpStatus} -> httpStatus) (\s@DeregisterClusterResponse' {} a -> s {httpStatus = a} :: DeregisterClusterResponse)

instance Prelude.NFData DeregisterClusterResponse where
  rnf DeregisterClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
