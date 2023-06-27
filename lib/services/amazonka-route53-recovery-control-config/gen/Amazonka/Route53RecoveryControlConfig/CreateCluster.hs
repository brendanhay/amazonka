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
-- Module      : Amazonka.Route53RecoveryControlConfig.CreateCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new cluster. A cluster is a set of redundant Regional endpoints
-- against which you can run API calls to update or get the state of one or
-- more routing controls. Each cluster has a name, status, Amazon Resource
-- Name (ARN), and an array of the five cluster endpoints (one for each
-- supported Amazon Web Services Region) that you can use with API calls to
-- the cluster data plane.
module Amazonka.Route53RecoveryControlConfig.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_clientToken,
    createCluster_tags,
    createCluster_clusterName,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | Creates a cluster.
--
-- /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | A unique, case-sensitive string of up to 64 ASCII characters. To make an
    -- idempotent API request with an action, specify a client token in the
    -- request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the cluster.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createCluster_clientToken' - A unique, case-sensitive string of up to 64 ASCII characters. To make an
-- idempotent API request with an action, specify a client token in the
-- request.
--
-- 'tags', 'createCluster_tags' - The tags associated with the cluster.
--
-- 'clusterName', 'createCluster_clusterName' - The name of the cluster.
newCreateCluster ::
  -- | 'clusterName'
  Prelude.Text ->
  CreateCluster
newCreateCluster pClusterName_ =
  CreateCluster'
    { clientToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      clusterName = pClusterName_
    }

-- | A unique, case-sensitive string of up to 64 ASCII characters. To make an
-- idempotent API request with an action, specify a client token in the
-- request.
createCluster_clientToken :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_clientToken = Lens.lens (\CreateCluster' {clientToken} -> clientToken) (\s@CreateCluster' {} a -> s {clientToken = a} :: CreateCluster)

-- | The tags associated with the cluster.
createCluster_tags :: Lens.Lens' CreateCluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCluster_tags = Lens.lens (\CreateCluster' {tags} -> tags) (\s@CreateCluster' {} a -> s {tags = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cluster.
createCluster_clusterName :: Lens.Lens' CreateCluster Prelude.Text
createCluster_clusterName = Lens.lens (\CreateCluster' {clusterName} -> clusterName) (\s@CreateCluster' {} a -> s {clusterName = a} :: CreateCluster)

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Data..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster where
  hashWithSalt _salt CreateCluster' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData CreateCluster where
  rnf CreateCluster' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clusterName

instance Data.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ClusterName" Data..= clusterName)
          ]
      )

instance Data.ToPath CreateCluster where
  toPath = Prelude.const "/cluster"

instance Data.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The cluster that was created.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'createClusterResponse_cluster' - The cluster that was created.
--
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cluster that was created.
createClusterResponse_cluster :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Cluster)
createClusterResponse_cluster = Lens.lens (\CreateClusterResponse' {cluster} -> cluster) (\s@CreateClusterResponse' {} a -> s {cluster = a} :: CreateClusterResponse)

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse where
  rnf CreateClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
