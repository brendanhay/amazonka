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
-- Module      : Amazonka.EMRContainers.CreateVirtualCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual cluster. Virtual cluster is a managed entity on Amazon
-- EMR on EKS. You can create, describe, list and delete virtual clusters.
-- They do not consume any additional resource in your system. A single
-- virtual cluster maps to a single Kubernetes namespace. Given this
-- relationship, you can model virtual clusters the same way you model
-- Kubernetes namespaces to meet your requirements.
module Amazonka.EMRContainers.CreateVirtualCluster
  ( -- * Creating a Request
    CreateVirtualCluster (..),
    newCreateVirtualCluster,

    -- * Request Lenses
    createVirtualCluster_tags,
    createVirtualCluster_name,
    createVirtualCluster_containerProvider,
    createVirtualCluster_clientToken,

    -- * Destructuring the Response
    CreateVirtualClusterResponse (..),
    newCreateVirtualClusterResponse,

    -- * Response Lenses
    createVirtualClusterResponse_arn,
    createVirtualClusterResponse_id,
    createVirtualClusterResponse_name,
    createVirtualClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVirtualCluster' smart constructor.
data CreateVirtualCluster = CreateVirtualCluster'
  { -- | The tags assigned to the virtual cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The specified name of the virtual cluster.
    name :: Prelude.Text,
    -- | The container provider of the virtual cluster.
    containerProvider :: ContainerProvider,
    -- | The client token of the virtual cluster.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVirtualCluster_tags' - The tags assigned to the virtual cluster.
--
-- 'name', 'createVirtualCluster_name' - The specified name of the virtual cluster.
--
-- 'containerProvider', 'createVirtualCluster_containerProvider' - The container provider of the virtual cluster.
--
-- 'clientToken', 'createVirtualCluster_clientToken' - The client token of the virtual cluster.
newCreateVirtualCluster ::
  -- | 'name'
  Prelude.Text ->
  -- | 'containerProvider'
  ContainerProvider ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateVirtualCluster
newCreateVirtualCluster
  pName_
  pContainerProvider_
  pClientToken_ =
    CreateVirtualCluster'
      { tags = Prelude.Nothing,
        name = pName_,
        containerProvider = pContainerProvider_,
        clientToken = pClientToken_
      }

-- | The tags assigned to the virtual cluster.
createVirtualCluster_tags :: Lens.Lens' CreateVirtualCluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVirtualCluster_tags = Lens.lens (\CreateVirtualCluster' {tags} -> tags) (\s@CreateVirtualCluster' {} a -> s {tags = a} :: CreateVirtualCluster) Prelude.. Lens.mapping Lens.coerced

-- | The specified name of the virtual cluster.
createVirtualCluster_name :: Lens.Lens' CreateVirtualCluster Prelude.Text
createVirtualCluster_name = Lens.lens (\CreateVirtualCluster' {name} -> name) (\s@CreateVirtualCluster' {} a -> s {name = a} :: CreateVirtualCluster)

-- | The container provider of the virtual cluster.
createVirtualCluster_containerProvider :: Lens.Lens' CreateVirtualCluster ContainerProvider
createVirtualCluster_containerProvider = Lens.lens (\CreateVirtualCluster' {containerProvider} -> containerProvider) (\s@CreateVirtualCluster' {} a -> s {containerProvider = a} :: CreateVirtualCluster)

-- | The client token of the virtual cluster.
createVirtualCluster_clientToken :: Lens.Lens' CreateVirtualCluster Prelude.Text
createVirtualCluster_clientToken = Lens.lens (\CreateVirtualCluster' {clientToken} -> clientToken) (\s@CreateVirtualCluster' {} a -> s {clientToken = a} :: CreateVirtualCluster)

instance Core.AWSRequest CreateVirtualCluster where
  type
    AWSResponse CreateVirtualCluster =
      CreateVirtualClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVirtualClusterResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVirtualCluster where
  hashWithSalt _salt CreateVirtualCluster' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` containerProvider
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateVirtualCluster where
  rnf CreateVirtualCluster' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf containerProvider `Prelude.seq`
          Prelude.rnf clientToken

instance Data.ToHeaders CreateVirtualCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVirtualCluster where
  toJSON CreateVirtualCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("containerProvider" Data..= containerProvider),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateVirtualCluster where
  toPath = Prelude.const "/virtualclusters"

instance Data.ToQuery CreateVirtualCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVirtualClusterResponse' smart constructor.
data CreateVirtualClusterResponse = CreateVirtualClusterResponse'
  { -- | This output contains the ARN of virtual cluster.
    arn :: Prelude.Maybe Prelude.Text,
    -- | This output contains the virtual cluster ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | This output contains the name of the virtual cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createVirtualClusterResponse_arn' - This output contains the ARN of virtual cluster.
--
-- 'id', 'createVirtualClusterResponse_id' - This output contains the virtual cluster ID.
--
-- 'name', 'createVirtualClusterResponse_name' - This output contains the name of the virtual cluster.
--
-- 'httpStatus', 'createVirtualClusterResponse_httpStatus' - The response's http status code.
newCreateVirtualClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVirtualClusterResponse
newCreateVirtualClusterResponse pHttpStatus_ =
  CreateVirtualClusterResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output contains the ARN of virtual cluster.
createVirtualClusterResponse_arn :: Lens.Lens' CreateVirtualClusterResponse (Prelude.Maybe Prelude.Text)
createVirtualClusterResponse_arn = Lens.lens (\CreateVirtualClusterResponse' {arn} -> arn) (\s@CreateVirtualClusterResponse' {} a -> s {arn = a} :: CreateVirtualClusterResponse)

-- | This output contains the virtual cluster ID.
createVirtualClusterResponse_id :: Lens.Lens' CreateVirtualClusterResponse (Prelude.Maybe Prelude.Text)
createVirtualClusterResponse_id = Lens.lens (\CreateVirtualClusterResponse' {id} -> id) (\s@CreateVirtualClusterResponse' {} a -> s {id = a} :: CreateVirtualClusterResponse)

-- | This output contains the name of the virtual cluster.
createVirtualClusterResponse_name :: Lens.Lens' CreateVirtualClusterResponse (Prelude.Maybe Prelude.Text)
createVirtualClusterResponse_name = Lens.lens (\CreateVirtualClusterResponse' {name} -> name) (\s@CreateVirtualClusterResponse' {} a -> s {name = a} :: CreateVirtualClusterResponse)

-- | The response's http status code.
createVirtualClusterResponse_httpStatus :: Lens.Lens' CreateVirtualClusterResponse Prelude.Int
createVirtualClusterResponse_httpStatus = Lens.lens (\CreateVirtualClusterResponse' {httpStatus} -> httpStatus) (\s@CreateVirtualClusterResponse' {} a -> s {httpStatus = a} :: CreateVirtualClusterResponse)

instance Prelude.NFData CreateVirtualClusterResponse where
  rnf CreateVirtualClusterResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf httpStatus
