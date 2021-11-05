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
-- Module      : Network.AWS.Kafka.DeleteCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the MSK cluster specified by the Amazon Resource Name (ARN) in
-- the request.
module Network.AWS.Kafka.DeleteCluster
  ( -- * Creating a Request
    DeleteCluster (..),
    newDeleteCluster,

    -- * Request Lenses
    deleteCluster_currentVersion,
    deleteCluster_clusterArn,

    -- * Destructuring the Response
    DeleteClusterResponse (..),
    newDeleteClusterResponse,

    -- * Response Lenses
    deleteClusterResponse_state,
    deleteClusterResponse_clusterArn,
    deleteClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kafka.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { -- | The current version of the MSK cluster.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'deleteCluster_currentVersion' - The current version of the MSK cluster.
--
-- 'clusterArn', 'deleteCluster_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
newDeleteCluster ::
  -- | 'clusterArn'
  Prelude.Text ->
  DeleteCluster
newDeleteCluster pClusterArn_ =
  DeleteCluster'
    { currentVersion = Prelude.Nothing,
      clusterArn = pClusterArn_
    }

-- | The current version of the MSK cluster.
deleteCluster_currentVersion :: Lens.Lens' DeleteCluster (Prelude.Maybe Prelude.Text)
deleteCluster_currentVersion = Lens.lens (\DeleteCluster' {currentVersion} -> currentVersion) (\s@DeleteCluster' {} a -> s {currentVersion = a} :: DeleteCluster)

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
deleteCluster_clusterArn :: Lens.Lens' DeleteCluster Prelude.Text
deleteCluster_clusterArn = Lens.lens (\DeleteCluster' {clusterArn} -> clusterArn) (\s@DeleteCluster' {} a -> s {clusterArn = a} :: DeleteCluster)

instance Core.AWSRequest DeleteCluster where
  type
    AWSResponse DeleteCluster =
      DeleteClusterResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteClusterResponse'
            Prelude.<$> (x Core..?> "state")
            Prelude.<*> (x Core..?> "clusterArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCluster

instance Prelude.NFData DeleteCluster

instance Core.ToHeaders DeleteCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteCluster where
  toPath DeleteCluster' {..} =
    Prelude.mconcat
      ["/v1/clusters/", Core.toBS clusterArn]

instance Core.ToQuery DeleteCluster where
  toQuery DeleteCluster' {..} =
    Prelude.mconcat
      ["currentVersion" Core.=: currentVersion]

-- | /See:/ 'newDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { -- | The state of the cluster. The possible states are ACTIVE, CREATING,
    -- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
    state :: Prelude.Maybe ClusterState,
    -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'deleteClusterResponse_state' - The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
--
-- 'clusterArn', 'deleteClusterResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'httpStatus', 'deleteClusterResponse_httpStatus' - The response's http status code.
newDeleteClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteClusterResponse
newDeleteClusterResponse pHttpStatus_ =
  DeleteClusterResponse'
    { state = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
deleteClusterResponse_state :: Lens.Lens' DeleteClusterResponse (Prelude.Maybe ClusterState)
deleteClusterResponse_state = Lens.lens (\DeleteClusterResponse' {state} -> state) (\s@DeleteClusterResponse' {} a -> s {state = a} :: DeleteClusterResponse)

-- | The Amazon Resource Name (ARN) of the cluster.
deleteClusterResponse_clusterArn :: Lens.Lens' DeleteClusterResponse (Prelude.Maybe Prelude.Text)
deleteClusterResponse_clusterArn = Lens.lens (\DeleteClusterResponse' {clusterArn} -> clusterArn) (\s@DeleteClusterResponse' {} a -> s {clusterArn = a} :: DeleteClusterResponse)

-- | The response's http status code.
deleteClusterResponse_httpStatus :: Lens.Lens' DeleteClusterResponse Prelude.Int
deleteClusterResponse_httpStatus = Lens.lens (\DeleteClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterResponse' {} a -> s {httpStatus = a} :: DeleteClusterResponse)

instance Prelude.NFData DeleteClusterResponse
