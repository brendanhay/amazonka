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
-- Module      : Network.AWS.Snowball.CancelCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a cluster job. You can only cancel a cluster job while it\'s in
-- the @AwaitingQuorum@ status. You\'ll have at least an hour after
-- creating a cluster job to cancel it.
module Network.AWS.Snowball.CancelCluster
  ( -- * Creating a Request
    CancelCluster (..),
    newCancelCluster,

    -- * Request Lenses
    cancelCluster_clusterId,

    -- * Destructuring the Response
    CancelClusterResponse (..),
    newCancelClusterResponse,

    -- * Response Lenses
    cancelClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newCancelCluster' smart constructor.
data CancelCluster = CancelCluster'
  { -- | The 39-character ID for the cluster that you want to cancel, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'cancelCluster_clusterId' - The 39-character ID for the cluster that you want to cancel, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
newCancelCluster ::
  -- | 'clusterId'
  Prelude.Text ->
  CancelCluster
newCancelCluster pClusterId_ =
  CancelCluster' {clusterId = pClusterId_}

-- | The 39-character ID for the cluster that you want to cancel, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
cancelCluster_clusterId :: Lens.Lens' CancelCluster Prelude.Text
cancelCluster_clusterId = Lens.lens (\CancelCluster' {clusterId} -> clusterId) (\s@CancelCluster' {} a -> s {clusterId = a} :: CancelCluster)

instance Prelude.AWSRequest CancelCluster where
  type Rs CancelCluster = CancelClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelCluster

instance Prelude.NFData CancelCluster

instance Prelude.ToHeaders CancelCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.CancelCluster" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CancelCluster where
  toJSON CancelCluster' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Prelude..= clusterId)]
      )

instance Prelude.ToPath CancelCluster where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelClusterResponse' smart constructor.
data CancelClusterResponse = CancelClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelClusterResponse_httpStatus' - The response's http status code.
newCancelClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelClusterResponse
newCancelClusterResponse pHttpStatus_ =
  CancelClusterResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
cancelClusterResponse_httpStatus :: Lens.Lens' CancelClusterResponse Prelude.Int
cancelClusterResponse_httpStatus = Lens.lens (\CancelClusterResponse' {httpStatus} -> httpStatus) (\s@CancelClusterResponse' {} a -> s {httpStatus = a} :: CancelClusterResponse)

instance Prelude.NFData CancelClusterResponse
