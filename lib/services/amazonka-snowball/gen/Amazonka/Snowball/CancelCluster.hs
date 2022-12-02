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
-- Module      : Amazonka.Snowball.CancelCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a cluster job. You can only cancel a cluster job while it\'s in
-- the @AwaitingQuorum@ status. You\'ll have at least an hour after
-- creating a cluster job to cancel it.
module Amazonka.Snowball.CancelCluster
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newCancelCluster' smart constructor.
data CancelCluster = CancelCluster'
  { -- | The 39-character ID for the cluster that you want to cancel, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest CancelCluster where
  type
    AWSResponse CancelCluster =
      CancelClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelCluster where
  hashWithSalt _salt CancelCluster' {..} =
    _salt `Prelude.hashWithSalt` clusterId

instance Prelude.NFData CancelCluster where
  rnf CancelCluster' {..} = Prelude.rnf clusterId

instance Data.ToHeaders CancelCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.CancelCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelCluster where
  toJSON CancelCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Data..= clusterId)]
      )

instance Data.ToPath CancelCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelClusterResponse' smart constructor.
data CancelClusterResponse = CancelClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CancelClusterResponse where
  rnf CancelClusterResponse' {..} =
    Prelude.rnf httpStatus
