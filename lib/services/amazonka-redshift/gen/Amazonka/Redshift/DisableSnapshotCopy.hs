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
-- Module      : Amazonka.Redshift.DisableSnapshotCopy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the automatic copying of snapshots from one region to another
-- region for a specified cluster.
--
-- If your cluster and its snapshots are encrypted using an encrypted
-- symmetric key from Key Management Service, use DeleteSnapshotCopyGrant
-- to delete the grant that grants Amazon Redshift permission to the key in
-- the destination region.
module Amazonka.Redshift.DisableSnapshotCopy
  ( -- * Creating a Request
    DisableSnapshotCopy (..),
    newDisableSnapshotCopy,

    -- * Request Lenses
    disableSnapshotCopy_clusterIdentifier,

    -- * Destructuring the Response
    DisableSnapshotCopyResponse (..),
    newDisableSnapshotCopyResponse,

    -- * Response Lenses
    disableSnapshotCopyResponse_cluster,
    disableSnapshotCopyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDisableSnapshotCopy' smart constructor.
data DisableSnapshotCopy = DisableSnapshotCopy'
  { -- | The unique identifier of the source cluster that you want to disable
    -- copying of snapshots to a destination region.
    --
    -- Constraints: Must be the valid name of an existing cluster that has
    -- cross-region snapshot copy enabled.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableSnapshotCopy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'disableSnapshotCopy_clusterIdentifier' - The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
newDisableSnapshotCopy ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  DisableSnapshotCopy
newDisableSnapshotCopy pClusterIdentifier_ =
  DisableSnapshotCopy'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
disableSnapshotCopy_clusterIdentifier :: Lens.Lens' DisableSnapshotCopy Prelude.Text
disableSnapshotCopy_clusterIdentifier = Lens.lens (\DisableSnapshotCopy' {clusterIdentifier} -> clusterIdentifier) (\s@DisableSnapshotCopy' {} a -> s {clusterIdentifier = a} :: DisableSnapshotCopy)

instance Core.AWSRequest DisableSnapshotCopy where
  type
    AWSResponse DisableSnapshotCopy =
      DisableSnapshotCopyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DisableSnapshotCopyResult"
      ( \s h x ->
          DisableSnapshotCopyResponse'
            Prelude.<$> (x Data..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableSnapshotCopy where
  hashWithSalt _salt DisableSnapshotCopy' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData DisableSnapshotCopy where
  rnf DisableSnapshotCopy' {..} =
    Prelude.rnf clusterIdentifier

instance Data.ToHeaders DisableSnapshotCopy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableSnapshotCopy where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableSnapshotCopy where
  toQuery DisableSnapshotCopy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DisableSnapshotCopy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newDisableSnapshotCopyResponse' smart constructor.
data DisableSnapshotCopyResponse = DisableSnapshotCopyResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableSnapshotCopyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'disableSnapshotCopyResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'disableSnapshotCopyResponse_httpStatus' - The response's http status code.
newDisableSnapshotCopyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableSnapshotCopyResponse
newDisableSnapshotCopyResponse pHttpStatus_ =
  DisableSnapshotCopyResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
disableSnapshotCopyResponse_cluster :: Lens.Lens' DisableSnapshotCopyResponse (Prelude.Maybe Cluster)
disableSnapshotCopyResponse_cluster = Lens.lens (\DisableSnapshotCopyResponse' {cluster} -> cluster) (\s@DisableSnapshotCopyResponse' {} a -> s {cluster = a} :: DisableSnapshotCopyResponse)

-- | The response's http status code.
disableSnapshotCopyResponse_httpStatus :: Lens.Lens' DisableSnapshotCopyResponse Prelude.Int
disableSnapshotCopyResponse_httpStatus = Lens.lens (\DisableSnapshotCopyResponse' {httpStatus} -> httpStatus) (\s@DisableSnapshotCopyResponse' {} a -> s {httpStatus = a} :: DisableSnapshotCopyResponse)

instance Prelude.NFData DisableSnapshotCopyResponse where
  rnf DisableSnapshotCopyResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
