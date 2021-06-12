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
-- Module      : Network.AWS.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotates the encryption keys for a cluster.
module Network.AWS.Redshift.RotateEncryptionKey
  ( -- * Creating a Request
    RotateEncryptionKey (..),
    newRotateEncryptionKey,

    -- * Request Lenses
    rotateEncryptionKey_clusterIdentifier,

    -- * Destructuring the Response
    RotateEncryptionKeyResponse (..),
    newRotateEncryptionKeyResponse,

    -- * Response Lenses
    rotateEncryptionKeyResponse_cluster,
    rotateEncryptionKeyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRotateEncryptionKey' smart constructor.
data RotateEncryptionKey = RotateEncryptionKey'
  { -- | The unique identifier of the cluster that you want to rotate the
    -- encryption keys for.
    --
    -- Constraints: Must be the name of valid cluster that has encryption
    -- enabled.
    clusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RotateEncryptionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'rotateEncryptionKey_clusterIdentifier' - The unique identifier of the cluster that you want to rotate the
-- encryption keys for.
--
-- Constraints: Must be the name of valid cluster that has encryption
-- enabled.
newRotateEncryptionKey ::
  -- | 'clusterIdentifier'
  Core.Text ->
  RotateEncryptionKey
newRotateEncryptionKey pClusterIdentifier_ =
  RotateEncryptionKey'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The unique identifier of the cluster that you want to rotate the
-- encryption keys for.
--
-- Constraints: Must be the name of valid cluster that has encryption
-- enabled.
rotateEncryptionKey_clusterIdentifier :: Lens.Lens' RotateEncryptionKey Core.Text
rotateEncryptionKey_clusterIdentifier = Lens.lens (\RotateEncryptionKey' {clusterIdentifier} -> clusterIdentifier) (\s@RotateEncryptionKey' {} a -> s {clusterIdentifier = a} :: RotateEncryptionKey)

instance Core.AWSRequest RotateEncryptionKey where
  type
    AWSResponse RotateEncryptionKey =
      RotateEncryptionKeyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RotateEncryptionKeyResult"
      ( \s h x ->
          RotateEncryptionKeyResponse'
            Core.<$> (x Core..@? "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RotateEncryptionKey

instance Core.NFData RotateEncryptionKey

instance Core.ToHeaders RotateEncryptionKey where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RotateEncryptionKey where
  toPath = Core.const "/"

instance Core.ToQuery RotateEncryptionKey where
  toQuery RotateEncryptionKey' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RotateEncryptionKey" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newRotateEncryptionKeyResponse' smart constructor.
data RotateEncryptionKeyResponse = RotateEncryptionKeyResponse'
  { cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RotateEncryptionKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'rotateEncryptionKeyResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'rotateEncryptionKeyResponse_httpStatus' - The response's http status code.
newRotateEncryptionKeyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RotateEncryptionKeyResponse
newRotateEncryptionKeyResponse pHttpStatus_ =
  RotateEncryptionKeyResponse'
    { cluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
rotateEncryptionKeyResponse_cluster :: Lens.Lens' RotateEncryptionKeyResponse (Core.Maybe Cluster)
rotateEncryptionKeyResponse_cluster = Lens.lens (\RotateEncryptionKeyResponse' {cluster} -> cluster) (\s@RotateEncryptionKeyResponse' {} a -> s {cluster = a} :: RotateEncryptionKeyResponse)

-- | The response's http status code.
rotateEncryptionKeyResponse_httpStatus :: Lens.Lens' RotateEncryptionKeyResponse Core.Int
rotateEncryptionKeyResponse_httpStatus = Lens.lens (\RotateEncryptionKeyResponse' {httpStatus} -> httpStatus) (\s@RotateEncryptionKeyResponse' {} a -> s {httpStatus = a} :: RotateEncryptionKeyResponse)

instance Core.NFData RotateEncryptionKeyResponse
