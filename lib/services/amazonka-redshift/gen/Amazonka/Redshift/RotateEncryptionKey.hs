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
-- Module      : Amazonka.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotates the encryption keys for a cluster.
module Amazonka.Redshift.RotateEncryptionKey
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRotateEncryptionKey' smart constructor.
data RotateEncryptionKey = RotateEncryptionKey'
  { -- | The unique identifier of the cluster that you want to rotate the
    -- encryption keys for.
    --
    -- Constraints: Must be the name of valid cluster that has encryption
    -- enabled.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
rotateEncryptionKey_clusterIdentifier :: Lens.Lens' RotateEncryptionKey Prelude.Text
rotateEncryptionKey_clusterIdentifier = Lens.lens (\RotateEncryptionKey' {clusterIdentifier} -> clusterIdentifier) (\s@RotateEncryptionKey' {} a -> s {clusterIdentifier = a} :: RotateEncryptionKey)

instance Core.AWSRequest RotateEncryptionKey where
  type
    AWSResponse RotateEncryptionKey =
      RotateEncryptionKeyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RotateEncryptionKeyResult"
      ( \s h x ->
          RotateEncryptionKeyResponse'
            Prelude.<$> (x Core..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RotateEncryptionKey where
  hashWithSalt _salt RotateEncryptionKey' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData RotateEncryptionKey where
  rnf RotateEncryptionKey' {..} =
    Prelude.rnf clusterIdentifier

instance Core.ToHeaders RotateEncryptionKey where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RotateEncryptionKey where
  toPath = Prelude.const "/"

instance Core.ToQuery RotateEncryptionKey where
  toQuery RotateEncryptionKey' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RotateEncryptionKey" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newRotateEncryptionKeyResponse' smart constructor.
data RotateEncryptionKeyResponse = RotateEncryptionKeyResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RotateEncryptionKeyResponse
newRotateEncryptionKeyResponse pHttpStatus_ =
  RotateEncryptionKeyResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
rotateEncryptionKeyResponse_cluster :: Lens.Lens' RotateEncryptionKeyResponse (Prelude.Maybe Cluster)
rotateEncryptionKeyResponse_cluster = Lens.lens (\RotateEncryptionKeyResponse' {cluster} -> cluster) (\s@RotateEncryptionKeyResponse' {} a -> s {cluster = a} :: RotateEncryptionKeyResponse)

-- | The response's http status code.
rotateEncryptionKeyResponse_httpStatus :: Lens.Lens' RotateEncryptionKeyResponse Prelude.Int
rotateEncryptionKeyResponse_httpStatus = Lens.lens (\RotateEncryptionKeyResponse' {httpStatus} -> httpStatus) (\s@RotateEncryptionKeyResponse' {} a -> s {httpStatus = a} :: RotateEncryptionKeyResponse)

instance Prelude.NFData RotateEncryptionKeyResponse where
  rnf RotateEncryptionKeyResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
