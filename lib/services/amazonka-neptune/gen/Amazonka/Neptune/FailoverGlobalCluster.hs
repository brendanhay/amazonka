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
-- Module      : Amazonka.Neptune.FailoverGlobalCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the failover process for a Neptune global database.
--
-- A failover for a Neptune global database promotes one of secondary
-- read-only DB clusters to be the primary DB cluster and demotes the
-- primary DB cluster to being a secondary (read-only) DB cluster. In other
-- words, the role of the current primary DB cluster and the selected
-- target secondary DB cluster are switched. The selected secondary DB
-- cluster assumes full read\/write capabilities for the Neptune global
-- database.
--
-- This action applies __only__ to Neptune global databases. This action is
-- only intended for use on healthy Neptune global databases with healthy
-- Neptune DB clusters and no region-wide outages, to test disaster
-- recovery scenarios or to reconfigure the global database topology.
module Amazonka.Neptune.FailoverGlobalCluster
  ( -- * Creating a Request
    FailoverGlobalCluster (..),
    newFailoverGlobalCluster,

    -- * Request Lenses
    failoverGlobalCluster_globalClusterIdentifier,
    failoverGlobalCluster_targetDbClusterIdentifier,

    -- * Destructuring the Response
    FailoverGlobalClusterResponse (..),
    newFailoverGlobalClusterResponse,

    -- * Response Lenses
    failoverGlobalClusterResponse_globalCluster,
    failoverGlobalClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newFailoverGlobalCluster' smart constructor.
data FailoverGlobalCluster = FailoverGlobalCluster'
  { -- | Identifier of the Neptune global database that should be failed over.
    -- The identifier is the unique key assigned by the user when the Neptune
    -- global database was created. In other words, it\'s the name of the
    -- global database that you want to fail over.
    --
    -- Constraints: Must match the identifier of an existing Neptune global
    -- database.
    globalClusterIdentifier :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the secondary Neptune DB cluster that
    -- you want to promote to primary for the global database.
    targetDbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverGlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalClusterIdentifier', 'failoverGlobalCluster_globalClusterIdentifier' - Identifier of the Neptune global database that should be failed over.
-- The identifier is the unique key assigned by the user when the Neptune
-- global database was created. In other words, it\'s the name of the
-- global database that you want to fail over.
--
-- Constraints: Must match the identifier of an existing Neptune global
-- database.
--
-- 'targetDbClusterIdentifier', 'failoverGlobalCluster_targetDbClusterIdentifier' - The Amazon Resource Name (ARN) of the secondary Neptune DB cluster that
-- you want to promote to primary for the global database.
newFailoverGlobalCluster ::
  -- | 'globalClusterIdentifier'
  Prelude.Text ->
  -- | 'targetDbClusterIdentifier'
  Prelude.Text ->
  FailoverGlobalCluster
newFailoverGlobalCluster
  pGlobalClusterIdentifier_
  pTargetDbClusterIdentifier_ =
    FailoverGlobalCluster'
      { globalClusterIdentifier =
          pGlobalClusterIdentifier_,
        targetDbClusterIdentifier =
          pTargetDbClusterIdentifier_
      }

-- | Identifier of the Neptune global database that should be failed over.
-- The identifier is the unique key assigned by the user when the Neptune
-- global database was created. In other words, it\'s the name of the
-- global database that you want to fail over.
--
-- Constraints: Must match the identifier of an existing Neptune global
-- database.
failoverGlobalCluster_globalClusterIdentifier :: Lens.Lens' FailoverGlobalCluster Prelude.Text
failoverGlobalCluster_globalClusterIdentifier = Lens.lens (\FailoverGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@FailoverGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: FailoverGlobalCluster)

-- | The Amazon Resource Name (ARN) of the secondary Neptune DB cluster that
-- you want to promote to primary for the global database.
failoverGlobalCluster_targetDbClusterIdentifier :: Lens.Lens' FailoverGlobalCluster Prelude.Text
failoverGlobalCluster_targetDbClusterIdentifier = Lens.lens (\FailoverGlobalCluster' {targetDbClusterIdentifier} -> targetDbClusterIdentifier) (\s@FailoverGlobalCluster' {} a -> s {targetDbClusterIdentifier = a} :: FailoverGlobalCluster)

instance Core.AWSRequest FailoverGlobalCluster where
  type
    AWSResponse FailoverGlobalCluster =
      FailoverGlobalClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "FailoverGlobalClusterResult"
      ( \s h x ->
          FailoverGlobalClusterResponse'
            Prelude.<$> (x Data..@? "GlobalCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable FailoverGlobalCluster where
  hashWithSalt _salt FailoverGlobalCluster' {..} =
    _salt
      `Prelude.hashWithSalt` globalClusterIdentifier
      `Prelude.hashWithSalt` targetDbClusterIdentifier

instance Prelude.NFData FailoverGlobalCluster where
  rnf FailoverGlobalCluster' {..} =
    Prelude.rnf globalClusterIdentifier `Prelude.seq`
      Prelude.rnf targetDbClusterIdentifier

instance Data.ToHeaders FailoverGlobalCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath FailoverGlobalCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery FailoverGlobalCluster where
  toQuery FailoverGlobalCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("FailoverGlobalCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier,
        "TargetDbClusterIdentifier"
          Data.=: targetDbClusterIdentifier
      ]

-- | /See:/ 'newFailoverGlobalClusterResponse' smart constructor.
data FailoverGlobalClusterResponse = FailoverGlobalClusterResponse'
  { globalCluster :: Prelude.Maybe GlobalCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverGlobalClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalCluster', 'failoverGlobalClusterResponse_globalCluster' - Undocumented member.
--
-- 'httpStatus', 'failoverGlobalClusterResponse_httpStatus' - The response's http status code.
newFailoverGlobalClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  FailoverGlobalClusterResponse
newFailoverGlobalClusterResponse pHttpStatus_ =
  FailoverGlobalClusterResponse'
    { globalCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
failoverGlobalClusterResponse_globalCluster :: Lens.Lens' FailoverGlobalClusterResponse (Prelude.Maybe GlobalCluster)
failoverGlobalClusterResponse_globalCluster = Lens.lens (\FailoverGlobalClusterResponse' {globalCluster} -> globalCluster) (\s@FailoverGlobalClusterResponse' {} a -> s {globalCluster = a} :: FailoverGlobalClusterResponse)

-- | The response's http status code.
failoverGlobalClusterResponse_httpStatus :: Lens.Lens' FailoverGlobalClusterResponse Prelude.Int
failoverGlobalClusterResponse_httpStatus = Lens.lens (\FailoverGlobalClusterResponse' {httpStatus} -> httpStatus) (\s@FailoverGlobalClusterResponse' {} a -> s {httpStatus = a} :: FailoverGlobalClusterResponse)

instance Prelude.NFData FailoverGlobalClusterResponse where
  rnf FailoverGlobalClusterResponse' {..} =
    Prelude.rnf globalCluster `Prelude.seq`
      Prelude.rnf httpStatus
