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
-- Module      : Amazonka.DrS.StopReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replication for a Source Server. This action would make the Source
-- Server unprotected, delete its existing snapshots and stop billing for
-- it.
module Amazonka.DrS.StopReplication
  ( -- * Creating a Request
    StopReplication (..),
    newStopReplication,

    -- * Request Lenses
    stopReplication_sourceServerID,

    -- * Destructuring the Response
    StopReplicationResponse (..),
    newStopReplicationResponse,

    -- * Response Lenses
    stopReplicationResponse_sourceServer,
    stopReplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopReplication' smart constructor.
data StopReplication = StopReplication'
  { -- | The ID of the Source Server to stop replication for.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServerID', 'stopReplication_sourceServerID' - The ID of the Source Server to stop replication for.
newStopReplication ::
  -- | 'sourceServerID'
  Prelude.Text ->
  StopReplication
newStopReplication pSourceServerID_ =
  StopReplication' {sourceServerID = pSourceServerID_}

-- | The ID of the Source Server to stop replication for.
stopReplication_sourceServerID :: Lens.Lens' StopReplication Prelude.Text
stopReplication_sourceServerID = Lens.lens (\StopReplication' {sourceServerID} -> sourceServerID) (\s@StopReplication' {} a -> s {sourceServerID = a} :: StopReplication)

instance Core.AWSRequest StopReplication where
  type
    AWSResponse StopReplication =
      StopReplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopReplicationResponse'
            Prelude.<$> (x Data..?> "sourceServer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopReplication where
  hashWithSalt _salt StopReplication' {..} =
    _salt `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData StopReplication where
  rnf StopReplication' {..} = Prelude.rnf sourceServerID

instance Data.ToHeaders StopReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopReplication where
  toJSON StopReplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath StopReplication where
  toPath = Prelude.const "/StopReplication"

instance Data.ToQuery StopReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopReplicationResponse' smart constructor.
data StopReplicationResponse = StopReplicationResponse'
  { -- | The Source Server that this action was targeted on.
    sourceServer :: Prelude.Maybe SourceServer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServer', 'stopReplicationResponse_sourceServer' - The Source Server that this action was targeted on.
--
-- 'httpStatus', 'stopReplicationResponse_httpStatus' - The response's http status code.
newStopReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopReplicationResponse
newStopReplicationResponse pHttpStatus_ =
  StopReplicationResponse'
    { sourceServer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Source Server that this action was targeted on.
stopReplicationResponse_sourceServer :: Lens.Lens' StopReplicationResponse (Prelude.Maybe SourceServer)
stopReplicationResponse_sourceServer = Lens.lens (\StopReplicationResponse' {sourceServer} -> sourceServer) (\s@StopReplicationResponse' {} a -> s {sourceServer = a} :: StopReplicationResponse)

-- | The response's http status code.
stopReplicationResponse_httpStatus :: Lens.Lens' StopReplicationResponse Prelude.Int
stopReplicationResponse_httpStatus = Lens.lens (\StopReplicationResponse' {httpStatus} -> httpStatus) (\s@StopReplicationResponse' {} a -> s {httpStatus = a} :: StopReplicationResponse)

instance Prelude.NFData StopReplicationResponse where
  rnf StopReplicationResponse' {..} =
    Prelude.rnf sourceServer `Prelude.seq`
      Prelude.rnf httpStatus
