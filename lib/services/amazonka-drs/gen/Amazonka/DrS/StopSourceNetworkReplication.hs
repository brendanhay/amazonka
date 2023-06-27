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
-- Module      : Amazonka.DrS.StopSourceNetworkReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replication for a Source Network. This action would make the
-- Source Network unprotected.
module Amazonka.DrS.StopSourceNetworkReplication
  ( -- * Creating a Request
    StopSourceNetworkReplication (..),
    newStopSourceNetworkReplication,

    -- * Request Lenses
    stopSourceNetworkReplication_sourceNetworkID,

    -- * Destructuring the Response
    StopSourceNetworkReplicationResponse (..),
    newStopSourceNetworkReplicationResponse,

    -- * Response Lenses
    stopSourceNetworkReplicationResponse_sourceNetwork,
    stopSourceNetworkReplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopSourceNetworkReplication' smart constructor.
data StopSourceNetworkReplication = StopSourceNetworkReplication'
  { -- | ID of the Source Network to stop replication.
    sourceNetworkID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSourceNetworkReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetworkID', 'stopSourceNetworkReplication_sourceNetworkID' - ID of the Source Network to stop replication.
newStopSourceNetworkReplication ::
  -- | 'sourceNetworkID'
  Prelude.Text ->
  StopSourceNetworkReplication
newStopSourceNetworkReplication pSourceNetworkID_ =
  StopSourceNetworkReplication'
    { sourceNetworkID =
        pSourceNetworkID_
    }

-- | ID of the Source Network to stop replication.
stopSourceNetworkReplication_sourceNetworkID :: Lens.Lens' StopSourceNetworkReplication Prelude.Text
stopSourceNetworkReplication_sourceNetworkID = Lens.lens (\StopSourceNetworkReplication' {sourceNetworkID} -> sourceNetworkID) (\s@StopSourceNetworkReplication' {} a -> s {sourceNetworkID = a} :: StopSourceNetworkReplication)

instance Core.AWSRequest StopSourceNetworkReplication where
  type
    AWSResponse StopSourceNetworkReplication =
      StopSourceNetworkReplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopSourceNetworkReplicationResponse'
            Prelude.<$> (x Data..?> "sourceNetwork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopSourceNetworkReplication
  where
  hashWithSalt _salt StopSourceNetworkReplication' {..} =
    _salt `Prelude.hashWithSalt` sourceNetworkID

instance Prelude.NFData StopSourceNetworkReplication where
  rnf StopSourceNetworkReplication' {..} =
    Prelude.rnf sourceNetworkID

instance Data.ToHeaders StopSourceNetworkReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopSourceNetworkReplication where
  toJSON StopSourceNetworkReplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceNetworkID" Data..= sourceNetworkID)
          ]
      )

instance Data.ToPath StopSourceNetworkReplication where
  toPath =
    Prelude.const "/StopSourceNetworkReplication"

instance Data.ToQuery StopSourceNetworkReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopSourceNetworkReplicationResponse' smart constructor.
data StopSourceNetworkReplicationResponse = StopSourceNetworkReplicationResponse'
  { -- | Source Network which was requested to stop replication.
    sourceNetwork :: Prelude.Maybe SourceNetwork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSourceNetworkReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetwork', 'stopSourceNetworkReplicationResponse_sourceNetwork' - Source Network which was requested to stop replication.
--
-- 'httpStatus', 'stopSourceNetworkReplicationResponse_httpStatus' - The response's http status code.
newStopSourceNetworkReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopSourceNetworkReplicationResponse
newStopSourceNetworkReplicationResponse pHttpStatus_ =
  StopSourceNetworkReplicationResponse'
    { sourceNetwork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Source Network which was requested to stop replication.
stopSourceNetworkReplicationResponse_sourceNetwork :: Lens.Lens' StopSourceNetworkReplicationResponse (Prelude.Maybe SourceNetwork)
stopSourceNetworkReplicationResponse_sourceNetwork = Lens.lens (\StopSourceNetworkReplicationResponse' {sourceNetwork} -> sourceNetwork) (\s@StopSourceNetworkReplicationResponse' {} a -> s {sourceNetwork = a} :: StopSourceNetworkReplicationResponse)

-- | The response's http status code.
stopSourceNetworkReplicationResponse_httpStatus :: Lens.Lens' StopSourceNetworkReplicationResponse Prelude.Int
stopSourceNetworkReplicationResponse_httpStatus = Lens.lens (\StopSourceNetworkReplicationResponse' {httpStatus} -> httpStatus) (\s@StopSourceNetworkReplicationResponse' {} a -> s {httpStatus = a} :: StopSourceNetworkReplicationResponse)

instance
  Prelude.NFData
    StopSourceNetworkReplicationResponse
  where
  rnf StopSourceNetworkReplicationResponse' {..} =
    Prelude.rnf sourceNetwork
      `Prelude.seq` Prelude.rnf httpStatus
