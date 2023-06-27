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
-- Module      : Amazonka.DrS.StartSourceNetworkReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts replication for a Source Network. This action would make the
-- Source Network protected.
module Amazonka.DrS.StartSourceNetworkReplication
  ( -- * Creating a Request
    StartSourceNetworkReplication (..),
    newStartSourceNetworkReplication,

    -- * Request Lenses
    startSourceNetworkReplication_sourceNetworkID,

    -- * Destructuring the Response
    StartSourceNetworkReplicationResponse (..),
    newStartSourceNetworkReplicationResponse,

    -- * Response Lenses
    startSourceNetworkReplicationResponse_sourceNetwork,
    startSourceNetworkReplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSourceNetworkReplication' smart constructor.
data StartSourceNetworkReplication = StartSourceNetworkReplication'
  { -- | ID of the Source Network to replicate.
    sourceNetworkID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSourceNetworkReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetworkID', 'startSourceNetworkReplication_sourceNetworkID' - ID of the Source Network to replicate.
newStartSourceNetworkReplication ::
  -- | 'sourceNetworkID'
  Prelude.Text ->
  StartSourceNetworkReplication
newStartSourceNetworkReplication pSourceNetworkID_ =
  StartSourceNetworkReplication'
    { sourceNetworkID =
        pSourceNetworkID_
    }

-- | ID of the Source Network to replicate.
startSourceNetworkReplication_sourceNetworkID :: Lens.Lens' StartSourceNetworkReplication Prelude.Text
startSourceNetworkReplication_sourceNetworkID = Lens.lens (\StartSourceNetworkReplication' {sourceNetworkID} -> sourceNetworkID) (\s@StartSourceNetworkReplication' {} a -> s {sourceNetworkID = a} :: StartSourceNetworkReplication)

instance
  Core.AWSRequest
    StartSourceNetworkReplication
  where
  type
    AWSResponse StartSourceNetworkReplication =
      StartSourceNetworkReplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSourceNetworkReplicationResponse'
            Prelude.<$> (x Data..?> "sourceNetwork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartSourceNetworkReplication
  where
  hashWithSalt _salt StartSourceNetworkReplication' {..} =
    _salt `Prelude.hashWithSalt` sourceNetworkID

instance Prelude.NFData StartSourceNetworkReplication where
  rnf StartSourceNetworkReplication' {..} =
    Prelude.rnf sourceNetworkID

instance Data.ToHeaders StartSourceNetworkReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSourceNetworkReplication where
  toJSON StartSourceNetworkReplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceNetworkID" Data..= sourceNetworkID)
          ]
      )

instance Data.ToPath StartSourceNetworkReplication where
  toPath =
    Prelude.const "/StartSourceNetworkReplication"

instance Data.ToQuery StartSourceNetworkReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSourceNetworkReplicationResponse' smart constructor.
data StartSourceNetworkReplicationResponse = StartSourceNetworkReplicationResponse'
  { -- | Source Network which was requested for replication.
    sourceNetwork :: Prelude.Maybe SourceNetwork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSourceNetworkReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetwork', 'startSourceNetworkReplicationResponse_sourceNetwork' - Source Network which was requested for replication.
--
-- 'httpStatus', 'startSourceNetworkReplicationResponse_httpStatus' - The response's http status code.
newStartSourceNetworkReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSourceNetworkReplicationResponse
newStartSourceNetworkReplicationResponse pHttpStatus_ =
  StartSourceNetworkReplicationResponse'
    { sourceNetwork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Source Network which was requested for replication.
startSourceNetworkReplicationResponse_sourceNetwork :: Lens.Lens' StartSourceNetworkReplicationResponse (Prelude.Maybe SourceNetwork)
startSourceNetworkReplicationResponse_sourceNetwork = Lens.lens (\StartSourceNetworkReplicationResponse' {sourceNetwork} -> sourceNetwork) (\s@StartSourceNetworkReplicationResponse' {} a -> s {sourceNetwork = a} :: StartSourceNetworkReplicationResponse)

-- | The response's http status code.
startSourceNetworkReplicationResponse_httpStatus :: Lens.Lens' StartSourceNetworkReplicationResponse Prelude.Int
startSourceNetworkReplicationResponse_httpStatus = Lens.lens (\StartSourceNetworkReplicationResponse' {httpStatus} -> httpStatus) (\s@StartSourceNetworkReplicationResponse' {} a -> s {httpStatus = a} :: StartSourceNetworkReplicationResponse)

instance
  Prelude.NFData
    StartSourceNetworkReplicationResponse
  where
  rnf StartSourceNetworkReplicationResponse' {..} =
    Prelude.rnf sourceNetwork
      `Prelude.seq` Prelude.rnf httpStatus
