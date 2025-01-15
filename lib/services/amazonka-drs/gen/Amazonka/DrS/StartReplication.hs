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
-- Module      : Amazonka.DrS.StartReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts replication for a stopped Source Server. This action would make
-- the Source Server protected again and restart billing for it.
module Amazonka.DrS.StartReplication
  ( -- * Creating a Request
    StartReplication (..),
    newStartReplication,

    -- * Request Lenses
    startReplication_sourceServerID,

    -- * Destructuring the Response
    StartReplicationResponse (..),
    newStartReplicationResponse,

    -- * Response Lenses
    startReplicationResponse_sourceServer,
    startReplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartReplication' smart constructor.
data StartReplication = StartReplication'
  { -- | The ID of the Source Server to start replication for.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServerID', 'startReplication_sourceServerID' - The ID of the Source Server to start replication for.
newStartReplication ::
  -- | 'sourceServerID'
  Prelude.Text ->
  StartReplication
newStartReplication pSourceServerID_ =
  StartReplication'
    { sourceServerID =
        pSourceServerID_
    }

-- | The ID of the Source Server to start replication for.
startReplication_sourceServerID :: Lens.Lens' StartReplication Prelude.Text
startReplication_sourceServerID = Lens.lens (\StartReplication' {sourceServerID} -> sourceServerID) (\s@StartReplication' {} a -> s {sourceServerID = a} :: StartReplication)

instance Core.AWSRequest StartReplication where
  type
    AWSResponse StartReplication =
      StartReplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplicationResponse'
            Prelude.<$> (x Data..?> "sourceServer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartReplication where
  hashWithSalt _salt StartReplication' {..} =
    _salt `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData StartReplication where
  rnf StartReplication' {..} =
    Prelude.rnf sourceServerID

instance Data.ToHeaders StartReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartReplication where
  toJSON StartReplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath StartReplication where
  toPath = Prelude.const "/StartReplication"

instance Data.ToQuery StartReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartReplicationResponse' smart constructor.
data StartReplicationResponse = StartReplicationResponse'
  { -- | The Source Server that this action was targeted on.
    sourceServer :: Prelude.Maybe SourceServer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServer', 'startReplicationResponse_sourceServer' - The Source Server that this action was targeted on.
--
-- 'httpStatus', 'startReplicationResponse_httpStatus' - The response's http status code.
newStartReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartReplicationResponse
newStartReplicationResponse pHttpStatus_ =
  StartReplicationResponse'
    { sourceServer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Source Server that this action was targeted on.
startReplicationResponse_sourceServer :: Lens.Lens' StartReplicationResponse (Prelude.Maybe SourceServer)
startReplicationResponse_sourceServer = Lens.lens (\StartReplicationResponse' {sourceServer} -> sourceServer) (\s@StartReplicationResponse' {} a -> s {sourceServer = a} :: StartReplicationResponse)

-- | The response's http status code.
startReplicationResponse_httpStatus :: Lens.Lens' StartReplicationResponse Prelude.Int
startReplicationResponse_httpStatus = Lens.lens (\StartReplicationResponse' {httpStatus} -> httpStatus) (\s@StartReplicationResponse' {} a -> s {httpStatus = a} :: StartReplicationResponse)

instance Prelude.NFData StartReplicationResponse where
  rnf StartReplicationResponse' {..} =
    Prelude.rnf sourceServer `Prelude.seq`
      Prelude.rnf httpStatus
