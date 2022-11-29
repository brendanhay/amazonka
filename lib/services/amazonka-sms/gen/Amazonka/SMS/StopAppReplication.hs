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
-- Module      : Amazonka.SMS.StopAppReplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replicating the specified application by deleting the replication
-- job for each server in the application.
module Amazonka.SMS.StopAppReplication
  ( -- * Creating a Request
    StopAppReplication (..),
    newStopAppReplication,

    -- * Request Lenses
    stopAppReplication_appId,

    -- * Destructuring the Response
    StopAppReplicationResponse (..),
    newStopAppReplicationResponse,

    -- * Response Lenses
    stopAppReplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newStopAppReplication' smart constructor.
data StopAppReplication = StopAppReplication'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAppReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'stopAppReplication_appId' - The ID of the application.
newStopAppReplication ::
  StopAppReplication
newStopAppReplication =
  StopAppReplication' {appId = Prelude.Nothing}

-- | The ID of the application.
stopAppReplication_appId :: Lens.Lens' StopAppReplication (Prelude.Maybe Prelude.Text)
stopAppReplication_appId = Lens.lens (\StopAppReplication' {appId} -> appId) (\s@StopAppReplication' {} a -> s {appId = a} :: StopAppReplication)

instance Core.AWSRequest StopAppReplication where
  type
    AWSResponse StopAppReplication =
      StopAppReplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopAppReplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopAppReplication where
  hashWithSalt _salt StopAppReplication' {..} =
    _salt `Prelude.hashWithSalt` appId

instance Prelude.NFData StopAppReplication where
  rnf StopAppReplication' {..} = Prelude.rnf appId

instance Core.ToHeaders StopAppReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.StopAppReplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopAppReplication where
  toJSON StopAppReplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [("appId" Core..=) Prelude.<$> appId]
      )

instance Core.ToPath StopAppReplication where
  toPath = Prelude.const "/"

instance Core.ToQuery StopAppReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopAppReplicationResponse' smart constructor.
data StopAppReplicationResponse = StopAppReplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAppReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopAppReplicationResponse_httpStatus' - The response's http status code.
newStopAppReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopAppReplicationResponse
newStopAppReplicationResponse pHttpStatus_ =
  StopAppReplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopAppReplicationResponse_httpStatus :: Lens.Lens' StopAppReplicationResponse Prelude.Int
stopAppReplicationResponse_httpStatus = Lens.lens (\StopAppReplicationResponse' {httpStatus} -> httpStatus) (\s@StopAppReplicationResponse' {} a -> s {httpStatus = a} :: StopAppReplicationResponse)

instance Prelude.NFData StopAppReplicationResponse where
  rnf StopAppReplicationResponse' {..} =
    Prelude.rnf httpStatus
