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
-- Module      : Amazonka.SMS.StartOnDemandAppReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified application.
module Amazonka.SMS.StartOnDemandAppReplication
  ( -- * Creating a Request
    StartOnDemandAppReplication (..),
    newStartOnDemandAppReplication,

    -- * Request Lenses
    startOnDemandAppReplication_description,
    startOnDemandAppReplication_appId,

    -- * Destructuring the Response
    StartOnDemandAppReplicationResponse (..),
    newStartOnDemandAppReplicationResponse,

    -- * Response Lenses
    startOnDemandAppReplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newStartOnDemandAppReplication' smart constructor.
data StartOnDemandAppReplication = StartOnDemandAppReplication'
  { -- | The description of the replication run.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartOnDemandAppReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'startOnDemandAppReplication_description' - The description of the replication run.
--
-- 'appId', 'startOnDemandAppReplication_appId' - The ID of the application.
newStartOnDemandAppReplication ::
  -- | 'appId'
  Prelude.Text ->
  StartOnDemandAppReplication
newStartOnDemandAppReplication pAppId_ =
  StartOnDemandAppReplication'
    { description =
        Prelude.Nothing,
      appId = pAppId_
    }

-- | The description of the replication run.
startOnDemandAppReplication_description :: Lens.Lens' StartOnDemandAppReplication (Prelude.Maybe Prelude.Text)
startOnDemandAppReplication_description = Lens.lens (\StartOnDemandAppReplication' {description} -> description) (\s@StartOnDemandAppReplication' {} a -> s {description = a} :: StartOnDemandAppReplication)

-- | The ID of the application.
startOnDemandAppReplication_appId :: Lens.Lens' StartOnDemandAppReplication Prelude.Text
startOnDemandAppReplication_appId = Lens.lens (\StartOnDemandAppReplication' {appId} -> appId) (\s@StartOnDemandAppReplication' {} a -> s {appId = a} :: StartOnDemandAppReplication)

instance Core.AWSRequest StartOnDemandAppReplication where
  type
    AWSResponse StartOnDemandAppReplication =
      StartOnDemandAppReplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartOnDemandAppReplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartOnDemandAppReplication where
  hashWithSalt _salt StartOnDemandAppReplication' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` appId

instance Prelude.NFData StartOnDemandAppReplication where
  rnf StartOnDemandAppReplication' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders StartOnDemandAppReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.StartOnDemandAppReplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartOnDemandAppReplication where
  toJSON StartOnDemandAppReplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("appId" Data..= appId)
          ]
      )

instance Data.ToPath StartOnDemandAppReplication where
  toPath = Prelude.const "/"

instance Data.ToQuery StartOnDemandAppReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartOnDemandAppReplicationResponse' smart constructor.
data StartOnDemandAppReplicationResponse = StartOnDemandAppReplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartOnDemandAppReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startOnDemandAppReplicationResponse_httpStatus' - The response's http status code.
newStartOnDemandAppReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartOnDemandAppReplicationResponse
newStartOnDemandAppReplicationResponse pHttpStatus_ =
  StartOnDemandAppReplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startOnDemandAppReplicationResponse_httpStatus :: Lens.Lens' StartOnDemandAppReplicationResponse Prelude.Int
startOnDemandAppReplicationResponse_httpStatus = Lens.lens (\StartOnDemandAppReplicationResponse' {httpStatus} -> httpStatus) (\s@StartOnDemandAppReplicationResponse' {} a -> s {httpStatus = a} :: StartOnDemandAppReplicationResponse)

instance
  Prelude.NFData
    StartOnDemandAppReplicationResponse
  where
  rnf StartOnDemandAppReplicationResponse' {..} =
    Prelude.rnf httpStatus
