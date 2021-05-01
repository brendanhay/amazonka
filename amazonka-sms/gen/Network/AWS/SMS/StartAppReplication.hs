{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SMS.StartAppReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts replicating the specified application by creating replication
-- jobs for each server in the application.
module Network.AWS.SMS.StartAppReplication
  ( -- * Creating a Request
    StartAppReplication (..),
    newStartAppReplication,

    -- * Request Lenses
    startAppReplication_appId,

    -- * Destructuring the Response
    StartAppReplicationResponse (..),
    newStartAppReplicationResponse,

    -- * Response Lenses
    startAppReplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newStartAppReplication' smart constructor.
data StartAppReplication = StartAppReplication'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartAppReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'startAppReplication_appId' - The ID of the application.
newStartAppReplication ::
  StartAppReplication
newStartAppReplication =
  StartAppReplication' {appId = Prelude.Nothing}

-- | The ID of the application.
startAppReplication_appId :: Lens.Lens' StartAppReplication (Prelude.Maybe Prelude.Text)
startAppReplication_appId = Lens.lens (\StartAppReplication' {appId} -> appId) (\s@StartAppReplication' {} a -> s {appId = a} :: StartAppReplication)

instance Prelude.AWSRequest StartAppReplication where
  type
    Rs StartAppReplication =
      StartAppReplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartAppReplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartAppReplication

instance Prelude.NFData StartAppReplication

instance Prelude.ToHeaders StartAppReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.StartAppReplication" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartAppReplication where
  toJSON StartAppReplication' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("appId" Prelude..=) Prelude.<$> appId]
      )

instance Prelude.ToPath StartAppReplication where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartAppReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAppReplicationResponse' smart constructor.
data StartAppReplicationResponse = StartAppReplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartAppReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startAppReplicationResponse_httpStatus' - The response's http status code.
newStartAppReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartAppReplicationResponse
newStartAppReplicationResponse pHttpStatus_ =
  StartAppReplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startAppReplicationResponse_httpStatus :: Lens.Lens' StartAppReplicationResponse Prelude.Int
startAppReplicationResponse_httpStatus = Lens.lens (\StartAppReplicationResponse' {httpStatus} -> httpStatus) (\s@StartAppReplicationResponse' {} a -> s {httpStatus = a} :: StartAppReplicationResponse)

instance Prelude.NFData StartAppReplicationResponse
