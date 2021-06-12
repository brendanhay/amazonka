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
-- Module      : Network.AWS.SMS.StartOnDemandAppReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified application.
module Network.AWS.SMS.StartOnDemandAppReplication
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newStartOnDemandAppReplication' smart constructor.
data StartOnDemandAppReplication = StartOnDemandAppReplication'
  { -- | The description of the replication run.
    description :: Core.Maybe Core.Text,
    -- | The ID of the application.
    appId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  StartOnDemandAppReplication
newStartOnDemandAppReplication pAppId_ =
  StartOnDemandAppReplication'
    { description =
        Core.Nothing,
      appId = pAppId_
    }

-- | The description of the replication run.
startOnDemandAppReplication_description :: Lens.Lens' StartOnDemandAppReplication (Core.Maybe Core.Text)
startOnDemandAppReplication_description = Lens.lens (\StartOnDemandAppReplication' {description} -> description) (\s@StartOnDemandAppReplication' {} a -> s {description = a} :: StartOnDemandAppReplication)

-- | The ID of the application.
startOnDemandAppReplication_appId :: Lens.Lens' StartOnDemandAppReplication Core.Text
startOnDemandAppReplication_appId = Lens.lens (\StartOnDemandAppReplication' {appId} -> appId) (\s@StartOnDemandAppReplication' {} a -> s {appId = a} :: StartOnDemandAppReplication)

instance Core.AWSRequest StartOnDemandAppReplication where
  type
    AWSResponse StartOnDemandAppReplication =
      StartOnDemandAppReplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartOnDemandAppReplicationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartOnDemandAppReplication

instance Core.NFData StartOnDemandAppReplication

instance Core.ToHeaders StartOnDemandAppReplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.StartOnDemandAppReplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartOnDemandAppReplication where
  toJSON StartOnDemandAppReplication' {..} =
    Core.object
      ( Core.catMaybes
          [ ("description" Core..=) Core.<$> description,
            Core.Just ("appId" Core..= appId)
          ]
      )

instance Core.ToPath StartOnDemandAppReplication where
  toPath = Core.const "/"

instance Core.ToQuery StartOnDemandAppReplication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartOnDemandAppReplicationResponse' smart constructor.
data StartOnDemandAppReplicationResponse = StartOnDemandAppReplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StartOnDemandAppReplicationResponse
newStartOnDemandAppReplicationResponse pHttpStatus_ =
  StartOnDemandAppReplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startOnDemandAppReplicationResponse_httpStatus :: Lens.Lens' StartOnDemandAppReplicationResponse Core.Int
startOnDemandAppReplicationResponse_httpStatus = Lens.lens (\StartOnDemandAppReplicationResponse' {httpStatus} -> httpStatus) (\s@StartOnDemandAppReplicationResponse' {} a -> s {httpStatus = a} :: StartOnDemandAppReplicationResponse)

instance
  Core.NFData
    StartOnDemandAppReplicationResponse
