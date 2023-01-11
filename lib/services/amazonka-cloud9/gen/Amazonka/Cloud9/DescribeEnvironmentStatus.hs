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
-- Module      : Amazonka.Cloud9.DescribeEnvironmentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status information for an Cloud9 development environment.
module Amazonka.Cloud9.DescribeEnvironmentStatus
  ( -- * Creating a Request
    DescribeEnvironmentStatus (..),
    newDescribeEnvironmentStatus,

    -- * Request Lenses
    describeEnvironmentStatus_environmentId,

    -- * Destructuring the Response
    DescribeEnvironmentStatusResponse (..),
    newDescribeEnvironmentStatusResponse,

    -- * Response Lenses
    describeEnvironmentStatusResponse_httpStatus,
    describeEnvironmentStatusResponse_status,
    describeEnvironmentStatusResponse_message,
  )
where

import Amazonka.Cloud9.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEnvironmentStatus' smart constructor.
data DescribeEnvironmentStatus = DescribeEnvironmentStatus'
  { -- | The ID of the environment to get status information about.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'describeEnvironmentStatus_environmentId' - The ID of the environment to get status information about.
newDescribeEnvironmentStatus ::
  -- | 'environmentId'
  Prelude.Text ->
  DescribeEnvironmentStatus
newDescribeEnvironmentStatus pEnvironmentId_ =
  DescribeEnvironmentStatus'
    { environmentId =
        pEnvironmentId_
    }

-- | The ID of the environment to get status information about.
describeEnvironmentStatus_environmentId :: Lens.Lens' DescribeEnvironmentStatus Prelude.Text
describeEnvironmentStatus_environmentId = Lens.lens (\DescribeEnvironmentStatus' {environmentId} -> environmentId) (\s@DescribeEnvironmentStatus' {} a -> s {environmentId = a} :: DescribeEnvironmentStatus)

instance Core.AWSRequest DescribeEnvironmentStatus where
  type
    AWSResponse DescribeEnvironmentStatus =
      DescribeEnvironmentStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEnvironmentStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "message")
      )

instance Prelude.Hashable DescribeEnvironmentStatus where
  hashWithSalt _salt DescribeEnvironmentStatus' {..} =
    _salt `Prelude.hashWithSalt` environmentId

instance Prelude.NFData DescribeEnvironmentStatus where
  rnf DescribeEnvironmentStatus' {..} =
    Prelude.rnf environmentId

instance Data.ToHeaders DescribeEnvironmentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCloud9WorkspaceManagementService.DescribeEnvironmentStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEnvironmentStatus where
  toJSON DescribeEnvironmentStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("environmentId" Data..= environmentId)
          ]
      )

instance Data.ToPath DescribeEnvironmentStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEnvironmentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEnvironmentStatusResponse' smart constructor.
data DescribeEnvironmentStatusResponse = DescribeEnvironmentStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the environment. Available values include:
    --
    -- -   @connecting@: The environment is connecting.
    --
    -- -   @creating@: The environment is being created.
    --
    -- -   @deleting@: The environment is being deleted.
    --
    -- -   @error@: The environment is in an error state.
    --
    -- -   @ready@: The environment is ready.
    --
    -- -   @stopped@: The environment is stopped.
    --
    -- -   @stopping@: The environment is stopping.
    status :: EnvironmentStatus,
    -- | Any informational message about the status of the environment.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeEnvironmentStatusResponse_httpStatus' - The response's http status code.
--
-- 'status', 'describeEnvironmentStatusResponse_status' - The status of the environment. Available values include:
--
-- -   @connecting@: The environment is connecting.
--
-- -   @creating@: The environment is being created.
--
-- -   @deleting@: The environment is being deleted.
--
-- -   @error@: The environment is in an error state.
--
-- -   @ready@: The environment is ready.
--
-- -   @stopped@: The environment is stopped.
--
-- -   @stopping@: The environment is stopping.
--
-- 'message', 'describeEnvironmentStatusResponse_message' - Any informational message about the status of the environment.
newDescribeEnvironmentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  EnvironmentStatus ->
  -- | 'message'
  Prelude.Text ->
  DescribeEnvironmentStatusResponse
newDescribeEnvironmentStatusResponse
  pHttpStatus_
  pStatus_
  pMessage_ =
    DescribeEnvironmentStatusResponse'
      { httpStatus =
          pHttpStatus_,
        status = pStatus_,
        message = pMessage_
      }

-- | The response's http status code.
describeEnvironmentStatusResponse_httpStatus :: Lens.Lens' DescribeEnvironmentStatusResponse Prelude.Int
describeEnvironmentStatusResponse_httpStatus = Lens.lens (\DescribeEnvironmentStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentStatusResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentStatusResponse)

-- | The status of the environment. Available values include:
--
-- -   @connecting@: The environment is connecting.
--
-- -   @creating@: The environment is being created.
--
-- -   @deleting@: The environment is being deleted.
--
-- -   @error@: The environment is in an error state.
--
-- -   @ready@: The environment is ready.
--
-- -   @stopped@: The environment is stopped.
--
-- -   @stopping@: The environment is stopping.
describeEnvironmentStatusResponse_status :: Lens.Lens' DescribeEnvironmentStatusResponse EnvironmentStatus
describeEnvironmentStatusResponse_status = Lens.lens (\DescribeEnvironmentStatusResponse' {status} -> status) (\s@DescribeEnvironmentStatusResponse' {} a -> s {status = a} :: DescribeEnvironmentStatusResponse)

-- | Any informational message about the status of the environment.
describeEnvironmentStatusResponse_message :: Lens.Lens' DescribeEnvironmentStatusResponse Prelude.Text
describeEnvironmentStatusResponse_message = Lens.lens (\DescribeEnvironmentStatusResponse' {message} -> message) (\s@DescribeEnvironmentStatusResponse' {} a -> s {message = a} :: DescribeEnvironmentStatusResponse)

instance
  Prelude.NFData
    DescribeEnvironmentStatusResponse
  where
  rnf DescribeEnvironmentStatusResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf message
