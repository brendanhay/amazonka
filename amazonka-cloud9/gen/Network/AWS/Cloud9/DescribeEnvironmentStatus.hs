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
-- Module      : Network.AWS.Cloud9.DescribeEnvironmentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status information for an AWS Cloud9 development environment.
module Network.AWS.Cloud9.DescribeEnvironmentStatus
  ( -- * Creating a Request
    DescribeEnvironmentStatus (..),
    newDescribeEnvironmentStatus,

    -- * Request Lenses
    describeEnvironmentStatus_environmentId,

    -- * Destructuring the Response
    DescribeEnvironmentStatusResponse (..),
    newDescribeEnvironmentStatusResponse,

    -- * Response Lenses
    describeEnvironmentStatusResponse_status,
    describeEnvironmentStatusResponse_message,
    describeEnvironmentStatusResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEnvironmentStatus' smart constructor.
data DescribeEnvironmentStatus = DescribeEnvironmentStatus'
  { -- | The ID of the environment to get status information about.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeEnvironmentStatus where
  type
    Rs DescribeEnvironmentStatus =
      DescribeEnvironmentStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEnvironmentStatusResponse'
            Prelude.<$> (x Prelude..?> "status")
            Prelude.<*> (x Prelude..?> "message")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEnvironmentStatus

instance Prelude.NFData DescribeEnvironmentStatus

instance Prelude.ToHeaders DescribeEnvironmentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCloud9WorkspaceManagementService.DescribeEnvironmentStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeEnvironmentStatus where
  toJSON DescribeEnvironmentStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("environmentId" Prelude..= environmentId)
          ]
      )

instance Prelude.ToPath DescribeEnvironmentStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeEnvironmentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEnvironmentStatusResponse' smart constructor.
data DescribeEnvironmentStatusResponse = DescribeEnvironmentStatusResponse'
  { -- | The status of the environment. Available values include:
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
    status :: Prelude.Maybe EnvironmentStatus,
    -- | Any informational message about the status of the environment.
    message :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'httpStatus', 'describeEnvironmentStatusResponse_httpStatus' - The response's http status code.
newDescribeEnvironmentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEnvironmentStatusResponse
newDescribeEnvironmentStatusResponse pHttpStatus_ =
  DescribeEnvironmentStatusResponse'
    { status =
        Prelude.Nothing,
      message = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

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
describeEnvironmentStatusResponse_status :: Lens.Lens' DescribeEnvironmentStatusResponse (Prelude.Maybe EnvironmentStatus)
describeEnvironmentStatusResponse_status = Lens.lens (\DescribeEnvironmentStatusResponse' {status} -> status) (\s@DescribeEnvironmentStatusResponse' {} a -> s {status = a} :: DescribeEnvironmentStatusResponse)

-- | Any informational message about the status of the environment.
describeEnvironmentStatusResponse_message :: Lens.Lens' DescribeEnvironmentStatusResponse (Prelude.Maybe Prelude.Text)
describeEnvironmentStatusResponse_message = Lens.lens (\DescribeEnvironmentStatusResponse' {message} -> message) (\s@DescribeEnvironmentStatusResponse' {} a -> s {message = a} :: DescribeEnvironmentStatusResponse)

-- | The response's http status code.
describeEnvironmentStatusResponse_httpStatus :: Lens.Lens' DescribeEnvironmentStatusResponse Prelude.Int
describeEnvironmentStatusResponse_httpStatus = Lens.lens (\DescribeEnvironmentStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentStatusResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentStatusResponse)

instance
  Prelude.NFData
    DescribeEnvironmentStatusResponse
