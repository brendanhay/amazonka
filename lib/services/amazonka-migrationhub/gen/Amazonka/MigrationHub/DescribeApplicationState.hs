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
-- Module      : Amazonka.MigrationHub.DescribeApplicationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the migration status of an application.
module Amazonka.MigrationHub.DescribeApplicationState
  ( -- * Creating a Request
    DescribeApplicationState (..),
    newDescribeApplicationState,

    -- * Request Lenses
    describeApplicationState_applicationId,

    -- * Destructuring the Response
    DescribeApplicationStateResponse (..),
    newDescribeApplicationStateResponse,

    -- * Response Lenses
    describeApplicationStateResponse_lastUpdatedTime,
    describeApplicationStateResponse_applicationStatus,
    describeApplicationStateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApplicationState' smart constructor.
data DescribeApplicationState = DescribeApplicationState'
  { -- | The configurationId in Application Discovery Service that uniquely
    -- identifies the grouped application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'describeApplicationState_applicationId' - The configurationId in Application Discovery Service that uniquely
-- identifies the grouped application.
newDescribeApplicationState ::
  -- | 'applicationId'
  Prelude.Text ->
  DescribeApplicationState
newDescribeApplicationState pApplicationId_ =
  DescribeApplicationState'
    { applicationId =
        pApplicationId_
    }

-- | The configurationId in Application Discovery Service that uniquely
-- identifies the grouped application.
describeApplicationState_applicationId :: Lens.Lens' DescribeApplicationState Prelude.Text
describeApplicationState_applicationId = Lens.lens (\DescribeApplicationState' {applicationId} -> applicationId) (\s@DescribeApplicationState' {} a -> s {applicationId = a} :: DescribeApplicationState)

instance Core.AWSRequest DescribeApplicationState where
  type
    AWSResponse DescribeApplicationState =
      DescribeApplicationStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationStateResponse'
            Prelude.<$> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "ApplicationStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApplicationState where
  hashWithSalt _salt DescribeApplicationState' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DescribeApplicationState where
  rnf DescribeApplicationState' {..} =
    Prelude.rnf applicationId

instance Data.ToHeaders DescribeApplicationState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHub.DescribeApplicationState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeApplicationState where
  toJSON DescribeApplicationState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationId" Data..= applicationId)
          ]
      )

instance Data.ToPath DescribeApplicationState where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeApplicationState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApplicationStateResponse' smart constructor.
data DescribeApplicationStateResponse = DescribeApplicationStateResponse'
  { -- | The timestamp when the application status was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Status of the application - Not Started, In-Progress, Complete.
    applicationStatus :: Prelude.Maybe ApplicationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'describeApplicationStateResponse_lastUpdatedTime' - The timestamp when the application status was last updated.
--
-- 'applicationStatus', 'describeApplicationStateResponse_applicationStatus' - Status of the application - Not Started, In-Progress, Complete.
--
-- 'httpStatus', 'describeApplicationStateResponse_httpStatus' - The response's http status code.
newDescribeApplicationStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeApplicationStateResponse
newDescribeApplicationStateResponse pHttpStatus_ =
  DescribeApplicationStateResponse'
    { lastUpdatedTime =
        Prelude.Nothing,
      applicationStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp when the application status was last updated.
describeApplicationStateResponse_lastUpdatedTime :: Lens.Lens' DescribeApplicationStateResponse (Prelude.Maybe Prelude.UTCTime)
describeApplicationStateResponse_lastUpdatedTime = Lens.lens (\DescribeApplicationStateResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@DescribeApplicationStateResponse' {} a -> s {lastUpdatedTime = a} :: DescribeApplicationStateResponse) Prelude.. Lens.mapping Data._Time

-- | Status of the application - Not Started, In-Progress, Complete.
describeApplicationStateResponse_applicationStatus :: Lens.Lens' DescribeApplicationStateResponse (Prelude.Maybe ApplicationStatus)
describeApplicationStateResponse_applicationStatus = Lens.lens (\DescribeApplicationStateResponse' {applicationStatus} -> applicationStatus) (\s@DescribeApplicationStateResponse' {} a -> s {applicationStatus = a} :: DescribeApplicationStateResponse)

-- | The response's http status code.
describeApplicationStateResponse_httpStatus :: Lens.Lens' DescribeApplicationStateResponse Prelude.Int
describeApplicationStateResponse_httpStatus = Lens.lens (\DescribeApplicationStateResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationStateResponse' {} a -> s {httpStatus = a} :: DescribeApplicationStateResponse)

instance
  Prelude.NFData
    DescribeApplicationStateResponse
  where
  rnf DescribeApplicationStateResponse' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf applicationStatus
      `Prelude.seq` Prelude.rnf httpStatus
