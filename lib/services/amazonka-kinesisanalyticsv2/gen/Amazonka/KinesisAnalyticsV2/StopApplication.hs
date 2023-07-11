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
-- Module      : Amazonka.KinesisAnalyticsV2.StopApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the application from processing data. You can stop an application
-- only if it is in the running status, unless you set the @Force@
-- parameter to @true@.
--
-- You can use the DescribeApplication operation to find the application
-- status.
--
-- Kinesis Data Analytics takes a snapshot when the application is stopped,
-- unless @Force@ is set to @true@.
module Amazonka.KinesisAnalyticsV2.StopApplication
  ( -- * Creating a Request
    StopApplication (..),
    newStopApplication,

    -- * Request Lenses
    stopApplication_force,
    stopApplication_applicationName,

    -- * Destructuring the Response
    StopApplicationResponse (..),
    newStopApplicationResponse,

    -- * Response Lenses
    stopApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopApplication' smart constructor.
data StopApplication = StopApplication'
  { -- | Set to @true@ to force the application to stop. If you set @Force@ to
    -- @true@, Kinesis Data Analytics stops the application without taking a
    -- snapshot.
    --
    -- Force-stopping your application may lead to data loss or duplication. To
    -- prevent data loss or duplicate processing of data during application
    -- restarts, we recommend you to take frequent snapshots of your
    -- application.
    --
    -- You can only force stop a Flink-based Kinesis Data Analytics
    -- application. You can\'t force stop a SQL-based Kinesis Data Analytics
    -- application.
    --
    -- The application must be in the @STARTING@, @UPDATING@, @STOPPING@,
    -- @AUTOSCALING@, or @RUNNING@ status.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The name of the running application to stop.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'stopApplication_force' - Set to @true@ to force the application to stop. If you set @Force@ to
-- @true@, Kinesis Data Analytics stops the application without taking a
-- snapshot.
--
-- Force-stopping your application may lead to data loss or duplication. To
-- prevent data loss or duplicate processing of data during application
-- restarts, we recommend you to take frequent snapshots of your
-- application.
--
-- You can only force stop a Flink-based Kinesis Data Analytics
-- application. You can\'t force stop a SQL-based Kinesis Data Analytics
-- application.
--
-- The application must be in the @STARTING@, @UPDATING@, @STOPPING@,
-- @AUTOSCALING@, or @RUNNING@ status.
--
-- 'applicationName', 'stopApplication_applicationName' - The name of the running application to stop.
newStopApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  StopApplication
newStopApplication pApplicationName_ =
  StopApplication'
    { force = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | Set to @true@ to force the application to stop. If you set @Force@ to
-- @true@, Kinesis Data Analytics stops the application without taking a
-- snapshot.
--
-- Force-stopping your application may lead to data loss or duplication. To
-- prevent data loss or duplicate processing of data during application
-- restarts, we recommend you to take frequent snapshots of your
-- application.
--
-- You can only force stop a Flink-based Kinesis Data Analytics
-- application. You can\'t force stop a SQL-based Kinesis Data Analytics
-- application.
--
-- The application must be in the @STARTING@, @UPDATING@, @STOPPING@,
-- @AUTOSCALING@, or @RUNNING@ status.
stopApplication_force :: Lens.Lens' StopApplication (Prelude.Maybe Prelude.Bool)
stopApplication_force = Lens.lens (\StopApplication' {force} -> force) (\s@StopApplication' {} a -> s {force = a} :: StopApplication)

-- | The name of the running application to stop.
stopApplication_applicationName :: Lens.Lens' StopApplication Prelude.Text
stopApplication_applicationName = Lens.lens (\StopApplication' {applicationName} -> applicationName) (\s@StopApplication' {} a -> s {applicationName = a} :: StopApplication)

instance Core.AWSRequest StopApplication where
  type
    AWSResponse StopApplication =
      StopApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopApplication where
  hashWithSalt _salt StopApplication' {..} =
    _salt
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData StopApplication where
  rnf StopApplication' {..} =
    Prelude.rnf force
      `Prelude.seq` Prelude.rnf applicationName

instance Data.ToHeaders StopApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.StopApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopApplication where
  toJSON StopApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Force" Data..=) Prelude.<$> force,
            Prelude.Just
              ("ApplicationName" Data..= applicationName)
          ]
      )

instance Data.ToPath StopApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery StopApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopApplicationResponse' smart constructor.
data StopApplicationResponse = StopApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopApplicationResponse_httpStatus' - The response's http status code.
newStopApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopApplicationResponse
newStopApplicationResponse pHttpStatus_ =
  StopApplicationResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopApplicationResponse_httpStatus :: Lens.Lens' StopApplicationResponse Prelude.Int
stopApplicationResponse_httpStatus = Lens.lens (\StopApplicationResponse' {httpStatus} -> httpStatus) (\s@StopApplicationResponse' {} a -> s {httpStatus = a} :: StopApplicationResponse)

instance Prelude.NFData StopApplicationResponse where
  rnf StopApplicationResponse' {..} =
    Prelude.rnf httpStatus
