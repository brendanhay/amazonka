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
-- Module      : Amazonka.KinesisAnalyticsV2.RollbackApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reverts the application to the previous running version. You can roll
-- back an application if you suspect it is stuck in a transient status.
--
-- You can roll back an application only if it is in the @UPDATING@ or
-- @AUTOSCALING@ status.
--
-- When you rollback an application, it loads state data from the last
-- successful snapshot. If the application has no snapshots, Kinesis Data
-- Analytics rejects the rollback request.
--
-- This action is not supported for Kinesis Data Analytics for SQL
-- applications.
module Amazonka.KinesisAnalyticsV2.RollbackApplication
  ( -- * Creating a Request
    RollbackApplication (..),
    newRollbackApplication,

    -- * Request Lenses
    rollbackApplication_applicationName,
    rollbackApplication_currentApplicationVersionId,

    -- * Destructuring the Response
    RollbackApplicationResponse (..),
    newRollbackApplicationResponse,

    -- * Response Lenses
    rollbackApplicationResponse_httpStatus,
    rollbackApplicationResponse_applicationDetail,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRollbackApplication' smart constructor.
data RollbackApplication = RollbackApplication'
  { -- | The name of the application.
    applicationName :: Prelude.Text,
    -- | The current application version ID. You can retrieve the application
    -- version ID using DescribeApplication.
    currentApplicationVersionId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'rollbackApplication_applicationName' - The name of the application.
--
-- 'currentApplicationVersionId', 'rollbackApplication_currentApplicationVersionId' - The current application version ID. You can retrieve the application
-- version ID using DescribeApplication.
newRollbackApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  RollbackApplication
newRollbackApplication
  pApplicationName_
  pCurrentApplicationVersionId_ =
    RollbackApplication'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_
      }

-- | The name of the application.
rollbackApplication_applicationName :: Lens.Lens' RollbackApplication Prelude.Text
rollbackApplication_applicationName = Lens.lens (\RollbackApplication' {applicationName} -> applicationName) (\s@RollbackApplication' {} a -> s {applicationName = a} :: RollbackApplication)

-- | The current application version ID. You can retrieve the application
-- version ID using DescribeApplication.
rollbackApplication_currentApplicationVersionId :: Lens.Lens' RollbackApplication Prelude.Natural
rollbackApplication_currentApplicationVersionId = Lens.lens (\RollbackApplication' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@RollbackApplication' {} a -> s {currentApplicationVersionId = a} :: RollbackApplication)

instance Core.AWSRequest RollbackApplication where
  type
    AWSResponse RollbackApplication =
      RollbackApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RollbackApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ApplicationDetail")
      )

instance Prelude.Hashable RollbackApplication where
  hashWithSalt _salt RollbackApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` currentApplicationVersionId

instance Prelude.NFData RollbackApplication where
  rnf RollbackApplication' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf currentApplicationVersionId

instance Data.ToHeaders RollbackApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.RollbackApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RollbackApplication where
  toJSON RollbackApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ( "CurrentApplicationVersionId"
                  Data..= currentApplicationVersionId
              )
          ]
      )

instance Data.ToPath RollbackApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery RollbackApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRollbackApplicationResponse' smart constructor.
data RollbackApplicationResponse = RollbackApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    applicationDetail :: ApplicationDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rollbackApplicationResponse_httpStatus' - The response's http status code.
--
-- 'applicationDetail', 'rollbackApplicationResponse_applicationDetail' - Undocumented member.
newRollbackApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationDetail'
  ApplicationDetail ->
  RollbackApplicationResponse
newRollbackApplicationResponse
  pHttpStatus_
  pApplicationDetail_ =
    RollbackApplicationResponse'
      { httpStatus =
          pHttpStatus_,
        applicationDetail = pApplicationDetail_
      }

-- | The response's http status code.
rollbackApplicationResponse_httpStatus :: Lens.Lens' RollbackApplicationResponse Prelude.Int
rollbackApplicationResponse_httpStatus = Lens.lens (\RollbackApplicationResponse' {httpStatus} -> httpStatus) (\s@RollbackApplicationResponse' {} a -> s {httpStatus = a} :: RollbackApplicationResponse)

-- | Undocumented member.
rollbackApplicationResponse_applicationDetail :: Lens.Lens' RollbackApplicationResponse ApplicationDetail
rollbackApplicationResponse_applicationDetail = Lens.lens (\RollbackApplicationResponse' {applicationDetail} -> applicationDetail) (\s@RollbackApplicationResponse' {} a -> s {applicationDetail = a} :: RollbackApplicationResponse)

instance Prelude.NFData RollbackApplicationResponse where
  rnf RollbackApplicationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationDetail
