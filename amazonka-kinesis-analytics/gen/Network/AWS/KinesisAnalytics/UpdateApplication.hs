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
-- Module      : Network.AWS.KinesisAnalytics.UpdateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This documentation is for version 1 of the Amazon Kinesis Data Analytics
-- API, which only supports SQL applications. Version 2 of the API supports
-- SQL and Java applications. For more information about version 2, see
-- </kinesisanalytics/latest/apiv2/Welcome.html Amazon Kinesis Data Analytics API V2 Documentation>.
--
-- Updates an existing Amazon Kinesis Analytics application. Using this
-- API, you can update application code, input configuration, and output
-- configuration.
--
-- Note that Amazon Kinesis Analytics updates the
-- @CurrentApplicationVersionId@ each time you update your application.
--
-- This operation requires permission for the
-- @kinesisanalytics:UpdateApplication@ action.
module Network.AWS.KinesisAnalytics.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_applicationName,
    updateApplication_currentApplicationVersionId,
    updateApplication_applicationUpdate,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | Name of the Amazon Kinesis Analytics application to update.
    applicationName :: Core.Text,
    -- | The current application version ID. You can use the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation to get this value.
    currentApplicationVersionId :: Core.Natural,
    -- | Describes application updates.
    applicationUpdate :: ApplicationUpdate
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'updateApplication_applicationName' - Name of the Amazon Kinesis Analytics application to update.
--
-- 'currentApplicationVersionId', 'updateApplication_currentApplicationVersionId' - The current application version ID. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get this value.
--
-- 'applicationUpdate', 'updateApplication_applicationUpdate' - Describes application updates.
newUpdateApplication ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'currentApplicationVersionId'
  Core.Natural ->
  -- | 'applicationUpdate'
  ApplicationUpdate ->
  UpdateApplication
newUpdateApplication
  pApplicationName_
  pCurrentApplicationVersionId_
  pApplicationUpdate_ =
    UpdateApplication'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        applicationUpdate = pApplicationUpdate_
      }

-- | Name of the Amazon Kinesis Analytics application to update.
updateApplication_applicationName :: Lens.Lens' UpdateApplication Core.Text
updateApplication_applicationName = Lens.lens (\UpdateApplication' {applicationName} -> applicationName) (\s@UpdateApplication' {} a -> s {applicationName = a} :: UpdateApplication)

-- | The current application version ID. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get this value.
updateApplication_currentApplicationVersionId :: Lens.Lens' UpdateApplication Core.Natural
updateApplication_currentApplicationVersionId = Lens.lens (\UpdateApplication' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@UpdateApplication' {} a -> s {currentApplicationVersionId = a} :: UpdateApplication)

-- | Describes application updates.
updateApplication_applicationUpdate :: Lens.Lens' UpdateApplication ApplicationUpdate
updateApplication_applicationUpdate = Lens.lens (\UpdateApplication' {applicationUpdate} -> applicationUpdate) (\s@UpdateApplication' {} a -> s {applicationUpdate = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateApplicationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateApplication

instance Core.NFData UpdateApplication

instance Core.ToHeaders UpdateApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20150814.UpdateApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ApplicationName" Core..= applicationName),
            Core.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Core.Just
              ("ApplicationUpdate" Core..= applicationUpdate)
          ]
      )

instance Core.ToPath UpdateApplication where
  toPath = Core.const "/"

instance Core.ToQuery UpdateApplication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApplicationResponse_httpStatus' - The response's http status code.
newUpdateApplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateApplicationResponse
newUpdateApplicationResponse pHttpStatus_ =
  UpdateApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Core.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

instance Core.NFData UpdateApplicationResponse
