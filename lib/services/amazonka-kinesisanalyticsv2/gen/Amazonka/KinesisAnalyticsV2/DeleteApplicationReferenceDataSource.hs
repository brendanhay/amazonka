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
-- Module      : Amazonka.KinesisAnalyticsV2.DeleteApplicationReferenceDataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reference data source configuration from the specified
-- SQL-based Kinesis Data Analytics application\'s configuration.
--
-- If the application is running, Kinesis Data Analytics immediately
-- removes the in-application table that you created using the
-- AddApplicationReferenceDataSource operation.
module Amazonka.KinesisAnalyticsV2.DeleteApplicationReferenceDataSource
  ( -- * Creating a Request
    DeleteApplicationReferenceDataSource (..),
    newDeleteApplicationReferenceDataSource,

    -- * Request Lenses
    deleteApplicationReferenceDataSource_applicationName,
    deleteApplicationReferenceDataSource_currentApplicationVersionId,
    deleteApplicationReferenceDataSource_referenceId,

    -- * Destructuring the Response
    DeleteApplicationReferenceDataSourceResponse (..),
    newDeleteApplicationReferenceDataSourceResponse,

    -- * Response Lenses
    deleteApplicationReferenceDataSourceResponse_applicationARN,
    deleteApplicationReferenceDataSourceResponse_applicationVersionId,
    deleteApplicationReferenceDataSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplicationReferenceDataSource' smart constructor.
data DeleteApplicationReferenceDataSource = DeleteApplicationReferenceDataSource'
  { -- | The name of an existing application.
    applicationName :: Prelude.Text,
    -- | The current application version. You can use the DescribeApplication
    -- operation to get the current application version. If the version
    -- specified is not the current version, the
    -- @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Prelude.Natural,
    -- | The ID of the reference data source. When you add a reference data
    -- source to your application using the AddApplicationReferenceDataSource,
    -- Kinesis Data Analytics assigns an ID. You can use the
    -- DescribeApplication operation to get the reference ID.
    referenceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationReferenceDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteApplicationReferenceDataSource_applicationName' - The name of an existing application.
--
-- 'currentApplicationVersionId', 'deleteApplicationReferenceDataSource_currentApplicationVersionId' - The current application version. You can use the DescribeApplication
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
--
-- 'referenceId', 'deleteApplicationReferenceDataSource_referenceId' - The ID of the reference data source. When you add a reference data
-- source to your application using the AddApplicationReferenceDataSource,
-- Kinesis Data Analytics assigns an ID. You can use the
-- DescribeApplication operation to get the reference ID.
newDeleteApplicationReferenceDataSource ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  -- | 'referenceId'
  Prelude.Text ->
  DeleteApplicationReferenceDataSource
newDeleteApplicationReferenceDataSource
  pApplicationName_
  pCurrentApplicationVersionId_
  pReferenceId_ =
    DeleteApplicationReferenceDataSource'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        referenceId = pReferenceId_
      }

-- | The name of an existing application.
deleteApplicationReferenceDataSource_applicationName :: Lens.Lens' DeleteApplicationReferenceDataSource Prelude.Text
deleteApplicationReferenceDataSource_applicationName = Lens.lens (\DeleteApplicationReferenceDataSource' {applicationName} -> applicationName) (\s@DeleteApplicationReferenceDataSource' {} a -> s {applicationName = a} :: DeleteApplicationReferenceDataSource)

-- | The current application version. You can use the DescribeApplication
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
deleteApplicationReferenceDataSource_currentApplicationVersionId :: Lens.Lens' DeleteApplicationReferenceDataSource Prelude.Natural
deleteApplicationReferenceDataSource_currentApplicationVersionId = Lens.lens (\DeleteApplicationReferenceDataSource' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@DeleteApplicationReferenceDataSource' {} a -> s {currentApplicationVersionId = a} :: DeleteApplicationReferenceDataSource)

-- | The ID of the reference data source. When you add a reference data
-- source to your application using the AddApplicationReferenceDataSource,
-- Kinesis Data Analytics assigns an ID. You can use the
-- DescribeApplication operation to get the reference ID.
deleteApplicationReferenceDataSource_referenceId :: Lens.Lens' DeleteApplicationReferenceDataSource Prelude.Text
deleteApplicationReferenceDataSource_referenceId = Lens.lens (\DeleteApplicationReferenceDataSource' {referenceId} -> referenceId) (\s@DeleteApplicationReferenceDataSource' {} a -> s {referenceId = a} :: DeleteApplicationReferenceDataSource)

instance
  Core.AWSRequest
    DeleteApplicationReferenceDataSource
  where
  type
    AWSResponse DeleteApplicationReferenceDataSource =
      DeleteApplicationReferenceDataSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApplicationReferenceDataSourceResponse'
            Prelude.<$> (x Core..?> "ApplicationARN")
              Prelude.<*> (x Core..?> "ApplicationVersionId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteApplicationReferenceDataSource
  where
  hashWithSalt
    _salt
    DeleteApplicationReferenceDataSource' {..} =
      _salt `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` currentApplicationVersionId
        `Prelude.hashWithSalt` referenceId

instance
  Prelude.NFData
    DeleteApplicationReferenceDataSource
  where
  rnf DeleteApplicationReferenceDataSource' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf referenceId

instance
  Core.ToHeaders
    DeleteApplicationReferenceDataSource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20180523.DeleteApplicationReferenceDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeleteApplicationReferenceDataSource
  where
  toJSON DeleteApplicationReferenceDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Core..= applicationName),
            Prelude.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Prelude.Just ("ReferenceId" Core..= referenceId)
          ]
      )

instance
  Core.ToPath
    DeleteApplicationReferenceDataSource
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteApplicationReferenceDataSource
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationReferenceDataSourceResponse' smart constructor.
data DeleteApplicationReferenceDataSourceResponse = DeleteApplicationReferenceDataSourceResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationARN :: Prelude.Maybe Prelude.Text,
    -- | The updated version ID of the application.
    applicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationReferenceDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationARN', 'deleteApplicationReferenceDataSourceResponse_applicationARN' - The application Amazon Resource Name (ARN).
--
-- 'applicationVersionId', 'deleteApplicationReferenceDataSourceResponse_applicationVersionId' - The updated version ID of the application.
--
-- 'httpStatus', 'deleteApplicationReferenceDataSourceResponse_httpStatus' - The response's http status code.
newDeleteApplicationReferenceDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationReferenceDataSourceResponse
newDeleteApplicationReferenceDataSourceResponse
  pHttpStatus_ =
    DeleteApplicationReferenceDataSourceResponse'
      { applicationARN =
          Prelude.Nothing,
        applicationVersionId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The application Amazon Resource Name (ARN).
deleteApplicationReferenceDataSourceResponse_applicationARN :: Lens.Lens' DeleteApplicationReferenceDataSourceResponse (Prelude.Maybe Prelude.Text)
deleteApplicationReferenceDataSourceResponse_applicationARN = Lens.lens (\DeleteApplicationReferenceDataSourceResponse' {applicationARN} -> applicationARN) (\s@DeleteApplicationReferenceDataSourceResponse' {} a -> s {applicationARN = a} :: DeleteApplicationReferenceDataSourceResponse)

-- | The updated version ID of the application.
deleteApplicationReferenceDataSourceResponse_applicationVersionId :: Lens.Lens' DeleteApplicationReferenceDataSourceResponse (Prelude.Maybe Prelude.Natural)
deleteApplicationReferenceDataSourceResponse_applicationVersionId = Lens.lens (\DeleteApplicationReferenceDataSourceResponse' {applicationVersionId} -> applicationVersionId) (\s@DeleteApplicationReferenceDataSourceResponse' {} a -> s {applicationVersionId = a} :: DeleteApplicationReferenceDataSourceResponse)

-- | The response's http status code.
deleteApplicationReferenceDataSourceResponse_httpStatus :: Lens.Lens' DeleteApplicationReferenceDataSourceResponse Prelude.Int
deleteApplicationReferenceDataSourceResponse_httpStatus = Lens.lens (\DeleteApplicationReferenceDataSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationReferenceDataSourceResponse' {} a -> s {httpStatus = a} :: DeleteApplicationReferenceDataSourceResponse)

instance
  Prelude.NFData
    DeleteApplicationReferenceDataSourceResponse
  where
  rnf DeleteApplicationReferenceDataSourceResponse' {..} =
    Prelude.rnf applicationARN
      `Prelude.seq` Prelude.rnf applicationVersionId
      `Prelude.seq` Prelude.rnf httpStatus
