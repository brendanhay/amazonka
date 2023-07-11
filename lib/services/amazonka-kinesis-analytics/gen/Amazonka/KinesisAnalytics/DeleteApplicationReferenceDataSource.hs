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
-- Module      : Amazonka.KinesisAnalytics.DeleteApplicationReferenceDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This documentation is for version 1 of the Amazon Kinesis Data Analytics
-- API, which only supports SQL applications. Version 2 of the API supports
-- SQL and Java applications. For more information about version 2, see
-- </kinesisanalytics/latest/apiv2/Welcome.html Amazon Kinesis Data Analytics API V2 Documentation>.
--
-- Deletes a reference data source configuration from the specified
-- application configuration.
--
-- If the application is running, Amazon Kinesis Analytics immediately
-- removes the in-application table that you created using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource>
-- operation.
--
-- This operation requires permissions to perform the
-- @kinesisanalytics.DeleteApplicationReferenceDataSource@ action.
module Amazonka.KinesisAnalytics.DeleteApplicationReferenceDataSource
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
    deleteApplicationReferenceDataSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplicationReferenceDataSource' smart constructor.
data DeleteApplicationReferenceDataSource = DeleteApplicationReferenceDataSource'
  { -- | Name of an existing application.
    applicationName :: Prelude.Text,
    -- | Version of the application. You can use the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation to get the current application version. If the version
    -- specified is not the current version, the
    -- @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Prelude.Natural,
    -- | ID of the reference data source. When you add a reference data source to
    -- your application using the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource>,
    -- Amazon Kinesis Analytics assigns an ID. You can use the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation to get the reference ID.
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
-- 'applicationName', 'deleteApplicationReferenceDataSource_applicationName' - Name of an existing application.
--
-- 'currentApplicationVersionId', 'deleteApplicationReferenceDataSource_currentApplicationVersionId' - Version of the application. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
--
-- 'referenceId', 'deleteApplicationReferenceDataSource_referenceId' - ID of the reference data source. When you add a reference data source to
-- your application using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource>,
-- Amazon Kinesis Analytics assigns an ID. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the reference ID.
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

-- | Name of an existing application.
deleteApplicationReferenceDataSource_applicationName :: Lens.Lens' DeleteApplicationReferenceDataSource Prelude.Text
deleteApplicationReferenceDataSource_applicationName = Lens.lens (\DeleteApplicationReferenceDataSource' {applicationName} -> applicationName) (\s@DeleteApplicationReferenceDataSource' {} a -> s {applicationName = a} :: DeleteApplicationReferenceDataSource)

-- | Version of the application. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
deleteApplicationReferenceDataSource_currentApplicationVersionId :: Lens.Lens' DeleteApplicationReferenceDataSource Prelude.Natural
deleteApplicationReferenceDataSource_currentApplicationVersionId = Lens.lens (\DeleteApplicationReferenceDataSource' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@DeleteApplicationReferenceDataSource' {} a -> s {currentApplicationVersionId = a} :: DeleteApplicationReferenceDataSource)

-- | ID of the reference data source. When you add a reference data source to
-- your application using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource>,
-- Amazon Kinesis Analytics assigns an ID. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the reference ID.
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
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationReferenceDataSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteApplicationReferenceDataSource
  where
  hashWithSalt
    _salt
    DeleteApplicationReferenceDataSource' {..} =
      _salt
        `Prelude.hashWithSalt` applicationName
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
  Data.ToHeaders
    DeleteApplicationReferenceDataSource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20150814.DeleteApplicationReferenceDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DeleteApplicationReferenceDataSource
  where
  toJSON DeleteApplicationReferenceDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ( "CurrentApplicationVersionId"
                  Data..= currentApplicationVersionId
              ),
            Prelude.Just ("ReferenceId" Data..= referenceId)
          ]
      )

instance
  Data.ToPath
    DeleteApplicationReferenceDataSource
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteApplicationReferenceDataSource
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationReferenceDataSourceResponse' smart constructor.
data DeleteApplicationReferenceDataSourceResponse = DeleteApplicationReferenceDataSourceResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'deleteApplicationReferenceDataSourceResponse_httpStatus' - The response's http status code.
newDeleteApplicationReferenceDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationReferenceDataSourceResponse
newDeleteApplicationReferenceDataSourceResponse
  pHttpStatus_ =
    DeleteApplicationReferenceDataSourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteApplicationReferenceDataSourceResponse_httpStatus :: Lens.Lens' DeleteApplicationReferenceDataSourceResponse Prelude.Int
deleteApplicationReferenceDataSourceResponse_httpStatus = Lens.lens (\DeleteApplicationReferenceDataSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationReferenceDataSourceResponse' {} a -> s {httpStatus = a} :: DeleteApplicationReferenceDataSourceResponse)

instance
  Prelude.NFData
    DeleteApplicationReferenceDataSourceResponse
  where
  rnf DeleteApplicationReferenceDataSourceResponse' {..} =
    Prelude.rnf httpStatus
