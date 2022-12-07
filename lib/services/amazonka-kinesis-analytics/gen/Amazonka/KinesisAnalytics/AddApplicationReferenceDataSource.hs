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
-- Module      : Amazonka.KinesisAnalytics.AddApplicationReferenceDataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Adds a reference data source to an existing application.
--
-- Amazon Kinesis Analytics reads reference data (that is, an Amazon S3
-- object) and creates an in-application table within your application. In
-- the request, you provide the source (S3 bucket name and object key
-- name), name of the in-application table to create, and the necessary
-- mapping information that describes how data in Amazon S3 object maps to
-- columns in the resulting in-application table.
--
-- For conceptual information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
-- For the limits on data sources you can add to your application, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits>.
--
-- This operation requires permissions to perform the
-- @kinesisanalytics:AddApplicationOutput@ action.
module Amazonka.KinesisAnalytics.AddApplicationReferenceDataSource
  ( -- * Creating a Request
    AddApplicationReferenceDataSource (..),
    newAddApplicationReferenceDataSource,

    -- * Request Lenses
    addApplicationReferenceDataSource_applicationName,
    addApplicationReferenceDataSource_currentApplicationVersionId,
    addApplicationReferenceDataSource_referenceDataSource,

    -- * Destructuring the Response
    AddApplicationReferenceDataSourceResponse (..),
    newAddApplicationReferenceDataSourceResponse,

    -- * Response Lenses
    addApplicationReferenceDataSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newAddApplicationReferenceDataSource' smart constructor.
data AddApplicationReferenceDataSource = AddApplicationReferenceDataSource'
  { -- | Name of an existing application.
    applicationName :: Prelude.Text,
    -- | Version of the application for which you are adding the reference data
    -- source. You can use the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation to get the current application version. If the version
    -- specified is not the current version, the
    -- @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Prelude.Natural,
    -- | The reference data source can be an object in your Amazon S3 bucket.
    -- Amazon Kinesis Analytics reads the object and copies the data into the
    -- in-application table that is created. You provide an S3 bucket, object
    -- key name, and the resulting in-application table that is created. You
    -- must also provide an IAM role with the necessary permissions that Amazon
    -- Kinesis Analytics can assume to read the object from your S3 bucket on
    -- your behalf.
    referenceDataSource :: ReferenceDataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationReferenceDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'addApplicationReferenceDataSource_applicationName' - Name of an existing application.
--
-- 'currentApplicationVersionId', 'addApplicationReferenceDataSource_currentApplicationVersionId' - Version of the application for which you are adding the reference data
-- source. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
--
-- 'referenceDataSource', 'addApplicationReferenceDataSource_referenceDataSource' - The reference data source can be an object in your Amazon S3 bucket.
-- Amazon Kinesis Analytics reads the object and copies the data into the
-- in-application table that is created. You provide an S3 bucket, object
-- key name, and the resulting in-application table that is created. You
-- must also provide an IAM role with the necessary permissions that Amazon
-- Kinesis Analytics can assume to read the object from your S3 bucket on
-- your behalf.
newAddApplicationReferenceDataSource ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  -- | 'referenceDataSource'
  ReferenceDataSource ->
  AddApplicationReferenceDataSource
newAddApplicationReferenceDataSource
  pApplicationName_
  pCurrentApplicationVersionId_
  pReferenceDataSource_ =
    AddApplicationReferenceDataSource'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        referenceDataSource =
          pReferenceDataSource_
      }

-- | Name of an existing application.
addApplicationReferenceDataSource_applicationName :: Lens.Lens' AddApplicationReferenceDataSource Prelude.Text
addApplicationReferenceDataSource_applicationName = Lens.lens (\AddApplicationReferenceDataSource' {applicationName} -> applicationName) (\s@AddApplicationReferenceDataSource' {} a -> s {applicationName = a} :: AddApplicationReferenceDataSource)

-- | Version of the application for which you are adding the reference data
-- source. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
addApplicationReferenceDataSource_currentApplicationVersionId :: Lens.Lens' AddApplicationReferenceDataSource Prelude.Natural
addApplicationReferenceDataSource_currentApplicationVersionId = Lens.lens (\AddApplicationReferenceDataSource' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@AddApplicationReferenceDataSource' {} a -> s {currentApplicationVersionId = a} :: AddApplicationReferenceDataSource)

-- | The reference data source can be an object in your Amazon S3 bucket.
-- Amazon Kinesis Analytics reads the object and copies the data into the
-- in-application table that is created. You provide an S3 bucket, object
-- key name, and the resulting in-application table that is created. You
-- must also provide an IAM role with the necessary permissions that Amazon
-- Kinesis Analytics can assume to read the object from your S3 bucket on
-- your behalf.
addApplicationReferenceDataSource_referenceDataSource :: Lens.Lens' AddApplicationReferenceDataSource ReferenceDataSource
addApplicationReferenceDataSource_referenceDataSource = Lens.lens (\AddApplicationReferenceDataSource' {referenceDataSource} -> referenceDataSource) (\s@AddApplicationReferenceDataSource' {} a -> s {referenceDataSource = a} :: AddApplicationReferenceDataSource)

instance
  Core.AWSRequest
    AddApplicationReferenceDataSource
  where
  type
    AWSResponse AddApplicationReferenceDataSource =
      AddApplicationReferenceDataSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddApplicationReferenceDataSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AddApplicationReferenceDataSource
  where
  hashWithSalt
    _salt
    AddApplicationReferenceDataSource' {..} =
      _salt `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` currentApplicationVersionId
        `Prelude.hashWithSalt` referenceDataSource

instance
  Prelude.NFData
    AddApplicationReferenceDataSource
  where
  rnf AddApplicationReferenceDataSource' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf referenceDataSource

instance
  Data.ToHeaders
    AddApplicationReferenceDataSource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20150814.AddApplicationReferenceDataSource" ::
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
    AddApplicationReferenceDataSource
  where
  toJSON AddApplicationReferenceDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ( "CurrentApplicationVersionId"
                  Data..= currentApplicationVersionId
              ),
            Prelude.Just
              ("ReferenceDataSource" Data..= referenceDataSource)
          ]
      )

instance
  Data.ToPath
    AddApplicationReferenceDataSource
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AddApplicationReferenceDataSource
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newAddApplicationReferenceDataSourceResponse' smart constructor.
data AddApplicationReferenceDataSourceResponse = AddApplicationReferenceDataSourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationReferenceDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addApplicationReferenceDataSourceResponse_httpStatus' - The response's http status code.
newAddApplicationReferenceDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddApplicationReferenceDataSourceResponse
newAddApplicationReferenceDataSourceResponse
  pHttpStatus_ =
    AddApplicationReferenceDataSourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
addApplicationReferenceDataSourceResponse_httpStatus :: Lens.Lens' AddApplicationReferenceDataSourceResponse Prelude.Int
addApplicationReferenceDataSourceResponse_httpStatus = Lens.lens (\AddApplicationReferenceDataSourceResponse' {httpStatus} -> httpStatus) (\s@AddApplicationReferenceDataSourceResponse' {} a -> s {httpStatus = a} :: AddApplicationReferenceDataSourceResponse)

instance
  Prelude.NFData
    AddApplicationReferenceDataSourceResponse
  where
  rnf AddApplicationReferenceDataSourceResponse' {..} =
    Prelude.rnf httpStatus
