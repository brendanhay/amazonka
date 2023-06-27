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
-- Module      : Amazonka.KinesisAnalyticsV2.DeleteApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon CloudWatch log stream from an Kinesis Data Analytics
-- application.
module Amazonka.KinesisAnalyticsV2.DeleteApplicationCloudWatchLoggingOption
  ( -- * Creating a Request
    DeleteApplicationCloudWatchLoggingOption (..),
    newDeleteApplicationCloudWatchLoggingOption,

    -- * Request Lenses
    deleteApplicationCloudWatchLoggingOption_conditionalToken,
    deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    deleteApplicationCloudWatchLoggingOption_applicationName,
    deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId,

    -- * Destructuring the Response
    DeleteApplicationCloudWatchLoggingOptionResponse (..),
    newDeleteApplicationCloudWatchLoggingOptionResponse,

    -- * Response Lenses
    deleteApplicationCloudWatchLoggingOptionResponse_applicationARN,
    deleteApplicationCloudWatchLoggingOptionResponse_applicationVersionId,
    deleteApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions,
    deleteApplicationCloudWatchLoggingOptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplicationCloudWatchLoggingOption' smart constructor.
data DeleteApplicationCloudWatchLoggingOption = DeleteApplicationCloudWatchLoggingOption'
  { -- | A value you use to implement strong concurrency for application updates.
    -- You must provide the @CurrentApplicationVersionId@ or the
    -- @ConditionalToken@. You get the application\'s current
    -- @ConditionalToken@ using DescribeApplication. For better concurrency
    -- support, use the @ConditionalToken@ parameter instead of
    -- @CurrentApplicationVersionId@.
    conditionalToken :: Prelude.Maybe Prelude.Text,
    -- | The version ID of the application. You must provide the
    -- @CurrentApplicationVersionId@ or the @ConditionalToken@. You can
    -- retrieve the application version ID using DescribeApplication. For
    -- better concurrency support, use the @ConditionalToken@ parameter instead
    -- of @CurrentApplicationVersionId@.
    currentApplicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The application name.
    applicationName :: Prelude.Text,
    -- | The @CloudWatchLoggingOptionId@ of the Amazon CloudWatch logging option
    -- to delete. You can get the @CloudWatchLoggingOptionId@ by using the
    -- DescribeApplication operation.
    cloudWatchLoggingOptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationCloudWatchLoggingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalToken', 'deleteApplicationCloudWatchLoggingOption_conditionalToken' - A value you use to implement strong concurrency for application updates.
-- You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You get the application\'s current
-- @ConditionalToken@ using DescribeApplication. For better concurrency
-- support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
--
-- 'currentApplicationVersionId', 'deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId' - The version ID of the application. You must provide the
-- @CurrentApplicationVersionId@ or the @ConditionalToken@. You can
-- retrieve the application version ID using DescribeApplication. For
-- better concurrency support, use the @ConditionalToken@ parameter instead
-- of @CurrentApplicationVersionId@.
--
-- 'applicationName', 'deleteApplicationCloudWatchLoggingOption_applicationName' - The application name.
--
-- 'cloudWatchLoggingOptionId', 'deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId' - The @CloudWatchLoggingOptionId@ of the Amazon CloudWatch logging option
-- to delete. You can get the @CloudWatchLoggingOptionId@ by using the
-- DescribeApplication operation.
newDeleteApplicationCloudWatchLoggingOption ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'cloudWatchLoggingOptionId'
  Prelude.Text ->
  DeleteApplicationCloudWatchLoggingOption
newDeleteApplicationCloudWatchLoggingOption
  pApplicationName_
  pCloudWatchLoggingOptionId_ =
    DeleteApplicationCloudWatchLoggingOption'
      { conditionalToken =
          Prelude.Nothing,
        currentApplicationVersionId =
          Prelude.Nothing,
        applicationName =
          pApplicationName_,
        cloudWatchLoggingOptionId =
          pCloudWatchLoggingOptionId_
      }

-- | A value you use to implement strong concurrency for application updates.
-- You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You get the application\'s current
-- @ConditionalToken@ using DescribeApplication. For better concurrency
-- support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
deleteApplicationCloudWatchLoggingOption_conditionalToken :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption (Prelude.Maybe Prelude.Text)
deleteApplicationCloudWatchLoggingOption_conditionalToken = Lens.lens (\DeleteApplicationCloudWatchLoggingOption' {conditionalToken} -> conditionalToken) (\s@DeleteApplicationCloudWatchLoggingOption' {} a -> s {conditionalToken = a} :: DeleteApplicationCloudWatchLoggingOption)

-- | The version ID of the application. You must provide the
-- @CurrentApplicationVersionId@ or the @ConditionalToken@. You can
-- retrieve the application version ID using DescribeApplication. For
-- better concurrency support, use the @ConditionalToken@ parameter instead
-- of @CurrentApplicationVersionId@.
deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption (Prelude.Maybe Prelude.Natural)
deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId = Lens.lens (\DeleteApplicationCloudWatchLoggingOption' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@DeleteApplicationCloudWatchLoggingOption' {} a -> s {currentApplicationVersionId = a} :: DeleteApplicationCloudWatchLoggingOption)

-- | The application name.
deleteApplicationCloudWatchLoggingOption_applicationName :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Prelude.Text
deleteApplicationCloudWatchLoggingOption_applicationName = Lens.lens (\DeleteApplicationCloudWatchLoggingOption' {applicationName} -> applicationName) (\s@DeleteApplicationCloudWatchLoggingOption' {} a -> s {applicationName = a} :: DeleteApplicationCloudWatchLoggingOption)

-- | The @CloudWatchLoggingOptionId@ of the Amazon CloudWatch logging option
-- to delete. You can get the @CloudWatchLoggingOptionId@ by using the
-- DescribeApplication operation.
deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Prelude.Text
deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId = Lens.lens (\DeleteApplicationCloudWatchLoggingOption' {cloudWatchLoggingOptionId} -> cloudWatchLoggingOptionId) (\s@DeleteApplicationCloudWatchLoggingOption' {} a -> s {cloudWatchLoggingOptionId = a} :: DeleteApplicationCloudWatchLoggingOption)

instance
  Core.AWSRequest
    DeleteApplicationCloudWatchLoggingOption
  where
  type
    AWSResponse
      DeleteApplicationCloudWatchLoggingOption =
      DeleteApplicationCloudWatchLoggingOptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApplicationCloudWatchLoggingOptionResponse'
            Prelude.<$> (x Data..?> "ApplicationARN")
            Prelude.<*> (x Data..?> "ApplicationVersionId")
            Prelude.<*> ( x
                            Data..?> "CloudWatchLoggingOptionDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteApplicationCloudWatchLoggingOption
  where
  hashWithSalt
    _salt
    DeleteApplicationCloudWatchLoggingOption' {..} =
      _salt
        `Prelude.hashWithSalt` conditionalToken
        `Prelude.hashWithSalt` currentApplicationVersionId
        `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` cloudWatchLoggingOptionId

instance
  Prelude.NFData
    DeleteApplicationCloudWatchLoggingOption
  where
  rnf DeleteApplicationCloudWatchLoggingOption' {..} =
    Prelude.rnf conditionalToken
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptionId

instance
  Data.ToHeaders
    DeleteApplicationCloudWatchLoggingOption
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.DeleteApplicationCloudWatchLoggingOption" ::
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
    DeleteApplicationCloudWatchLoggingOption
  where
  toJSON DeleteApplicationCloudWatchLoggingOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionalToken" Data..=)
              Prelude.<$> conditionalToken,
            ("CurrentApplicationVersionId" Data..=)
              Prelude.<$> currentApplicationVersionId,
            Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ( "CloudWatchLoggingOptionId"
                  Data..= cloudWatchLoggingOptionId
              )
          ]
      )

instance
  Data.ToPath
    DeleteApplicationCloudWatchLoggingOption
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteApplicationCloudWatchLoggingOption
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationCloudWatchLoggingOptionResponse' smart constructor.
data DeleteApplicationCloudWatchLoggingOptionResponse = DeleteApplicationCloudWatchLoggingOptionResponse'
  { -- | The application\'s Amazon Resource Name (ARN).
    applicationARN :: Prelude.Maybe Prelude.Text,
    -- | The version ID of the application. Kinesis Data Analytics updates the
    -- @ApplicationVersionId@ each time you change the CloudWatch logging
    -- options.
    applicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The descriptions of the remaining CloudWatch logging options for the
    -- application.
    cloudWatchLoggingOptionDescriptions :: Prelude.Maybe [CloudWatchLoggingOptionDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationCloudWatchLoggingOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationARN', 'deleteApplicationCloudWatchLoggingOptionResponse_applicationARN' - The application\'s Amazon Resource Name (ARN).
--
-- 'applicationVersionId', 'deleteApplicationCloudWatchLoggingOptionResponse_applicationVersionId' - The version ID of the application. Kinesis Data Analytics updates the
-- @ApplicationVersionId@ each time you change the CloudWatch logging
-- options.
--
-- 'cloudWatchLoggingOptionDescriptions', 'deleteApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions' - The descriptions of the remaining CloudWatch logging options for the
-- application.
--
-- 'httpStatus', 'deleteApplicationCloudWatchLoggingOptionResponse_httpStatus' - The response's http status code.
newDeleteApplicationCloudWatchLoggingOptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationCloudWatchLoggingOptionResponse
newDeleteApplicationCloudWatchLoggingOptionResponse
  pHttpStatus_ =
    DeleteApplicationCloudWatchLoggingOptionResponse'
      { applicationARN =
          Prelude.Nothing,
        applicationVersionId =
          Prelude.Nothing,
        cloudWatchLoggingOptionDescriptions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The application\'s Amazon Resource Name (ARN).
deleteApplicationCloudWatchLoggingOptionResponse_applicationARN :: Lens.Lens' DeleteApplicationCloudWatchLoggingOptionResponse (Prelude.Maybe Prelude.Text)
deleteApplicationCloudWatchLoggingOptionResponse_applicationARN = Lens.lens (\DeleteApplicationCloudWatchLoggingOptionResponse' {applicationARN} -> applicationARN) (\s@DeleteApplicationCloudWatchLoggingOptionResponse' {} a -> s {applicationARN = a} :: DeleteApplicationCloudWatchLoggingOptionResponse)

-- | The version ID of the application. Kinesis Data Analytics updates the
-- @ApplicationVersionId@ each time you change the CloudWatch logging
-- options.
deleteApplicationCloudWatchLoggingOptionResponse_applicationVersionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOptionResponse (Prelude.Maybe Prelude.Natural)
deleteApplicationCloudWatchLoggingOptionResponse_applicationVersionId = Lens.lens (\DeleteApplicationCloudWatchLoggingOptionResponse' {applicationVersionId} -> applicationVersionId) (\s@DeleteApplicationCloudWatchLoggingOptionResponse' {} a -> s {applicationVersionId = a} :: DeleteApplicationCloudWatchLoggingOptionResponse)

-- | The descriptions of the remaining CloudWatch logging options for the
-- application.
deleteApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions :: Lens.Lens' DeleteApplicationCloudWatchLoggingOptionResponse (Prelude.Maybe [CloudWatchLoggingOptionDescription])
deleteApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions = Lens.lens (\DeleteApplicationCloudWatchLoggingOptionResponse' {cloudWatchLoggingOptionDescriptions} -> cloudWatchLoggingOptionDescriptions) (\s@DeleteApplicationCloudWatchLoggingOptionResponse' {} a -> s {cloudWatchLoggingOptionDescriptions = a} :: DeleteApplicationCloudWatchLoggingOptionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteApplicationCloudWatchLoggingOptionResponse_httpStatus :: Lens.Lens' DeleteApplicationCloudWatchLoggingOptionResponse Prelude.Int
deleteApplicationCloudWatchLoggingOptionResponse_httpStatus = Lens.lens (\DeleteApplicationCloudWatchLoggingOptionResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationCloudWatchLoggingOptionResponse' {} a -> s {httpStatus = a} :: DeleteApplicationCloudWatchLoggingOptionResponse)

instance
  Prelude.NFData
    DeleteApplicationCloudWatchLoggingOptionResponse
  where
  rnf
    DeleteApplicationCloudWatchLoggingOptionResponse' {..} =
      Prelude.rnf applicationARN
        `Prelude.seq` Prelude.rnf applicationVersionId
        `Prelude.seq` Prelude.rnf cloudWatchLoggingOptionDescriptions
        `Prelude.seq` Prelude.rnf httpStatus
