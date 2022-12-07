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
-- Module      : Amazonka.KinesisAnalyticsV2.AddApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an Amazon CloudWatch log stream to monitor application
-- configuration errors.
module Amazonka.KinesisAnalyticsV2.AddApplicationCloudWatchLoggingOption
  ( -- * Creating a Request
    AddApplicationCloudWatchLoggingOption (..),
    newAddApplicationCloudWatchLoggingOption,

    -- * Request Lenses
    addApplicationCloudWatchLoggingOption_conditionalToken,
    addApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    addApplicationCloudWatchLoggingOption_applicationName,
    addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption,

    -- * Destructuring the Response
    AddApplicationCloudWatchLoggingOptionResponse (..),
    newAddApplicationCloudWatchLoggingOptionResponse,

    -- * Response Lenses
    addApplicationCloudWatchLoggingOptionResponse_applicationARN,
    addApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions,
    addApplicationCloudWatchLoggingOptionResponse_applicationVersionId,
    addApplicationCloudWatchLoggingOptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddApplicationCloudWatchLoggingOption' smart constructor.
data AddApplicationCloudWatchLoggingOption = AddApplicationCloudWatchLoggingOption'
  { -- | A value you use to implement strong concurrency for application updates.
    -- You must provide the @CurrentApplicationVersionId@ or the
    -- @ConditionalToken@. You get the application\'s current
    -- @ConditionalToken@ using DescribeApplication. For better concurrency
    -- support, use the @ConditionalToken@ parameter instead of
    -- @CurrentApplicationVersionId@.
    conditionalToken :: Prelude.Maybe Prelude.Text,
    -- | The version ID of the Kinesis Data Analytics application. You must
    -- provide the @CurrentApplicationVersionId@ or the @ConditionalToken@.You
    -- can retrieve the application version ID using DescribeApplication. For
    -- better concurrency support, use the @ConditionalToken@ parameter instead
    -- of @CurrentApplicationVersionId@.
    currentApplicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The Kinesis Data Analytics application name.
    applicationName :: Prelude.Text,
    -- | Provides the Amazon CloudWatch log stream Amazon Resource Name (ARN).
    cloudWatchLoggingOption :: CloudWatchLoggingOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationCloudWatchLoggingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalToken', 'addApplicationCloudWatchLoggingOption_conditionalToken' - A value you use to implement strong concurrency for application updates.
-- You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You get the application\'s current
-- @ConditionalToken@ using DescribeApplication. For better concurrency
-- support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
--
-- 'currentApplicationVersionId', 'addApplicationCloudWatchLoggingOption_currentApplicationVersionId' - The version ID of the Kinesis Data Analytics application. You must
-- provide the @CurrentApplicationVersionId@ or the @ConditionalToken@.You
-- can retrieve the application version ID using DescribeApplication. For
-- better concurrency support, use the @ConditionalToken@ parameter instead
-- of @CurrentApplicationVersionId@.
--
-- 'applicationName', 'addApplicationCloudWatchLoggingOption_applicationName' - The Kinesis Data Analytics application name.
--
-- 'cloudWatchLoggingOption', 'addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption' - Provides the Amazon CloudWatch log stream Amazon Resource Name (ARN).
newAddApplicationCloudWatchLoggingOption ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'cloudWatchLoggingOption'
  CloudWatchLoggingOption ->
  AddApplicationCloudWatchLoggingOption
newAddApplicationCloudWatchLoggingOption
  pApplicationName_
  pCloudWatchLoggingOption_ =
    AddApplicationCloudWatchLoggingOption'
      { conditionalToken =
          Prelude.Nothing,
        currentApplicationVersionId =
          Prelude.Nothing,
        applicationName = pApplicationName_,
        cloudWatchLoggingOption =
          pCloudWatchLoggingOption_
      }

-- | A value you use to implement strong concurrency for application updates.
-- You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You get the application\'s current
-- @ConditionalToken@ using DescribeApplication. For better concurrency
-- support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
addApplicationCloudWatchLoggingOption_conditionalToken :: Lens.Lens' AddApplicationCloudWatchLoggingOption (Prelude.Maybe Prelude.Text)
addApplicationCloudWatchLoggingOption_conditionalToken = Lens.lens (\AddApplicationCloudWatchLoggingOption' {conditionalToken} -> conditionalToken) (\s@AddApplicationCloudWatchLoggingOption' {} a -> s {conditionalToken = a} :: AddApplicationCloudWatchLoggingOption)

-- | The version ID of the Kinesis Data Analytics application. You must
-- provide the @CurrentApplicationVersionId@ or the @ConditionalToken@.You
-- can retrieve the application version ID using DescribeApplication. For
-- better concurrency support, use the @ConditionalToken@ parameter instead
-- of @CurrentApplicationVersionId@.
addApplicationCloudWatchLoggingOption_currentApplicationVersionId :: Lens.Lens' AddApplicationCloudWatchLoggingOption (Prelude.Maybe Prelude.Natural)
addApplicationCloudWatchLoggingOption_currentApplicationVersionId = Lens.lens (\AddApplicationCloudWatchLoggingOption' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@AddApplicationCloudWatchLoggingOption' {} a -> s {currentApplicationVersionId = a} :: AddApplicationCloudWatchLoggingOption)

-- | The Kinesis Data Analytics application name.
addApplicationCloudWatchLoggingOption_applicationName :: Lens.Lens' AddApplicationCloudWatchLoggingOption Prelude.Text
addApplicationCloudWatchLoggingOption_applicationName = Lens.lens (\AddApplicationCloudWatchLoggingOption' {applicationName} -> applicationName) (\s@AddApplicationCloudWatchLoggingOption' {} a -> s {applicationName = a} :: AddApplicationCloudWatchLoggingOption)

-- | Provides the Amazon CloudWatch log stream Amazon Resource Name (ARN).
addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption :: Lens.Lens' AddApplicationCloudWatchLoggingOption CloudWatchLoggingOption
addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption = Lens.lens (\AddApplicationCloudWatchLoggingOption' {cloudWatchLoggingOption} -> cloudWatchLoggingOption) (\s@AddApplicationCloudWatchLoggingOption' {} a -> s {cloudWatchLoggingOption = a} :: AddApplicationCloudWatchLoggingOption)

instance
  Core.AWSRequest
    AddApplicationCloudWatchLoggingOption
  where
  type
    AWSResponse
      AddApplicationCloudWatchLoggingOption =
      AddApplicationCloudWatchLoggingOptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddApplicationCloudWatchLoggingOptionResponse'
            Prelude.<$> (x Data..?> "ApplicationARN")
              Prelude.<*> ( x Data..?> "CloudWatchLoggingOptionDescriptions"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (x Data..?> "ApplicationVersionId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AddApplicationCloudWatchLoggingOption
  where
  hashWithSalt
    _salt
    AddApplicationCloudWatchLoggingOption' {..} =
      _salt `Prelude.hashWithSalt` conditionalToken
        `Prelude.hashWithSalt` currentApplicationVersionId
        `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` cloudWatchLoggingOption

instance
  Prelude.NFData
    AddApplicationCloudWatchLoggingOption
  where
  rnf AddApplicationCloudWatchLoggingOption' {..} =
    Prelude.rnf conditionalToken
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOption

instance
  Data.ToHeaders
    AddApplicationCloudWatchLoggingOption
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.AddApplicationCloudWatchLoggingOption" ::
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
    AddApplicationCloudWatchLoggingOption
  where
  toJSON AddApplicationCloudWatchLoggingOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionalToken" Data..=)
              Prelude.<$> conditionalToken,
            ("CurrentApplicationVersionId" Data..=)
              Prelude.<$> currentApplicationVersionId,
            Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ( "CloudWatchLoggingOption"
                  Data..= cloudWatchLoggingOption
              )
          ]
      )

instance
  Data.ToPath
    AddApplicationCloudWatchLoggingOption
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AddApplicationCloudWatchLoggingOption
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddApplicationCloudWatchLoggingOptionResponse' smart constructor.
data AddApplicationCloudWatchLoggingOptionResponse = AddApplicationCloudWatchLoggingOptionResponse'
  { -- | The application\'s ARN.
    applicationARN :: Prelude.Maybe Prelude.Text,
    -- | The descriptions of the current CloudWatch logging options for the
    -- Kinesis Data Analytics application.
    cloudWatchLoggingOptionDescriptions :: Prelude.Maybe [CloudWatchLoggingOptionDescription],
    -- | The new version ID of the Kinesis Data Analytics application. Kinesis
    -- Data Analytics updates the @ApplicationVersionId@ each time you change
    -- the CloudWatch logging options.
    applicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationCloudWatchLoggingOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationARN', 'addApplicationCloudWatchLoggingOptionResponse_applicationARN' - The application\'s ARN.
--
-- 'cloudWatchLoggingOptionDescriptions', 'addApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions' - The descriptions of the current CloudWatch logging options for the
-- Kinesis Data Analytics application.
--
-- 'applicationVersionId', 'addApplicationCloudWatchLoggingOptionResponse_applicationVersionId' - The new version ID of the Kinesis Data Analytics application. Kinesis
-- Data Analytics updates the @ApplicationVersionId@ each time you change
-- the CloudWatch logging options.
--
-- 'httpStatus', 'addApplicationCloudWatchLoggingOptionResponse_httpStatus' - The response's http status code.
newAddApplicationCloudWatchLoggingOptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddApplicationCloudWatchLoggingOptionResponse
newAddApplicationCloudWatchLoggingOptionResponse
  pHttpStatus_ =
    AddApplicationCloudWatchLoggingOptionResponse'
      { applicationARN =
          Prelude.Nothing,
        cloudWatchLoggingOptionDescriptions =
          Prelude.Nothing,
        applicationVersionId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The application\'s ARN.
addApplicationCloudWatchLoggingOptionResponse_applicationARN :: Lens.Lens' AddApplicationCloudWatchLoggingOptionResponse (Prelude.Maybe Prelude.Text)
addApplicationCloudWatchLoggingOptionResponse_applicationARN = Lens.lens (\AddApplicationCloudWatchLoggingOptionResponse' {applicationARN} -> applicationARN) (\s@AddApplicationCloudWatchLoggingOptionResponse' {} a -> s {applicationARN = a} :: AddApplicationCloudWatchLoggingOptionResponse)

-- | The descriptions of the current CloudWatch logging options for the
-- Kinesis Data Analytics application.
addApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions :: Lens.Lens' AddApplicationCloudWatchLoggingOptionResponse (Prelude.Maybe [CloudWatchLoggingOptionDescription])
addApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions = Lens.lens (\AddApplicationCloudWatchLoggingOptionResponse' {cloudWatchLoggingOptionDescriptions} -> cloudWatchLoggingOptionDescriptions) (\s@AddApplicationCloudWatchLoggingOptionResponse' {} a -> s {cloudWatchLoggingOptionDescriptions = a} :: AddApplicationCloudWatchLoggingOptionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The new version ID of the Kinesis Data Analytics application. Kinesis
-- Data Analytics updates the @ApplicationVersionId@ each time you change
-- the CloudWatch logging options.
addApplicationCloudWatchLoggingOptionResponse_applicationVersionId :: Lens.Lens' AddApplicationCloudWatchLoggingOptionResponse (Prelude.Maybe Prelude.Natural)
addApplicationCloudWatchLoggingOptionResponse_applicationVersionId = Lens.lens (\AddApplicationCloudWatchLoggingOptionResponse' {applicationVersionId} -> applicationVersionId) (\s@AddApplicationCloudWatchLoggingOptionResponse' {} a -> s {applicationVersionId = a} :: AddApplicationCloudWatchLoggingOptionResponse)

-- | The response's http status code.
addApplicationCloudWatchLoggingOptionResponse_httpStatus :: Lens.Lens' AddApplicationCloudWatchLoggingOptionResponse Prelude.Int
addApplicationCloudWatchLoggingOptionResponse_httpStatus = Lens.lens (\AddApplicationCloudWatchLoggingOptionResponse' {httpStatus} -> httpStatus) (\s@AddApplicationCloudWatchLoggingOptionResponse' {} a -> s {httpStatus = a} :: AddApplicationCloudWatchLoggingOptionResponse)

instance
  Prelude.NFData
    AddApplicationCloudWatchLoggingOptionResponse
  where
  rnf
    AddApplicationCloudWatchLoggingOptionResponse' {..} =
      Prelude.rnf applicationARN
        `Prelude.seq` Prelude.rnf cloudWatchLoggingOptionDescriptions
        `Prelude.seq` Prelude.rnf applicationVersionId
        `Prelude.seq` Prelude.rnf httpStatus
