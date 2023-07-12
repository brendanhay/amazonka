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
-- Module      : Amazonka.KinesisAnalyticsV2.UpdateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Kinesis Data Analytics application. Using this
-- operation, you can update application code, input configuration, and
-- output configuration.
--
-- Kinesis Data Analytics updates the @ApplicationVersionId@ each time you
-- update your application.
--
-- You cannot update the @RuntimeEnvironment@ of an existing application.
-- If you need to update an application\'s @RuntimeEnvironment@, you must
-- delete the application and create it again.
module Amazonka.KinesisAnalyticsV2.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_applicationConfigurationUpdate,
    updateApplication_cloudWatchLoggingOptionUpdates,
    updateApplication_conditionalToken,
    updateApplication_currentApplicationVersionId,
    updateApplication_runConfigurationUpdate,
    updateApplication_serviceExecutionRoleUpdate,
    updateApplication_applicationName,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_httpStatus,
    updateApplicationResponse_applicationDetail,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | Describes application configuration updates.
    applicationConfigurationUpdate :: Prelude.Maybe ApplicationConfigurationUpdate,
    -- | Describes application Amazon CloudWatch logging option updates. You can
    -- only update existing CloudWatch logging options with this action. To add
    -- a new CloudWatch logging option, use
    -- AddApplicationCloudWatchLoggingOption.
    cloudWatchLoggingOptionUpdates :: Prelude.Maybe [CloudWatchLoggingOptionUpdate],
    -- | A value you use to implement strong concurrency for application updates.
    -- You must provide the @CurrentApplicationVersionId@ or the
    -- @ConditionalToken@. You get the application\'s current
    -- @ConditionalToken@ using DescribeApplication. For better concurrency
    -- support, use the @ConditionalToken@ parameter instead of
    -- @CurrentApplicationVersionId@.
    conditionalToken :: Prelude.Maybe Prelude.Text,
    -- | The current application version ID. You must provide the
    -- @CurrentApplicationVersionId@ or the @ConditionalToken@.You can retrieve
    -- the application version ID using DescribeApplication. For better
    -- concurrency support, use the @ConditionalToken@ parameter instead of
    -- @CurrentApplicationVersionId@.
    currentApplicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | Describes updates to the application\'s starting parameters.
    runConfigurationUpdate :: Prelude.Maybe RunConfigurationUpdate,
    -- | Describes updates to the service execution role.
    serviceExecutionRoleUpdate :: Prelude.Maybe Prelude.Text,
    -- | The name of the application to update.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationConfigurationUpdate', 'updateApplication_applicationConfigurationUpdate' - Describes application configuration updates.
--
-- 'cloudWatchLoggingOptionUpdates', 'updateApplication_cloudWatchLoggingOptionUpdates' - Describes application Amazon CloudWatch logging option updates. You can
-- only update existing CloudWatch logging options with this action. To add
-- a new CloudWatch logging option, use
-- AddApplicationCloudWatchLoggingOption.
--
-- 'conditionalToken', 'updateApplication_conditionalToken' - A value you use to implement strong concurrency for application updates.
-- You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You get the application\'s current
-- @ConditionalToken@ using DescribeApplication. For better concurrency
-- support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
--
-- 'currentApplicationVersionId', 'updateApplication_currentApplicationVersionId' - The current application version ID. You must provide the
-- @CurrentApplicationVersionId@ or the @ConditionalToken@.You can retrieve
-- the application version ID using DescribeApplication. For better
-- concurrency support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
--
-- 'runConfigurationUpdate', 'updateApplication_runConfigurationUpdate' - Describes updates to the application\'s starting parameters.
--
-- 'serviceExecutionRoleUpdate', 'updateApplication_serviceExecutionRoleUpdate' - Describes updates to the service execution role.
--
-- 'applicationName', 'updateApplication_applicationName' - The name of the application to update.
newUpdateApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  UpdateApplication
newUpdateApplication pApplicationName_ =
  UpdateApplication'
    { applicationConfigurationUpdate =
        Prelude.Nothing,
      cloudWatchLoggingOptionUpdates = Prelude.Nothing,
      conditionalToken = Prelude.Nothing,
      currentApplicationVersionId = Prelude.Nothing,
      runConfigurationUpdate = Prelude.Nothing,
      serviceExecutionRoleUpdate = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | Describes application configuration updates.
updateApplication_applicationConfigurationUpdate :: Lens.Lens' UpdateApplication (Prelude.Maybe ApplicationConfigurationUpdate)
updateApplication_applicationConfigurationUpdate = Lens.lens (\UpdateApplication' {applicationConfigurationUpdate} -> applicationConfigurationUpdate) (\s@UpdateApplication' {} a -> s {applicationConfigurationUpdate = a} :: UpdateApplication)

-- | Describes application Amazon CloudWatch logging option updates. You can
-- only update existing CloudWatch logging options with this action. To add
-- a new CloudWatch logging option, use
-- AddApplicationCloudWatchLoggingOption.
updateApplication_cloudWatchLoggingOptionUpdates :: Lens.Lens' UpdateApplication (Prelude.Maybe [CloudWatchLoggingOptionUpdate])
updateApplication_cloudWatchLoggingOptionUpdates = Lens.lens (\UpdateApplication' {cloudWatchLoggingOptionUpdates} -> cloudWatchLoggingOptionUpdates) (\s@UpdateApplication' {} a -> s {cloudWatchLoggingOptionUpdates = a} :: UpdateApplication) Prelude.. Lens.mapping Lens.coerced

-- | A value you use to implement strong concurrency for application updates.
-- You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You get the application\'s current
-- @ConditionalToken@ using DescribeApplication. For better concurrency
-- support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
updateApplication_conditionalToken :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_conditionalToken = Lens.lens (\UpdateApplication' {conditionalToken} -> conditionalToken) (\s@UpdateApplication' {} a -> s {conditionalToken = a} :: UpdateApplication)

-- | The current application version ID. You must provide the
-- @CurrentApplicationVersionId@ or the @ConditionalToken@.You can retrieve
-- the application version ID using DescribeApplication. For better
-- concurrency support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
updateApplication_currentApplicationVersionId :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Natural)
updateApplication_currentApplicationVersionId = Lens.lens (\UpdateApplication' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@UpdateApplication' {} a -> s {currentApplicationVersionId = a} :: UpdateApplication)

-- | Describes updates to the application\'s starting parameters.
updateApplication_runConfigurationUpdate :: Lens.Lens' UpdateApplication (Prelude.Maybe RunConfigurationUpdate)
updateApplication_runConfigurationUpdate = Lens.lens (\UpdateApplication' {runConfigurationUpdate} -> runConfigurationUpdate) (\s@UpdateApplication' {} a -> s {runConfigurationUpdate = a} :: UpdateApplication)

-- | Describes updates to the service execution role.
updateApplication_serviceExecutionRoleUpdate :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_serviceExecutionRoleUpdate = Lens.lens (\UpdateApplication' {serviceExecutionRoleUpdate} -> serviceExecutionRoleUpdate) (\s@UpdateApplication' {} a -> s {serviceExecutionRoleUpdate = a} :: UpdateApplication)

-- | The name of the application to update.
updateApplication_applicationName :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_applicationName = Lens.lens (\UpdateApplication' {applicationName} -> applicationName) (\s@UpdateApplication' {} a -> s {applicationName = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ApplicationDetail")
      )

instance Prelude.Hashable UpdateApplication where
  hashWithSalt _salt UpdateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` applicationConfigurationUpdate
      `Prelude.hashWithSalt` cloudWatchLoggingOptionUpdates
      `Prelude.hashWithSalt` conditionalToken
      `Prelude.hashWithSalt` currentApplicationVersionId
      `Prelude.hashWithSalt` runConfigurationUpdate
      `Prelude.hashWithSalt` serviceExecutionRoleUpdate
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData UpdateApplication where
  rnf UpdateApplication' {..} =
    Prelude.rnf applicationConfigurationUpdate
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptionUpdates
      `Prelude.seq` Prelude.rnf conditionalToken
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf runConfigurationUpdate
      `Prelude.seq` Prelude.rnf serviceExecutionRoleUpdate
      `Prelude.seq` Prelude.rnf applicationName

instance Data.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.UpdateApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationConfigurationUpdate" Data..=)
              Prelude.<$> applicationConfigurationUpdate,
            ("CloudWatchLoggingOptionUpdates" Data..=)
              Prelude.<$> cloudWatchLoggingOptionUpdates,
            ("ConditionalToken" Data..=)
              Prelude.<$> conditionalToken,
            ("CurrentApplicationVersionId" Data..=)
              Prelude.<$> currentApplicationVersionId,
            ("RunConfigurationUpdate" Data..=)
              Prelude.<$> runConfigurationUpdate,
            ("ServiceExecutionRoleUpdate" Data..=)
              Prelude.<$> serviceExecutionRoleUpdate,
            Prelude.Just
              ("ApplicationName" Data..= applicationName)
          ]
      )

instance Data.ToPath UpdateApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Describes application updates.
    applicationDetail :: ApplicationDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApplicationResponse_httpStatus' - The response's http status code.
--
-- 'applicationDetail', 'updateApplicationResponse_applicationDetail' - Describes application updates.
newUpdateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationDetail'
  ApplicationDetail ->
  UpdateApplicationResponse
newUpdateApplicationResponse
  pHttpStatus_
  pApplicationDetail_ =
    UpdateApplicationResponse'
      { httpStatus =
          pHttpStatus_,
        applicationDetail = pApplicationDetail_
      }

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Prelude.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

-- | Describes application updates.
updateApplicationResponse_applicationDetail :: Lens.Lens' UpdateApplicationResponse ApplicationDetail
updateApplicationResponse_applicationDetail = Lens.lens (\UpdateApplicationResponse' {applicationDetail} -> applicationDetail) (\s@UpdateApplicationResponse' {} a -> s {applicationDetail = a} :: UpdateApplicationResponse)

instance Prelude.NFData UpdateApplicationResponse where
  rnf UpdateApplicationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationDetail
