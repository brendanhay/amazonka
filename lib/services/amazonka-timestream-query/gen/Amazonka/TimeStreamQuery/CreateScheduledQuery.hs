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
-- Module      : Amazonka.TimeStreamQuery.CreateScheduledQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a scheduled query that will be run on your behalf at the
-- configured schedule. Timestream assumes the execution role provided as
-- part of the @ScheduledQueryExecutionRoleArn@ parameter to run the query.
-- You can use the @NotificationConfiguration@ parameter to configure
-- notification for your scheduled query operations.
module Amazonka.TimeStreamQuery.CreateScheduledQuery
  ( -- * Creating a Request
    CreateScheduledQuery (..),
    newCreateScheduledQuery,

    -- * Request Lenses
    createScheduledQuery_clientToken,
    createScheduledQuery_kmsKeyId,
    createScheduledQuery_tags,
    createScheduledQuery_targetConfiguration,
    createScheduledQuery_name,
    createScheduledQuery_queryString,
    createScheduledQuery_scheduleConfiguration,
    createScheduledQuery_notificationConfiguration,
    createScheduledQuery_scheduledQueryExecutionRoleArn,
    createScheduledQuery_errorReportConfiguration,

    -- * Destructuring the Response
    CreateScheduledQueryResponse (..),
    newCreateScheduledQueryResponse,

    -- * Response Lenses
    createScheduledQueryResponse_httpStatus,
    createScheduledQueryResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newCreateScheduledQuery' smart constructor.
data CreateScheduledQuery = CreateScheduledQuery'
  { -- | Using a ClientToken makes the call to CreateScheduledQuery idempotent,
    -- in other words, making the same request repeatedly will produce the same
    -- result. Making multiple identical CreateScheduledQuery requests has the
    -- same effect as making a single request.
    --
    -- -   If CreateScheduledQuery is called without a @ClientToken@, the Query
    --     SDK generates a @ClientToken@ on your behalf.
    --
    -- -   After 8 hours, any request with the same @ClientToken@ is treated as
    --     a new request.
    clientToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon KMS key used to encrypt the scheduled query resource,
    -- at-rest. If the Amazon KMS key is not specified, the scheduled query
    -- resource will be encrypted with a Timestream owned Amazon KMS key. To
    -- specify a KMS key, use the key ID, key ARN, alias name, or alias ARN.
    -- When using an alias name, prefix the name with /alias\//
    --
    -- If ErrorReportConfiguration uses @SSE_KMS@ as encryption type, the same
    -- KmsKeyId is used to encrypt the error report at rest.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs to label the scheduled query.
    tags :: Prelude.Maybe [Tag],
    -- | Configuration used for writing the result of a query.
    targetConfiguration :: Prelude.Maybe TargetConfiguration,
    -- | Name of the scheduled query.
    name :: Prelude.Text,
    -- | The query string to run. Parameter names can be specified in the query
    -- string @\@@ character followed by an identifier. The named Parameter
    -- @\@scheduled_runtime@ is reserved and can be used in the query to get
    -- the time at which the query is scheduled to run.
    --
    -- The timestamp calculated according to the ScheduleConfiguration
    -- parameter, will be the value of @\@scheduled_runtime@ paramater for each
    -- query run. For example, consider an instance of a scheduled query
    -- executing on 2021-12-01 00:00:00. For this instance, the
    -- @\@scheduled_runtime@ parameter is initialized to the timestamp
    -- 2021-12-01 00:00:00 when invoking the query.
    queryString :: Data.Sensitive Prelude.Text,
    -- | The schedule configuration for the query.
    scheduleConfiguration :: ScheduleConfiguration,
    -- | Notification configuration for the scheduled query. A notification is
    -- sent by Timestream when a query run finishes, when the state is updated
    -- or when you delete it.
    notificationConfiguration :: NotificationConfiguration,
    -- | The ARN for the IAM role that Timestream will assume when running the
    -- scheduled query.
    scheduledQueryExecutionRoleArn :: Prelude.Text,
    -- | Configuration for error reporting. Error reports will be generated when
    -- a problem is encountered when writing the query results.
    errorReportConfiguration :: ErrorReportConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduledQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createScheduledQuery_clientToken' - Using a ClientToken makes the call to CreateScheduledQuery idempotent,
-- in other words, making the same request repeatedly will produce the same
-- result. Making multiple identical CreateScheduledQuery requests has the
-- same effect as making a single request.
--
-- -   If CreateScheduledQuery is called without a @ClientToken@, the Query
--     SDK generates a @ClientToken@ on your behalf.
--
-- -   After 8 hours, any request with the same @ClientToken@ is treated as
--     a new request.
--
-- 'kmsKeyId', 'createScheduledQuery_kmsKeyId' - The Amazon KMS key used to encrypt the scheduled query resource,
-- at-rest. If the Amazon KMS key is not specified, the scheduled query
-- resource will be encrypted with a Timestream owned Amazon KMS key. To
-- specify a KMS key, use the key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix the name with /alias\//
--
-- If ErrorReportConfiguration uses @SSE_KMS@ as encryption type, the same
-- KmsKeyId is used to encrypt the error report at rest.
--
-- 'tags', 'createScheduledQuery_tags' - A list of key-value pairs to label the scheduled query.
--
-- 'targetConfiguration', 'createScheduledQuery_targetConfiguration' - Configuration used for writing the result of a query.
--
-- 'name', 'createScheduledQuery_name' - Name of the scheduled query.
--
-- 'queryString', 'createScheduledQuery_queryString' - The query string to run. Parameter names can be specified in the query
-- string @\@@ character followed by an identifier. The named Parameter
-- @\@scheduled_runtime@ is reserved and can be used in the query to get
-- the time at which the query is scheduled to run.
--
-- The timestamp calculated according to the ScheduleConfiguration
-- parameter, will be the value of @\@scheduled_runtime@ paramater for each
-- query run. For example, consider an instance of a scheduled query
-- executing on 2021-12-01 00:00:00. For this instance, the
-- @\@scheduled_runtime@ parameter is initialized to the timestamp
-- 2021-12-01 00:00:00 when invoking the query.
--
-- 'scheduleConfiguration', 'createScheduledQuery_scheduleConfiguration' - The schedule configuration for the query.
--
-- 'notificationConfiguration', 'createScheduledQuery_notificationConfiguration' - Notification configuration for the scheduled query. A notification is
-- sent by Timestream when a query run finishes, when the state is updated
-- or when you delete it.
--
-- 'scheduledQueryExecutionRoleArn', 'createScheduledQuery_scheduledQueryExecutionRoleArn' - The ARN for the IAM role that Timestream will assume when running the
-- scheduled query.
--
-- 'errorReportConfiguration', 'createScheduledQuery_errorReportConfiguration' - Configuration for error reporting. Error reports will be generated when
-- a problem is encountered when writing the query results.
newCreateScheduledQuery ::
  -- | 'name'
  Prelude.Text ->
  -- | 'queryString'
  Prelude.Text ->
  -- | 'scheduleConfiguration'
  ScheduleConfiguration ->
  -- | 'notificationConfiguration'
  NotificationConfiguration ->
  -- | 'scheduledQueryExecutionRoleArn'
  Prelude.Text ->
  -- | 'errorReportConfiguration'
  ErrorReportConfiguration ->
  CreateScheduledQuery
newCreateScheduledQuery
  pName_
  pQueryString_
  pScheduleConfiguration_
  pNotificationConfiguration_
  pScheduledQueryExecutionRoleArn_
  pErrorReportConfiguration_ =
    CreateScheduledQuery'
      { clientToken =
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        targetConfiguration = Prelude.Nothing,
        name = pName_,
        queryString = Data._Sensitive Lens.# pQueryString_,
        scheduleConfiguration = pScheduleConfiguration_,
        notificationConfiguration =
          pNotificationConfiguration_,
        scheduledQueryExecutionRoleArn =
          pScheduledQueryExecutionRoleArn_,
        errorReportConfiguration =
          pErrorReportConfiguration_
      }

-- | Using a ClientToken makes the call to CreateScheduledQuery idempotent,
-- in other words, making the same request repeatedly will produce the same
-- result. Making multiple identical CreateScheduledQuery requests has the
-- same effect as making a single request.
--
-- -   If CreateScheduledQuery is called without a @ClientToken@, the Query
--     SDK generates a @ClientToken@ on your behalf.
--
-- -   After 8 hours, any request with the same @ClientToken@ is treated as
--     a new request.
createScheduledQuery_clientToken :: Lens.Lens' CreateScheduledQuery (Prelude.Maybe Prelude.Text)
createScheduledQuery_clientToken = Lens.lens (\CreateScheduledQuery' {clientToken} -> clientToken) (\s@CreateScheduledQuery' {} a -> s {clientToken = a} :: CreateScheduledQuery) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon KMS key used to encrypt the scheduled query resource,
-- at-rest. If the Amazon KMS key is not specified, the scheduled query
-- resource will be encrypted with a Timestream owned Amazon KMS key. To
-- specify a KMS key, use the key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix the name with /alias\//
--
-- If ErrorReportConfiguration uses @SSE_KMS@ as encryption type, the same
-- KmsKeyId is used to encrypt the error report at rest.
createScheduledQuery_kmsKeyId :: Lens.Lens' CreateScheduledQuery (Prelude.Maybe Prelude.Text)
createScheduledQuery_kmsKeyId = Lens.lens (\CreateScheduledQuery' {kmsKeyId} -> kmsKeyId) (\s@CreateScheduledQuery' {} a -> s {kmsKeyId = a} :: CreateScheduledQuery)

-- | A list of key-value pairs to label the scheduled query.
createScheduledQuery_tags :: Lens.Lens' CreateScheduledQuery (Prelude.Maybe [Tag])
createScheduledQuery_tags = Lens.lens (\CreateScheduledQuery' {tags} -> tags) (\s@CreateScheduledQuery' {} a -> s {tags = a} :: CreateScheduledQuery) Prelude.. Lens.mapping Lens.coerced

-- | Configuration used for writing the result of a query.
createScheduledQuery_targetConfiguration :: Lens.Lens' CreateScheduledQuery (Prelude.Maybe TargetConfiguration)
createScheduledQuery_targetConfiguration = Lens.lens (\CreateScheduledQuery' {targetConfiguration} -> targetConfiguration) (\s@CreateScheduledQuery' {} a -> s {targetConfiguration = a} :: CreateScheduledQuery)

-- | Name of the scheduled query.
createScheduledQuery_name :: Lens.Lens' CreateScheduledQuery Prelude.Text
createScheduledQuery_name = Lens.lens (\CreateScheduledQuery' {name} -> name) (\s@CreateScheduledQuery' {} a -> s {name = a} :: CreateScheduledQuery)

-- | The query string to run. Parameter names can be specified in the query
-- string @\@@ character followed by an identifier. The named Parameter
-- @\@scheduled_runtime@ is reserved and can be used in the query to get
-- the time at which the query is scheduled to run.
--
-- The timestamp calculated according to the ScheduleConfiguration
-- parameter, will be the value of @\@scheduled_runtime@ paramater for each
-- query run. For example, consider an instance of a scheduled query
-- executing on 2021-12-01 00:00:00. For this instance, the
-- @\@scheduled_runtime@ parameter is initialized to the timestamp
-- 2021-12-01 00:00:00 when invoking the query.
createScheduledQuery_queryString :: Lens.Lens' CreateScheduledQuery Prelude.Text
createScheduledQuery_queryString = Lens.lens (\CreateScheduledQuery' {queryString} -> queryString) (\s@CreateScheduledQuery' {} a -> s {queryString = a} :: CreateScheduledQuery) Prelude.. Data._Sensitive

-- | The schedule configuration for the query.
createScheduledQuery_scheduleConfiguration :: Lens.Lens' CreateScheduledQuery ScheduleConfiguration
createScheduledQuery_scheduleConfiguration = Lens.lens (\CreateScheduledQuery' {scheduleConfiguration} -> scheduleConfiguration) (\s@CreateScheduledQuery' {} a -> s {scheduleConfiguration = a} :: CreateScheduledQuery)

-- | Notification configuration for the scheduled query. A notification is
-- sent by Timestream when a query run finishes, when the state is updated
-- or when you delete it.
createScheduledQuery_notificationConfiguration :: Lens.Lens' CreateScheduledQuery NotificationConfiguration
createScheduledQuery_notificationConfiguration = Lens.lens (\CreateScheduledQuery' {notificationConfiguration} -> notificationConfiguration) (\s@CreateScheduledQuery' {} a -> s {notificationConfiguration = a} :: CreateScheduledQuery)

-- | The ARN for the IAM role that Timestream will assume when running the
-- scheduled query.
createScheduledQuery_scheduledQueryExecutionRoleArn :: Lens.Lens' CreateScheduledQuery Prelude.Text
createScheduledQuery_scheduledQueryExecutionRoleArn = Lens.lens (\CreateScheduledQuery' {scheduledQueryExecutionRoleArn} -> scheduledQueryExecutionRoleArn) (\s@CreateScheduledQuery' {} a -> s {scheduledQueryExecutionRoleArn = a} :: CreateScheduledQuery)

-- | Configuration for error reporting. Error reports will be generated when
-- a problem is encountered when writing the query results.
createScheduledQuery_errorReportConfiguration :: Lens.Lens' CreateScheduledQuery ErrorReportConfiguration
createScheduledQuery_errorReportConfiguration = Lens.lens (\CreateScheduledQuery' {errorReportConfiguration} -> errorReportConfiguration) (\s@CreateScheduledQuery' {} a -> s {errorReportConfiguration = a} :: CreateScheduledQuery)

instance Core.AWSRequest CreateScheduledQuery where
  type
    AWSResponse CreateScheduledQuery =
      CreateScheduledQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScheduledQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
      )

instance Prelude.Hashable CreateScheduledQuery where
  hashWithSalt _salt CreateScheduledQuery' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` scheduleConfiguration
      `Prelude.hashWithSalt` notificationConfiguration
      `Prelude.hashWithSalt` scheduledQueryExecutionRoleArn
      `Prelude.hashWithSalt` errorReportConfiguration

instance Prelude.NFData CreateScheduledQuery where
  rnf CreateScheduledQuery' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf scheduleConfiguration
      `Prelude.seq` Prelude.rnf notificationConfiguration
      `Prelude.seq` Prelude.rnf scheduledQueryExecutionRoleArn
      `Prelude.seq` Prelude.rnf errorReportConfiguration

instance Data.ToHeaders CreateScheduledQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.CreateScheduledQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateScheduledQuery where
  toJSON CreateScheduledQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TargetConfiguration" Data..=)
              Prelude.<$> targetConfiguration,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("QueryString" Data..= queryString),
            Prelude.Just
              ( "ScheduleConfiguration"
                  Data..= scheduleConfiguration
              ),
            Prelude.Just
              ( "NotificationConfiguration"
                  Data..= notificationConfiguration
              ),
            Prelude.Just
              ( "ScheduledQueryExecutionRoleArn"
                  Data..= scheduledQueryExecutionRoleArn
              ),
            Prelude.Just
              ( "ErrorReportConfiguration"
                  Data..= errorReportConfiguration
              )
          ]
      )

instance Data.ToPath CreateScheduledQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateScheduledQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateScheduledQueryResponse' smart constructor.
data CreateScheduledQueryResponse = CreateScheduledQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | ARN for the created scheduled query.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduledQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createScheduledQueryResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createScheduledQueryResponse_arn' - ARN for the created scheduled query.
newCreateScheduledQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  CreateScheduledQueryResponse
newCreateScheduledQueryResponse pHttpStatus_ pArn_ =
  CreateScheduledQueryResponse'
    { httpStatus =
        pHttpStatus_,
      arn = pArn_
    }

-- | The response's http status code.
createScheduledQueryResponse_httpStatus :: Lens.Lens' CreateScheduledQueryResponse Prelude.Int
createScheduledQueryResponse_httpStatus = Lens.lens (\CreateScheduledQueryResponse' {httpStatus} -> httpStatus) (\s@CreateScheduledQueryResponse' {} a -> s {httpStatus = a} :: CreateScheduledQueryResponse)

-- | ARN for the created scheduled query.
createScheduledQueryResponse_arn :: Lens.Lens' CreateScheduledQueryResponse Prelude.Text
createScheduledQueryResponse_arn = Lens.lens (\CreateScheduledQueryResponse' {arn} -> arn) (\s@CreateScheduledQueryResponse' {} a -> s {arn = a} :: CreateScheduledQueryResponse)

instance Prelude.NFData CreateScheduledQueryResponse where
  rnf CreateScheduledQueryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
