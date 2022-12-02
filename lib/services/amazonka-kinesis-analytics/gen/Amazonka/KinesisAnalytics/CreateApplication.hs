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
-- Module      : Amazonka.KinesisAnalytics.CreateApplication
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
-- Creates an Amazon Kinesis Analytics application. You can configure each
-- application with one streaming source as input, application code to
-- process the input, and up to three destinations where you want Amazon
-- Kinesis Analytics to write the output data from your application. For an
-- overview, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works.html How it Works>.
--
-- In the input configuration, you map the streaming source to an
-- in-application stream, which you can think of as a constantly updating
-- table. In the mapping, you must provide a schema for the in-application
-- stream and map each data column in the in-application stream to a data
-- element in the streaming source.
--
-- Your application code is one or more SQL statements that read input
-- data, transform it, and generate output. Your application code can
-- create one or more SQL artifacts like SQL streams or pumps.
--
-- In the output configuration, you can configure the application to write
-- data from in-application streams created in your applications to up to
-- three destinations.
--
-- To read data from your source stream or write data to destination
-- streams, Amazon Kinesis Analytics needs your permissions. You grant
-- these permissions by creating IAM roles. This operation requires
-- permissions to perform the @kinesisanalytics:CreateApplication@ action.
--
-- For introductory exercises to create an Amazon Kinesis Analytics
-- application, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/getting-started.html Getting Started>.
module Amazonka.KinesisAnalytics.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_tags,
    createApplication_applicationCode,
    createApplication_cloudWatchLoggingOptions,
    createApplication_outputs,
    createApplication_inputs,
    createApplication_applicationDescription,
    createApplication_applicationName,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | TBD
--
-- /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | A list of one or more tags to assign to the application. A tag is a
    -- key-value pair that identifies an application. Note that the maximum
    -- number of application tags includes system tags. The maximum number of
    -- user-defined application tags is 50. For more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-tagging.html Using Tagging>.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | One or more SQL statements that read input data, transform it, and
    -- generate output. For example, you can write a SQL statement that reads
    -- data from one in-application stream, generates a running average of the
    -- number of advertisement clicks by vendor, and insert resulting rows in
    -- another in-application stream using pumps. For more information about
    -- the typical pattern, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-app-code.html Application Code>.
    --
    -- You can provide such series of SQL statements, where output of one
    -- statement can be used as the input for the next statement. You store
    -- intermediate results by creating in-application streams and pumps.
    --
    -- Note that the application code must create the streams with names
    -- specified in the @Outputs@. For example, if your @Outputs@ defines
    -- output streams named @ExampleOutputStream1@ and @ExampleOutputStream2@,
    -- then your application code must create these streams.
    applicationCode :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to configure a CloudWatch log stream to monitor
    -- application configuration errors. For more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
    cloudWatchLoggingOptions :: Prelude.Maybe [CloudWatchLoggingOption],
    -- | You can configure application output to write data from any of the
    -- in-application streams to up to three destinations.
    --
    -- These destinations can be Amazon Kinesis streams, Amazon Kinesis
    -- Firehose delivery streams, AWS Lambda destinations, or any combination
    -- of the three.
    --
    -- In the configuration, you specify the in-application stream name, the
    -- destination stream or Lambda function Amazon Resource Name (ARN), and
    -- the format to use when writing data. You must also provide an IAM role
    -- that Amazon Kinesis Analytics can assume to write to the destination
    -- stream or Lambda function on your behalf.
    --
    -- In the output configuration, you also provide the output stream or
    -- Lambda function ARN. For stream destinations, you provide the format of
    -- data in the stream (for example, JSON, CSV). You also must provide an
    -- IAM role that Amazon Kinesis Analytics can assume to write to the stream
    -- or Lambda function on your behalf.
    outputs :: Prelude.Maybe [Output],
    -- | Use this parameter to configure the application input.
    --
    -- You can configure your application to receive input from a single
    -- streaming source. In this configuration, you map this streaming source
    -- to an in-application stream that is created. Your application code can
    -- then query the in-application stream like a table (you can think of it
    -- as a constantly updating table).
    --
    -- For the streaming source, you provide its Amazon Resource Name (ARN) and
    -- format of data on the stream (for example, JSON, CSV, etc.). You also
    -- must provide an IAM role that Amazon Kinesis Analytics can assume to
    -- read this stream on your behalf.
    --
    -- To create the in-application stream, you need to specify a schema to
    -- transform your data into a schematized version used in SQL. In the
    -- schema, you provide the necessary mapping of the data elements in the
    -- streaming source to record columns in the in-app stream.
    inputs :: Prelude.Maybe [Input],
    -- | Summary description of the application.
    applicationDescription :: Prelude.Maybe Prelude.Text,
    -- | Name of your Amazon Kinesis Analytics application (for example,
    -- @sample-app@).
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createApplication_tags' - A list of one or more tags to assign to the application. A tag is a
-- key-value pair that identifies an application. Note that the maximum
-- number of application tags includes system tags. The maximum number of
-- user-defined application tags is 50. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-tagging.html Using Tagging>.
--
-- 'applicationCode', 'createApplication_applicationCode' - One or more SQL statements that read input data, transform it, and
-- generate output. For example, you can write a SQL statement that reads
-- data from one in-application stream, generates a running average of the
-- number of advertisement clicks by vendor, and insert resulting rows in
-- another in-application stream using pumps. For more information about
-- the typical pattern, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-app-code.html Application Code>.
--
-- You can provide such series of SQL statements, where output of one
-- statement can be used as the input for the next statement. You store
-- intermediate results by creating in-application streams and pumps.
--
-- Note that the application code must create the streams with names
-- specified in the @Outputs@. For example, if your @Outputs@ defines
-- output streams named @ExampleOutputStream1@ and @ExampleOutputStream2@,
-- then your application code must create these streams.
--
-- 'cloudWatchLoggingOptions', 'createApplication_cloudWatchLoggingOptions' - Use this parameter to configure a CloudWatch log stream to monitor
-- application configuration errors. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
--
-- 'outputs', 'createApplication_outputs' - You can configure application output to write data from any of the
-- in-application streams to up to three destinations.
--
-- These destinations can be Amazon Kinesis streams, Amazon Kinesis
-- Firehose delivery streams, AWS Lambda destinations, or any combination
-- of the three.
--
-- In the configuration, you specify the in-application stream name, the
-- destination stream or Lambda function Amazon Resource Name (ARN), and
-- the format to use when writing data. You must also provide an IAM role
-- that Amazon Kinesis Analytics can assume to write to the destination
-- stream or Lambda function on your behalf.
--
-- In the output configuration, you also provide the output stream or
-- Lambda function ARN. For stream destinations, you provide the format of
-- data in the stream (for example, JSON, CSV). You also must provide an
-- IAM role that Amazon Kinesis Analytics can assume to write to the stream
-- or Lambda function on your behalf.
--
-- 'inputs', 'createApplication_inputs' - Use this parameter to configure the application input.
--
-- You can configure your application to receive input from a single
-- streaming source. In this configuration, you map this streaming source
-- to an in-application stream that is created. Your application code can
-- then query the in-application stream like a table (you can think of it
-- as a constantly updating table).
--
-- For the streaming source, you provide its Amazon Resource Name (ARN) and
-- format of data on the stream (for example, JSON, CSV, etc.). You also
-- must provide an IAM role that Amazon Kinesis Analytics can assume to
-- read this stream on your behalf.
--
-- To create the in-application stream, you need to specify a schema to
-- transform your data into a schematized version used in SQL. In the
-- schema, you provide the necessary mapping of the data elements in the
-- streaming source to record columns in the in-app stream.
--
-- 'applicationDescription', 'createApplication_applicationDescription' - Summary description of the application.
--
-- 'applicationName', 'createApplication_applicationName' - Name of your Amazon Kinesis Analytics application (for example,
-- @sample-app@).
newCreateApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  CreateApplication
newCreateApplication pApplicationName_ =
  CreateApplication'
    { tags = Prelude.Nothing,
      applicationCode = Prelude.Nothing,
      cloudWatchLoggingOptions = Prelude.Nothing,
      outputs = Prelude.Nothing,
      inputs = Prelude.Nothing,
      applicationDescription = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | A list of one or more tags to assign to the application. A tag is a
-- key-value pair that identifies an application. Note that the maximum
-- number of application tags includes system tags. The maximum number of
-- user-defined application tags is 50. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-tagging.html Using Tagging>.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.NonEmpty Tag))
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | One or more SQL statements that read input data, transform it, and
-- generate output. For example, you can write a SQL statement that reads
-- data from one in-application stream, generates a running average of the
-- number of advertisement clicks by vendor, and insert resulting rows in
-- another in-application stream using pumps. For more information about
-- the typical pattern, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-app-code.html Application Code>.
--
-- You can provide such series of SQL statements, where output of one
-- statement can be used as the input for the next statement. You store
-- intermediate results by creating in-application streams and pumps.
--
-- Note that the application code must create the streams with names
-- specified in the @Outputs@. For example, if your @Outputs@ defines
-- output streams named @ExampleOutputStream1@ and @ExampleOutputStream2@,
-- then your application code must create these streams.
createApplication_applicationCode :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_applicationCode = Lens.lens (\CreateApplication' {applicationCode} -> applicationCode) (\s@CreateApplication' {} a -> s {applicationCode = a} :: CreateApplication)

-- | Use this parameter to configure a CloudWatch log stream to monitor
-- application configuration errors. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
createApplication_cloudWatchLoggingOptions :: Lens.Lens' CreateApplication (Prelude.Maybe [CloudWatchLoggingOption])
createApplication_cloudWatchLoggingOptions = Lens.lens (\CreateApplication' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@CreateApplication' {} a -> s {cloudWatchLoggingOptions = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | You can configure application output to write data from any of the
-- in-application streams to up to three destinations.
--
-- These destinations can be Amazon Kinesis streams, Amazon Kinesis
-- Firehose delivery streams, AWS Lambda destinations, or any combination
-- of the three.
--
-- In the configuration, you specify the in-application stream name, the
-- destination stream or Lambda function Amazon Resource Name (ARN), and
-- the format to use when writing data. You must also provide an IAM role
-- that Amazon Kinesis Analytics can assume to write to the destination
-- stream or Lambda function on your behalf.
--
-- In the output configuration, you also provide the output stream or
-- Lambda function ARN. For stream destinations, you provide the format of
-- data in the stream (for example, JSON, CSV). You also must provide an
-- IAM role that Amazon Kinesis Analytics can assume to write to the stream
-- or Lambda function on your behalf.
createApplication_outputs :: Lens.Lens' CreateApplication (Prelude.Maybe [Output])
createApplication_outputs = Lens.lens (\CreateApplication' {outputs} -> outputs) (\s@CreateApplication' {} a -> s {outputs = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | Use this parameter to configure the application input.
--
-- You can configure your application to receive input from a single
-- streaming source. In this configuration, you map this streaming source
-- to an in-application stream that is created. Your application code can
-- then query the in-application stream like a table (you can think of it
-- as a constantly updating table).
--
-- For the streaming source, you provide its Amazon Resource Name (ARN) and
-- format of data on the stream (for example, JSON, CSV, etc.). You also
-- must provide an IAM role that Amazon Kinesis Analytics can assume to
-- read this stream on your behalf.
--
-- To create the in-application stream, you need to specify a schema to
-- transform your data into a schematized version used in SQL. In the
-- schema, you provide the necessary mapping of the data elements in the
-- streaming source to record columns in the in-app stream.
createApplication_inputs :: Lens.Lens' CreateApplication (Prelude.Maybe [Input])
createApplication_inputs = Lens.lens (\CreateApplication' {inputs} -> inputs) (\s@CreateApplication' {} a -> s {inputs = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | Summary description of the application.
createApplication_applicationDescription :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_applicationDescription = Lens.lens (\CreateApplication' {applicationDescription} -> applicationDescription) (\s@CreateApplication' {} a -> s {applicationDescription = a} :: CreateApplication)

-- | Name of your Amazon Kinesis Analytics application (for example,
-- @sample-app@).
createApplication_applicationName :: Lens.Lens' CreateApplication Prelude.Text
createApplication_applicationName = Lens.lens (\CreateApplication' {applicationName} -> applicationName) (\s@CreateApplication' {} a -> s {applicationName = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ApplicationSummary")
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` applicationCode
      `Prelude.hashWithSalt` cloudWatchLoggingOptions
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` applicationDescription
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf applicationCode
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf applicationDescription
      `Prelude.seq` Prelude.rnf applicationName

instance Data.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20150814.CreateApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ApplicationCode" Data..=)
              Prelude.<$> applicationCode,
            ("CloudWatchLoggingOptions" Data..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("Outputs" Data..=) Prelude.<$> outputs,
            ("Inputs" Data..=) Prelude.<$> inputs,
            ("ApplicationDescription" Data..=)
              Prelude.<$> applicationDescription,
            Prelude.Just
              ("ApplicationName" Data..= applicationName)
          ]
      )

instance Data.ToPath CreateApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | TBD
--
-- /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | In response to your @CreateApplication@ request, Amazon Kinesis
    -- Analytics returns a response with a summary of the application it
    -- created, including the application Amazon Resource Name (ARN), name, and
    -- status.
    applicationSummary :: ApplicationSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
--
-- 'applicationSummary', 'createApplicationResponse_applicationSummary' - In response to your @CreateApplication@ request, Amazon Kinesis
-- Analytics returns a response with a summary of the application it
-- created, including the application Amazon Resource Name (ARN), name, and
-- status.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationSummary'
  ApplicationSummary ->
  CreateApplicationResponse
newCreateApplicationResponse
  pHttpStatus_
  pApplicationSummary_ =
    CreateApplicationResponse'
      { httpStatus =
          pHttpStatus_,
        applicationSummary = pApplicationSummary_
      }

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

-- | In response to your @CreateApplication@ request, Amazon Kinesis
-- Analytics returns a response with a summary of the application it
-- created, including the application Amazon Resource Name (ARN), name, and
-- status.
createApplicationResponse_applicationSummary :: Lens.Lens' CreateApplicationResponse ApplicationSummary
createApplicationResponse_applicationSummary = Lens.lens (\CreateApplicationResponse' {applicationSummary} -> applicationSummary) (\s@CreateApplicationResponse' {} a -> s {applicationSummary = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse where
  rnf CreateApplicationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationSummary
