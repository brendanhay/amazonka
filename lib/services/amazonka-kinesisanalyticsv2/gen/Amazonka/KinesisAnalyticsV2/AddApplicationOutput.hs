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
-- Module      : Amazonka.KinesisAnalyticsV2.AddApplicationOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an external destination to your SQL-based Kinesis Data Analytics
-- application.
--
-- If you want Kinesis Data Analytics to deliver data from an
-- in-application stream within your application to an external destination
-- (such as an Kinesis data stream, a Kinesis Data Firehose delivery
-- stream, or an Amazon Lambda function), you add the relevant
-- configuration to your application using this operation. You can
-- configure one or more outputs for your application. Each output
-- configuration maps an in-application stream and an external destination.
--
-- You can use one of the output configurations to deliver data from your
-- in-application error stream to an external destination so that you can
-- analyze the errors.
--
-- Any configuration update, including adding a streaming source using this
-- operation, results in a new version of the application. You can use the
-- DescribeApplication operation to find the current application version.
module Amazonka.KinesisAnalyticsV2.AddApplicationOutput
  ( -- * Creating a Request
    AddApplicationOutput (..),
    newAddApplicationOutput,

    -- * Request Lenses
    addApplicationOutput_applicationName,
    addApplicationOutput_currentApplicationVersionId,
    addApplicationOutput_output,

    -- * Destructuring the Response
    AddApplicationOutputResponse (..),
    newAddApplicationOutputResponse,

    -- * Response Lenses
    addApplicationOutputResponse_applicationARN,
    addApplicationOutputResponse_applicationVersionId,
    addApplicationOutputResponse_outputDescriptions,
    addApplicationOutputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddApplicationOutput' smart constructor.
data AddApplicationOutput = AddApplicationOutput'
  { -- | The name of the application to which you want to add the output
    -- configuration.
    applicationName :: Prelude.Text,
    -- | The version of the application to which you want to add the output
    -- configuration. You can use the DescribeApplication operation to get the
    -- current application version. If the version specified is not the current
    -- version, the @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Prelude.Natural,
    -- | An array of objects, each describing one output configuration. In the
    -- output configuration, you specify the name of an in-application stream,
    -- a destination (that is, a Kinesis data stream, a Kinesis Data Firehose
    -- delivery stream, or an Amazon Lambda function), and record the formation
    -- to use when writing to the destination.
    output :: Output
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'addApplicationOutput_applicationName' - The name of the application to which you want to add the output
-- configuration.
--
-- 'currentApplicationVersionId', 'addApplicationOutput_currentApplicationVersionId' - The version of the application to which you want to add the output
-- configuration. You can use the DescribeApplication operation to get the
-- current application version. If the version specified is not the current
-- version, the @ConcurrentModificationException@ is returned.
--
-- 'output', 'addApplicationOutput_output' - An array of objects, each describing one output configuration. In the
-- output configuration, you specify the name of an in-application stream,
-- a destination (that is, a Kinesis data stream, a Kinesis Data Firehose
-- delivery stream, or an Amazon Lambda function), and record the formation
-- to use when writing to the destination.
newAddApplicationOutput ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  -- | 'output'
  Output ->
  AddApplicationOutput
newAddApplicationOutput
  pApplicationName_
  pCurrentApplicationVersionId_
  pOutput_ =
    AddApplicationOutput'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        output = pOutput_
      }

-- | The name of the application to which you want to add the output
-- configuration.
addApplicationOutput_applicationName :: Lens.Lens' AddApplicationOutput Prelude.Text
addApplicationOutput_applicationName = Lens.lens (\AddApplicationOutput' {applicationName} -> applicationName) (\s@AddApplicationOutput' {} a -> s {applicationName = a} :: AddApplicationOutput)

-- | The version of the application to which you want to add the output
-- configuration. You can use the DescribeApplication operation to get the
-- current application version. If the version specified is not the current
-- version, the @ConcurrentModificationException@ is returned.
addApplicationOutput_currentApplicationVersionId :: Lens.Lens' AddApplicationOutput Prelude.Natural
addApplicationOutput_currentApplicationVersionId = Lens.lens (\AddApplicationOutput' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@AddApplicationOutput' {} a -> s {currentApplicationVersionId = a} :: AddApplicationOutput)

-- | An array of objects, each describing one output configuration. In the
-- output configuration, you specify the name of an in-application stream,
-- a destination (that is, a Kinesis data stream, a Kinesis Data Firehose
-- delivery stream, or an Amazon Lambda function), and record the formation
-- to use when writing to the destination.
addApplicationOutput_output :: Lens.Lens' AddApplicationOutput Output
addApplicationOutput_output = Lens.lens (\AddApplicationOutput' {output} -> output) (\s@AddApplicationOutput' {} a -> s {output = a} :: AddApplicationOutput)

instance Core.AWSRequest AddApplicationOutput where
  type
    AWSResponse AddApplicationOutput =
      AddApplicationOutputResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddApplicationOutputResponse'
            Prelude.<$> (x Data..?> "ApplicationARN")
            Prelude.<*> (x Data..?> "ApplicationVersionId")
            Prelude.<*> ( x
                            Data..?> "OutputDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddApplicationOutput where
  hashWithSalt _salt AddApplicationOutput' {..} =
    _salt
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` currentApplicationVersionId
      `Prelude.hashWithSalt` output

instance Prelude.NFData AddApplicationOutput where
  rnf AddApplicationOutput' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf output

instance Data.ToHeaders AddApplicationOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.AddApplicationOutput" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddApplicationOutput where
  toJSON AddApplicationOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ( "CurrentApplicationVersionId"
                  Data..= currentApplicationVersionId
              ),
            Prelude.Just ("Output" Data..= output)
          ]
      )

instance Data.ToPath AddApplicationOutput where
  toPath = Prelude.const "/"

instance Data.ToQuery AddApplicationOutput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddApplicationOutputResponse' smart constructor.
data AddApplicationOutputResponse = AddApplicationOutputResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationARN :: Prelude.Maybe Prelude.Text,
    -- | The updated application version ID. Kinesis Data Analytics increments
    -- this ID when the application is updated.
    applicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | Describes the application output configuration. For more information,
    -- see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
    outputDescriptions :: Prelude.Maybe [OutputDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationARN', 'addApplicationOutputResponse_applicationARN' - The application Amazon Resource Name (ARN).
--
-- 'applicationVersionId', 'addApplicationOutputResponse_applicationVersionId' - The updated application version ID. Kinesis Data Analytics increments
-- this ID when the application is updated.
--
-- 'outputDescriptions', 'addApplicationOutputResponse_outputDescriptions' - Describes the application output configuration. For more information,
-- see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
--
-- 'httpStatus', 'addApplicationOutputResponse_httpStatus' - The response's http status code.
newAddApplicationOutputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddApplicationOutputResponse
newAddApplicationOutputResponse pHttpStatus_ =
  AddApplicationOutputResponse'
    { applicationARN =
        Prelude.Nothing,
      applicationVersionId = Prelude.Nothing,
      outputDescriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application Amazon Resource Name (ARN).
addApplicationOutputResponse_applicationARN :: Lens.Lens' AddApplicationOutputResponse (Prelude.Maybe Prelude.Text)
addApplicationOutputResponse_applicationARN = Lens.lens (\AddApplicationOutputResponse' {applicationARN} -> applicationARN) (\s@AddApplicationOutputResponse' {} a -> s {applicationARN = a} :: AddApplicationOutputResponse)

-- | The updated application version ID. Kinesis Data Analytics increments
-- this ID when the application is updated.
addApplicationOutputResponse_applicationVersionId :: Lens.Lens' AddApplicationOutputResponse (Prelude.Maybe Prelude.Natural)
addApplicationOutputResponse_applicationVersionId = Lens.lens (\AddApplicationOutputResponse' {applicationVersionId} -> applicationVersionId) (\s@AddApplicationOutputResponse' {} a -> s {applicationVersionId = a} :: AddApplicationOutputResponse)

-- | Describes the application output configuration. For more information,
-- see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
addApplicationOutputResponse_outputDescriptions :: Lens.Lens' AddApplicationOutputResponse (Prelude.Maybe [OutputDescription])
addApplicationOutputResponse_outputDescriptions = Lens.lens (\AddApplicationOutputResponse' {outputDescriptions} -> outputDescriptions) (\s@AddApplicationOutputResponse' {} a -> s {outputDescriptions = a} :: AddApplicationOutputResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addApplicationOutputResponse_httpStatus :: Lens.Lens' AddApplicationOutputResponse Prelude.Int
addApplicationOutputResponse_httpStatus = Lens.lens (\AddApplicationOutputResponse' {httpStatus} -> httpStatus) (\s@AddApplicationOutputResponse' {} a -> s {httpStatus = a} :: AddApplicationOutputResponse)

instance Prelude.NFData AddApplicationOutputResponse where
  rnf AddApplicationOutputResponse' {..} =
    Prelude.rnf applicationARN
      `Prelude.seq` Prelude.rnf applicationVersionId
      `Prelude.seq` Prelude.rnf outputDescriptions
      `Prelude.seq` Prelude.rnf httpStatus
