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
-- Module      : Amazonka.KinesisAnalytics.DeleteApplicationOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- Deletes output destination configuration from your application
-- configuration. Amazon Kinesis Analytics will no longer write data from
-- the corresponding in-application stream to the external output
-- destination.
--
-- This operation requires permissions to perform the
-- @kinesisanalytics:DeleteApplicationOutput@ action.
module Amazonka.KinesisAnalytics.DeleteApplicationOutput
  ( -- * Creating a Request
    DeleteApplicationOutput (..),
    newDeleteApplicationOutput,

    -- * Request Lenses
    deleteApplicationOutput_applicationName,
    deleteApplicationOutput_currentApplicationVersionId,
    deleteApplicationOutput_outputId,

    -- * Destructuring the Response
    DeleteApplicationOutputResponse (..),
    newDeleteApplicationOutputResponse,

    -- * Response Lenses
    deleteApplicationOutputResponse_httpStatus,
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
-- /See:/ 'newDeleteApplicationOutput' smart constructor.
data DeleteApplicationOutput = DeleteApplicationOutput'
  { -- | Amazon Kinesis Analytics application name.
    applicationName :: Prelude.Text,
    -- | Amazon Kinesis Analytics application version. You can use the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation to get the current application version. If the version
    -- specified is not the current version, the
    -- @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Prelude.Natural,
    -- | The ID of the configuration to delete. Each output configuration that is
    -- added to the application, either when the application is created or
    -- later using the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationOutput.html AddApplicationOutput>
    -- operation, has a unique ID. You need to provide the ID to uniquely
    -- identify the output configuration that you want to delete from the
    -- application configuration. You can use the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation to get the specific @OutputId@.
    outputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteApplicationOutput_applicationName' - Amazon Kinesis Analytics application name.
--
-- 'currentApplicationVersionId', 'deleteApplicationOutput_currentApplicationVersionId' - Amazon Kinesis Analytics application version. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
--
-- 'outputId', 'deleteApplicationOutput_outputId' - The ID of the configuration to delete. Each output configuration that is
-- added to the application, either when the application is created or
-- later using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationOutput.html AddApplicationOutput>
-- operation, has a unique ID. You need to provide the ID to uniquely
-- identify the output configuration that you want to delete from the
-- application configuration. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the specific @OutputId@.
newDeleteApplicationOutput ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  -- | 'outputId'
  Prelude.Text ->
  DeleteApplicationOutput
newDeleteApplicationOutput
  pApplicationName_
  pCurrentApplicationVersionId_
  pOutputId_ =
    DeleteApplicationOutput'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        outputId = pOutputId_
      }

-- | Amazon Kinesis Analytics application name.
deleteApplicationOutput_applicationName :: Lens.Lens' DeleteApplicationOutput Prelude.Text
deleteApplicationOutput_applicationName = Lens.lens (\DeleteApplicationOutput' {applicationName} -> applicationName) (\s@DeleteApplicationOutput' {} a -> s {applicationName = a} :: DeleteApplicationOutput)

-- | Amazon Kinesis Analytics application version. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
deleteApplicationOutput_currentApplicationVersionId :: Lens.Lens' DeleteApplicationOutput Prelude.Natural
deleteApplicationOutput_currentApplicationVersionId = Lens.lens (\DeleteApplicationOutput' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@DeleteApplicationOutput' {} a -> s {currentApplicationVersionId = a} :: DeleteApplicationOutput)

-- | The ID of the configuration to delete. Each output configuration that is
-- added to the application, either when the application is created or
-- later using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationOutput.html AddApplicationOutput>
-- operation, has a unique ID. You need to provide the ID to uniquely
-- identify the output configuration that you want to delete from the
-- application configuration. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the specific @OutputId@.
deleteApplicationOutput_outputId :: Lens.Lens' DeleteApplicationOutput Prelude.Text
deleteApplicationOutput_outputId = Lens.lens (\DeleteApplicationOutput' {outputId} -> outputId) (\s@DeleteApplicationOutput' {} a -> s {outputId = a} :: DeleteApplicationOutput)

instance Core.AWSRequest DeleteApplicationOutput where
  type
    AWSResponse DeleteApplicationOutput =
      DeleteApplicationOutputResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationOutputResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApplicationOutput where
  hashWithSalt _salt DeleteApplicationOutput' {..} =
    _salt
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` currentApplicationVersionId
      `Prelude.hashWithSalt` outputId

instance Prelude.NFData DeleteApplicationOutput where
  rnf DeleteApplicationOutput' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf outputId

instance Data.ToHeaders DeleteApplicationOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20150814.DeleteApplicationOutput" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteApplicationOutput where
  toJSON DeleteApplicationOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ( "CurrentApplicationVersionId"
                  Data..= currentApplicationVersionId
              ),
            Prelude.Just ("OutputId" Data..= outputId)
          ]
      )

instance Data.ToPath DeleteApplicationOutput where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteApplicationOutput where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDeleteApplicationOutputResponse' smart constructor.
data DeleteApplicationOutputResponse = DeleteApplicationOutputResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApplicationOutputResponse_httpStatus' - The response's http status code.
newDeleteApplicationOutputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationOutputResponse
newDeleteApplicationOutputResponse pHttpStatus_ =
  DeleteApplicationOutputResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteApplicationOutputResponse_httpStatus :: Lens.Lens' DeleteApplicationOutputResponse Prelude.Int
deleteApplicationOutputResponse_httpStatus = Lens.lens (\DeleteApplicationOutputResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationOutputResponse' {} a -> s {httpStatus = a} :: DeleteApplicationOutputResponse)

instance
  Prelude.NFData
    DeleteApplicationOutputResponse
  where
  rnf DeleteApplicationOutputResponse' {..} =
    Prelude.rnf httpStatus
