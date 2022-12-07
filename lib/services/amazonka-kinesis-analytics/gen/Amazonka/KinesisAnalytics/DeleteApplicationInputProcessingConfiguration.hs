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
-- Module      : Amazonka.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
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
-- Deletes an
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
-- from an input.
module Amazonka.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
  ( -- * Creating a Request
    DeleteApplicationInputProcessingConfiguration (..),
    newDeleteApplicationInputProcessingConfiguration,

    -- * Request Lenses
    deleteApplicationInputProcessingConfiguration_applicationName,
    deleteApplicationInputProcessingConfiguration_currentApplicationVersionId,
    deleteApplicationInputProcessingConfiguration_inputId,

    -- * Destructuring the Response
    DeleteApplicationInputProcessingConfigurationResponse (..),
    newDeleteApplicationInputProcessingConfigurationResponse,

    -- * Response Lenses
    deleteApplicationInputProcessingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplicationInputProcessingConfiguration' smart constructor.
data DeleteApplicationInputProcessingConfiguration = DeleteApplicationInputProcessingConfiguration'
  { -- | The Kinesis Analytics application name.
    applicationName :: Prelude.Text,
    -- | The version ID of the Kinesis Analytics application.
    currentApplicationVersionId :: Prelude.Natural,
    -- | The ID of the input configuration from which to delete the input
    -- processing configuration. You can get a list of the input IDs for an
    -- application by using the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation.
    inputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationInputProcessingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteApplicationInputProcessingConfiguration_applicationName' - The Kinesis Analytics application name.
--
-- 'currentApplicationVersionId', 'deleteApplicationInputProcessingConfiguration_currentApplicationVersionId' - The version ID of the Kinesis Analytics application.
--
-- 'inputId', 'deleteApplicationInputProcessingConfiguration_inputId' - The ID of the input configuration from which to delete the input
-- processing configuration. You can get a list of the input IDs for an
-- application by using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation.
newDeleteApplicationInputProcessingConfiguration ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  -- | 'inputId'
  Prelude.Text ->
  DeleteApplicationInputProcessingConfiguration
newDeleteApplicationInputProcessingConfiguration
  pApplicationName_
  pCurrentApplicationVersionId_
  pInputId_ =
    DeleteApplicationInputProcessingConfiguration'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        inputId = pInputId_
      }

-- | The Kinesis Analytics application name.
deleteApplicationInputProcessingConfiguration_applicationName :: Lens.Lens' DeleteApplicationInputProcessingConfiguration Prelude.Text
deleteApplicationInputProcessingConfiguration_applicationName = Lens.lens (\DeleteApplicationInputProcessingConfiguration' {applicationName} -> applicationName) (\s@DeleteApplicationInputProcessingConfiguration' {} a -> s {applicationName = a} :: DeleteApplicationInputProcessingConfiguration)

-- | The version ID of the Kinesis Analytics application.
deleteApplicationInputProcessingConfiguration_currentApplicationVersionId :: Lens.Lens' DeleteApplicationInputProcessingConfiguration Prelude.Natural
deleteApplicationInputProcessingConfiguration_currentApplicationVersionId = Lens.lens (\DeleteApplicationInputProcessingConfiguration' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@DeleteApplicationInputProcessingConfiguration' {} a -> s {currentApplicationVersionId = a} :: DeleteApplicationInputProcessingConfiguration)

-- | The ID of the input configuration from which to delete the input
-- processing configuration. You can get a list of the input IDs for an
-- application by using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation.
deleteApplicationInputProcessingConfiguration_inputId :: Lens.Lens' DeleteApplicationInputProcessingConfiguration Prelude.Text
deleteApplicationInputProcessingConfiguration_inputId = Lens.lens (\DeleteApplicationInputProcessingConfiguration' {inputId} -> inputId) (\s@DeleteApplicationInputProcessingConfiguration' {} a -> s {inputId = a} :: DeleteApplicationInputProcessingConfiguration)

instance
  Core.AWSRequest
    DeleteApplicationInputProcessingConfiguration
  where
  type
    AWSResponse
      DeleteApplicationInputProcessingConfiguration =
      DeleteApplicationInputProcessingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationInputProcessingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteApplicationInputProcessingConfiguration
  where
  hashWithSalt
    _salt
    DeleteApplicationInputProcessingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` currentApplicationVersionId
        `Prelude.hashWithSalt` inputId

instance
  Prelude.NFData
    DeleteApplicationInputProcessingConfiguration
  where
  rnf
    DeleteApplicationInputProcessingConfiguration' {..} =
      Prelude.rnf applicationName
        `Prelude.seq` Prelude.rnf currentApplicationVersionId
        `Prelude.seq` Prelude.rnf inputId

instance
  Data.ToHeaders
    DeleteApplicationInputProcessingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20150814.DeleteApplicationInputProcessingConfiguration" ::
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
    DeleteApplicationInputProcessingConfiguration
  where
  toJSON
    DeleteApplicationInputProcessingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("ApplicationName" Data..= applicationName),
              Prelude.Just
                ( "CurrentApplicationVersionId"
                    Data..= currentApplicationVersionId
                ),
              Prelude.Just ("InputId" Data..= inputId)
            ]
        )

instance
  Data.ToPath
    DeleteApplicationInputProcessingConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteApplicationInputProcessingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationInputProcessingConfigurationResponse' smart constructor.
data DeleteApplicationInputProcessingConfigurationResponse = DeleteApplicationInputProcessingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationInputProcessingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApplicationInputProcessingConfigurationResponse_httpStatus' - The response's http status code.
newDeleteApplicationInputProcessingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationInputProcessingConfigurationResponse
newDeleteApplicationInputProcessingConfigurationResponse
  pHttpStatus_ =
    DeleteApplicationInputProcessingConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteApplicationInputProcessingConfigurationResponse_httpStatus :: Lens.Lens' DeleteApplicationInputProcessingConfigurationResponse Prelude.Int
deleteApplicationInputProcessingConfigurationResponse_httpStatus = Lens.lens (\DeleteApplicationInputProcessingConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationInputProcessingConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteApplicationInputProcessingConfigurationResponse)

instance
  Prelude.NFData
    DeleteApplicationInputProcessingConfigurationResponse
  where
  rnf
    DeleteApplicationInputProcessingConfigurationResponse' {..} =
      Prelude.rnf httpStatus
