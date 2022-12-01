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
-- Module      : Amazonka.KinesisAnalytics.AddApplicationInputProcessingConfiguration
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
-- Adds an
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
-- to an application. An input processor preprocesses records on the input
-- stream before the application\'s SQL code executes. Currently, the only
-- input processor available is
-- <https://docs.aws.amazon.com/lambda/ AWS Lambda>.
module Amazonka.KinesisAnalytics.AddApplicationInputProcessingConfiguration
  ( -- * Creating a Request
    AddApplicationInputProcessingConfiguration (..),
    newAddApplicationInputProcessingConfiguration,

    -- * Request Lenses
    addApplicationInputProcessingConfiguration_applicationName,
    addApplicationInputProcessingConfiguration_currentApplicationVersionId,
    addApplicationInputProcessingConfiguration_inputId,
    addApplicationInputProcessingConfiguration_inputProcessingConfiguration,

    -- * Destructuring the Response
    AddApplicationInputProcessingConfigurationResponse (..),
    newAddApplicationInputProcessingConfigurationResponse,

    -- * Response Lenses
    addApplicationInputProcessingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddApplicationInputProcessingConfiguration' smart constructor.
data AddApplicationInputProcessingConfiguration = AddApplicationInputProcessingConfiguration'
  { -- | Name of the application to which you want to add the input processing
    -- configuration.
    applicationName :: Prelude.Text,
    -- | Version of the application to which you want to add the input processing
    -- configuration. You can use the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation to get the current application version. If the version
    -- specified is not the current version, the
    -- @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Prelude.Natural,
    -- | The ID of the input configuration to add the input processing
    -- configuration to. You can get a list of the input IDs for an application
    -- using the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation.
    inputId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
    -- to add to the application.
    inputProcessingConfiguration :: InputProcessingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationInputProcessingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'addApplicationInputProcessingConfiguration_applicationName' - Name of the application to which you want to add the input processing
-- configuration.
--
-- 'currentApplicationVersionId', 'addApplicationInputProcessingConfiguration_currentApplicationVersionId' - Version of the application to which you want to add the input processing
-- configuration. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
--
-- 'inputId', 'addApplicationInputProcessingConfiguration_inputId' - The ID of the input configuration to add the input processing
-- configuration to. You can get a list of the input IDs for an application
-- using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation.
--
-- 'inputProcessingConfiguration', 'addApplicationInputProcessingConfiguration_inputProcessingConfiguration' - The
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
-- to add to the application.
newAddApplicationInputProcessingConfiguration ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  -- | 'inputId'
  Prelude.Text ->
  -- | 'inputProcessingConfiguration'
  InputProcessingConfiguration ->
  AddApplicationInputProcessingConfiguration
newAddApplicationInputProcessingConfiguration
  pApplicationName_
  pCurrentApplicationVersionId_
  pInputId_
  pInputProcessingConfiguration_ =
    AddApplicationInputProcessingConfiguration'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        inputId = pInputId_,
        inputProcessingConfiguration =
          pInputProcessingConfiguration_
      }

-- | Name of the application to which you want to add the input processing
-- configuration.
addApplicationInputProcessingConfiguration_applicationName :: Lens.Lens' AddApplicationInputProcessingConfiguration Prelude.Text
addApplicationInputProcessingConfiguration_applicationName = Lens.lens (\AddApplicationInputProcessingConfiguration' {applicationName} -> applicationName) (\s@AddApplicationInputProcessingConfiguration' {} a -> s {applicationName = a} :: AddApplicationInputProcessingConfiguration)

-- | Version of the application to which you want to add the input processing
-- configuration. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get the current application version. If the version
-- specified is not the current version, the
-- @ConcurrentModificationException@ is returned.
addApplicationInputProcessingConfiguration_currentApplicationVersionId :: Lens.Lens' AddApplicationInputProcessingConfiguration Prelude.Natural
addApplicationInputProcessingConfiguration_currentApplicationVersionId = Lens.lens (\AddApplicationInputProcessingConfiguration' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@AddApplicationInputProcessingConfiguration' {} a -> s {currentApplicationVersionId = a} :: AddApplicationInputProcessingConfiguration)

-- | The ID of the input configuration to add the input processing
-- configuration to. You can get a list of the input IDs for an application
-- using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation.
addApplicationInputProcessingConfiguration_inputId :: Lens.Lens' AddApplicationInputProcessingConfiguration Prelude.Text
addApplicationInputProcessingConfiguration_inputId = Lens.lens (\AddApplicationInputProcessingConfiguration' {inputId} -> inputId) (\s@AddApplicationInputProcessingConfiguration' {} a -> s {inputId = a} :: AddApplicationInputProcessingConfiguration)

-- | The
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
-- to add to the application.
addApplicationInputProcessingConfiguration_inputProcessingConfiguration :: Lens.Lens' AddApplicationInputProcessingConfiguration InputProcessingConfiguration
addApplicationInputProcessingConfiguration_inputProcessingConfiguration = Lens.lens (\AddApplicationInputProcessingConfiguration' {inputProcessingConfiguration} -> inputProcessingConfiguration) (\s@AddApplicationInputProcessingConfiguration' {} a -> s {inputProcessingConfiguration = a} :: AddApplicationInputProcessingConfiguration)

instance
  Core.AWSRequest
    AddApplicationInputProcessingConfiguration
  where
  type
    AWSResponse
      AddApplicationInputProcessingConfiguration =
      AddApplicationInputProcessingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddApplicationInputProcessingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AddApplicationInputProcessingConfiguration
  where
  hashWithSalt
    _salt
    AddApplicationInputProcessingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` currentApplicationVersionId
        `Prelude.hashWithSalt` inputId
        `Prelude.hashWithSalt` inputProcessingConfiguration

instance
  Prelude.NFData
    AddApplicationInputProcessingConfiguration
  where
  rnf AddApplicationInputProcessingConfiguration' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf inputId
      `Prelude.seq` Prelude.rnf inputProcessingConfiguration

instance
  Core.ToHeaders
    AddApplicationInputProcessingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20150814.AddApplicationInputProcessingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    AddApplicationInputProcessingConfiguration
  where
  toJSON
    AddApplicationInputProcessingConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("ApplicationName" Core..= applicationName),
              Prelude.Just
                ( "CurrentApplicationVersionId"
                    Core..= currentApplicationVersionId
                ),
              Prelude.Just ("InputId" Core..= inputId),
              Prelude.Just
                ( "InputProcessingConfiguration"
                    Core..= inputProcessingConfiguration
                )
            ]
        )

instance
  Core.ToPath
    AddApplicationInputProcessingConfiguration
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    AddApplicationInputProcessingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddApplicationInputProcessingConfigurationResponse' smart constructor.
data AddApplicationInputProcessingConfigurationResponse = AddApplicationInputProcessingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationInputProcessingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addApplicationInputProcessingConfigurationResponse_httpStatus' - The response's http status code.
newAddApplicationInputProcessingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddApplicationInputProcessingConfigurationResponse
newAddApplicationInputProcessingConfigurationResponse
  pHttpStatus_ =
    AddApplicationInputProcessingConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
addApplicationInputProcessingConfigurationResponse_httpStatus :: Lens.Lens' AddApplicationInputProcessingConfigurationResponse Prelude.Int
addApplicationInputProcessingConfigurationResponse_httpStatus = Lens.lens (\AddApplicationInputProcessingConfigurationResponse' {httpStatus} -> httpStatus) (\s@AddApplicationInputProcessingConfigurationResponse' {} a -> s {httpStatus = a} :: AddApplicationInputProcessingConfigurationResponse)

instance
  Prelude.NFData
    AddApplicationInputProcessingConfigurationResponse
  where
  rnf
    AddApplicationInputProcessingConfigurationResponse' {..} =
      Prelude.rnf httpStatus
