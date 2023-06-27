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
-- Module      : Amazonka.KinesisAnalyticsV2.AddApplicationInputProcessingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an InputProcessingConfiguration to a SQL-based Kinesis Data
-- Analytics application. An input processor pre-processes records on the
-- input stream before the application\'s SQL code executes. Currently, the
-- only input processor available is
-- <https://docs.aws.amazon.com/lambda/ Amazon Lambda>.
module Amazonka.KinesisAnalyticsV2.AddApplicationInputProcessingConfiguration
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
    addApplicationInputProcessingConfigurationResponse_applicationARN,
    addApplicationInputProcessingConfigurationResponse_applicationVersionId,
    addApplicationInputProcessingConfigurationResponse_inputId,
    addApplicationInputProcessingConfigurationResponse_inputProcessingConfigurationDescription,
    addApplicationInputProcessingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddApplicationInputProcessingConfiguration' smart constructor.
data AddApplicationInputProcessingConfiguration = AddApplicationInputProcessingConfiguration'
  { -- | The name of the application to which you want to add the input
    -- processing configuration.
    applicationName :: Prelude.Text,
    -- | The version of the application to which you want to add the input
    -- processing configuration. You can use the DescribeApplication operation
    -- to get the current application version. If the version specified is not
    -- the current version, the @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Prelude.Natural,
    -- | The ID of the input configuration to add the input processing
    -- configuration to. You can get a list of the input IDs for an application
    -- using the DescribeApplication operation.
    inputId :: Prelude.Text,
    -- | The InputProcessingConfiguration to add to the application.
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
-- 'applicationName', 'addApplicationInputProcessingConfiguration_applicationName' - The name of the application to which you want to add the input
-- processing configuration.
--
-- 'currentApplicationVersionId', 'addApplicationInputProcessingConfiguration_currentApplicationVersionId' - The version of the application to which you want to add the input
-- processing configuration. You can use the DescribeApplication operation
-- to get the current application version. If the version specified is not
-- the current version, the @ConcurrentModificationException@ is returned.
--
-- 'inputId', 'addApplicationInputProcessingConfiguration_inputId' - The ID of the input configuration to add the input processing
-- configuration to. You can get a list of the input IDs for an application
-- using the DescribeApplication operation.
--
-- 'inputProcessingConfiguration', 'addApplicationInputProcessingConfiguration_inputProcessingConfiguration' - The InputProcessingConfiguration to add to the application.
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

-- | The name of the application to which you want to add the input
-- processing configuration.
addApplicationInputProcessingConfiguration_applicationName :: Lens.Lens' AddApplicationInputProcessingConfiguration Prelude.Text
addApplicationInputProcessingConfiguration_applicationName = Lens.lens (\AddApplicationInputProcessingConfiguration' {applicationName} -> applicationName) (\s@AddApplicationInputProcessingConfiguration' {} a -> s {applicationName = a} :: AddApplicationInputProcessingConfiguration)

-- | The version of the application to which you want to add the input
-- processing configuration. You can use the DescribeApplication operation
-- to get the current application version. If the version specified is not
-- the current version, the @ConcurrentModificationException@ is returned.
addApplicationInputProcessingConfiguration_currentApplicationVersionId :: Lens.Lens' AddApplicationInputProcessingConfiguration Prelude.Natural
addApplicationInputProcessingConfiguration_currentApplicationVersionId = Lens.lens (\AddApplicationInputProcessingConfiguration' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@AddApplicationInputProcessingConfiguration' {} a -> s {currentApplicationVersionId = a} :: AddApplicationInputProcessingConfiguration)

-- | The ID of the input configuration to add the input processing
-- configuration to. You can get a list of the input IDs for an application
-- using the DescribeApplication operation.
addApplicationInputProcessingConfiguration_inputId :: Lens.Lens' AddApplicationInputProcessingConfiguration Prelude.Text
addApplicationInputProcessingConfiguration_inputId = Lens.lens (\AddApplicationInputProcessingConfiguration' {inputId} -> inputId) (\s@AddApplicationInputProcessingConfiguration' {} a -> s {inputId = a} :: AddApplicationInputProcessingConfiguration)

-- | The InputProcessingConfiguration to add to the application.
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
    Response.receiveJSON
      ( \s h x ->
          AddApplicationInputProcessingConfigurationResponse'
            Prelude.<$> (x Data..?> "ApplicationARN")
            Prelude.<*> (x Data..?> "ApplicationVersionId")
            Prelude.<*> (x Data..?> "InputId")
            Prelude.<*> ( x
                            Data..?> "InputProcessingConfigurationDescription"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AddApplicationInputProcessingConfiguration
  where
  hashWithSalt
    _salt
    AddApplicationInputProcessingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` applicationName
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
  Data.ToHeaders
    AddApplicationInputProcessingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.AddApplicationInputProcessingConfiguration" ::
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
    AddApplicationInputProcessingConfiguration
  where
  toJSON
    AddApplicationInputProcessingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("ApplicationName" Data..= applicationName),
              Prelude.Just
                ( "CurrentApplicationVersionId"
                    Data..= currentApplicationVersionId
                ),
              Prelude.Just ("InputId" Data..= inputId),
              Prelude.Just
                ( "InputProcessingConfiguration"
                    Data..= inputProcessingConfiguration
                )
            ]
        )

instance
  Data.ToPath
    AddApplicationInputProcessingConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AddApplicationInputProcessingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddApplicationInputProcessingConfigurationResponse' smart constructor.
data AddApplicationInputProcessingConfigurationResponse = AddApplicationInputProcessingConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationARN :: Prelude.Maybe Prelude.Text,
    -- | Provides the current application version.
    applicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The input ID that is associated with the application input. This is the
    -- ID that Kinesis Data Analytics assigns to each input configuration that
    -- you add to your application.
    inputId :: Prelude.Maybe Prelude.Text,
    -- | The description of the preprocessor that executes on records in this
    -- input before the application\'s code is run.
    inputProcessingConfigurationDescription :: Prelude.Maybe InputProcessingConfigurationDescription,
    -- | The response's http status code.
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
-- 'applicationARN', 'addApplicationInputProcessingConfigurationResponse_applicationARN' - The Amazon Resource Name (ARN) of the application.
--
-- 'applicationVersionId', 'addApplicationInputProcessingConfigurationResponse_applicationVersionId' - Provides the current application version.
--
-- 'inputId', 'addApplicationInputProcessingConfigurationResponse_inputId' - The input ID that is associated with the application input. This is the
-- ID that Kinesis Data Analytics assigns to each input configuration that
-- you add to your application.
--
-- 'inputProcessingConfigurationDescription', 'addApplicationInputProcessingConfigurationResponse_inputProcessingConfigurationDescription' - The description of the preprocessor that executes on records in this
-- input before the application\'s code is run.
--
-- 'httpStatus', 'addApplicationInputProcessingConfigurationResponse_httpStatus' - The response's http status code.
newAddApplicationInputProcessingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddApplicationInputProcessingConfigurationResponse
newAddApplicationInputProcessingConfigurationResponse
  pHttpStatus_ =
    AddApplicationInputProcessingConfigurationResponse'
      { applicationARN =
          Prelude.Nothing,
        applicationVersionId =
          Prelude.Nothing,
        inputId =
          Prelude.Nothing,
        inputProcessingConfigurationDescription =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the application.
addApplicationInputProcessingConfigurationResponse_applicationARN :: Lens.Lens' AddApplicationInputProcessingConfigurationResponse (Prelude.Maybe Prelude.Text)
addApplicationInputProcessingConfigurationResponse_applicationARN = Lens.lens (\AddApplicationInputProcessingConfigurationResponse' {applicationARN} -> applicationARN) (\s@AddApplicationInputProcessingConfigurationResponse' {} a -> s {applicationARN = a} :: AddApplicationInputProcessingConfigurationResponse)

-- | Provides the current application version.
addApplicationInputProcessingConfigurationResponse_applicationVersionId :: Lens.Lens' AddApplicationInputProcessingConfigurationResponse (Prelude.Maybe Prelude.Natural)
addApplicationInputProcessingConfigurationResponse_applicationVersionId = Lens.lens (\AddApplicationInputProcessingConfigurationResponse' {applicationVersionId} -> applicationVersionId) (\s@AddApplicationInputProcessingConfigurationResponse' {} a -> s {applicationVersionId = a} :: AddApplicationInputProcessingConfigurationResponse)

-- | The input ID that is associated with the application input. This is the
-- ID that Kinesis Data Analytics assigns to each input configuration that
-- you add to your application.
addApplicationInputProcessingConfigurationResponse_inputId :: Lens.Lens' AddApplicationInputProcessingConfigurationResponse (Prelude.Maybe Prelude.Text)
addApplicationInputProcessingConfigurationResponse_inputId = Lens.lens (\AddApplicationInputProcessingConfigurationResponse' {inputId} -> inputId) (\s@AddApplicationInputProcessingConfigurationResponse' {} a -> s {inputId = a} :: AddApplicationInputProcessingConfigurationResponse)

-- | The description of the preprocessor that executes on records in this
-- input before the application\'s code is run.
addApplicationInputProcessingConfigurationResponse_inputProcessingConfigurationDescription :: Lens.Lens' AddApplicationInputProcessingConfigurationResponse (Prelude.Maybe InputProcessingConfigurationDescription)
addApplicationInputProcessingConfigurationResponse_inputProcessingConfigurationDescription = Lens.lens (\AddApplicationInputProcessingConfigurationResponse' {inputProcessingConfigurationDescription} -> inputProcessingConfigurationDescription) (\s@AddApplicationInputProcessingConfigurationResponse' {} a -> s {inputProcessingConfigurationDescription = a} :: AddApplicationInputProcessingConfigurationResponse)

-- | The response's http status code.
addApplicationInputProcessingConfigurationResponse_httpStatus :: Lens.Lens' AddApplicationInputProcessingConfigurationResponse Prelude.Int
addApplicationInputProcessingConfigurationResponse_httpStatus = Lens.lens (\AddApplicationInputProcessingConfigurationResponse' {httpStatus} -> httpStatus) (\s@AddApplicationInputProcessingConfigurationResponse' {} a -> s {httpStatus = a} :: AddApplicationInputProcessingConfigurationResponse)

instance
  Prelude.NFData
    AddApplicationInputProcessingConfigurationResponse
  where
  rnf
    AddApplicationInputProcessingConfigurationResponse' {..} =
      Prelude.rnf applicationARN
        `Prelude.seq` Prelude.rnf applicationVersionId
        `Prelude.seq` Prelude.rnf inputId
        `Prelude.seq` Prelude.rnf inputProcessingConfigurationDescription
        `Prelude.seq` Prelude.rnf httpStatus
