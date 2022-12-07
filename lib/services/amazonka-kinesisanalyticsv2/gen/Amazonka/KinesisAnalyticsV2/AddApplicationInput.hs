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
-- Module      : Amazonka.KinesisAnalyticsV2.AddApplicationInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a streaming source to your SQL-based Kinesis Data Analytics
-- application.
--
-- You can add a streaming source when you create an application, or you
-- can use this operation to add a streaming source after you create an
-- application. For more information, see CreateApplication.
--
-- Any configuration update, including adding a streaming source using this
-- operation, results in a new version of the application. You can use the
-- DescribeApplication operation to find the current application version.
module Amazonka.KinesisAnalyticsV2.AddApplicationInput
  ( -- * Creating a Request
    AddApplicationInput (..),
    newAddApplicationInput,

    -- * Request Lenses
    addApplicationInput_applicationName,
    addApplicationInput_currentApplicationVersionId,
    addApplicationInput_input,

    -- * Destructuring the Response
    AddApplicationInputResponse (..),
    newAddApplicationInputResponse,

    -- * Response Lenses
    addApplicationInputResponse_inputDescriptions,
    addApplicationInputResponse_applicationARN,
    addApplicationInputResponse_applicationVersionId,
    addApplicationInputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddApplicationInput' smart constructor.
data AddApplicationInput = AddApplicationInput'
  { -- | The name of your existing application to which you want to add the
    -- streaming source.
    applicationName :: Prelude.Text,
    -- | The current version of your application. You must provide the
    -- @ApplicationVersionID@ or the @ConditionalToken@.You can use the
    -- DescribeApplication operation to find the current application version.
    currentApplicationVersionId :: Prelude.Natural,
    -- | The Input to add.
    input :: Input
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'addApplicationInput_applicationName' - The name of your existing application to which you want to add the
-- streaming source.
--
-- 'currentApplicationVersionId', 'addApplicationInput_currentApplicationVersionId' - The current version of your application. You must provide the
-- @ApplicationVersionID@ or the @ConditionalToken@.You can use the
-- DescribeApplication operation to find the current application version.
--
-- 'input', 'addApplicationInput_input' - The Input to add.
newAddApplicationInput ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  -- | 'input'
  Input ->
  AddApplicationInput
newAddApplicationInput
  pApplicationName_
  pCurrentApplicationVersionId_
  pInput_ =
    AddApplicationInput'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        input = pInput_
      }

-- | The name of your existing application to which you want to add the
-- streaming source.
addApplicationInput_applicationName :: Lens.Lens' AddApplicationInput Prelude.Text
addApplicationInput_applicationName = Lens.lens (\AddApplicationInput' {applicationName} -> applicationName) (\s@AddApplicationInput' {} a -> s {applicationName = a} :: AddApplicationInput)

-- | The current version of your application. You must provide the
-- @ApplicationVersionID@ or the @ConditionalToken@.You can use the
-- DescribeApplication operation to find the current application version.
addApplicationInput_currentApplicationVersionId :: Lens.Lens' AddApplicationInput Prelude.Natural
addApplicationInput_currentApplicationVersionId = Lens.lens (\AddApplicationInput' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@AddApplicationInput' {} a -> s {currentApplicationVersionId = a} :: AddApplicationInput)

-- | The Input to add.
addApplicationInput_input :: Lens.Lens' AddApplicationInput Input
addApplicationInput_input = Lens.lens (\AddApplicationInput' {input} -> input) (\s@AddApplicationInput' {} a -> s {input = a} :: AddApplicationInput)

instance Core.AWSRequest AddApplicationInput where
  type
    AWSResponse AddApplicationInput =
      AddApplicationInputResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddApplicationInputResponse'
            Prelude.<$> ( x Data..?> "InputDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ApplicationARN")
            Prelude.<*> (x Data..?> "ApplicationVersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddApplicationInput where
  hashWithSalt _salt AddApplicationInput' {..} =
    _salt `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` currentApplicationVersionId
      `Prelude.hashWithSalt` input

instance Prelude.NFData AddApplicationInput where
  rnf AddApplicationInput' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf input

instance Data.ToHeaders AddApplicationInput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.AddApplicationInput" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddApplicationInput where
  toJSON AddApplicationInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ( "CurrentApplicationVersionId"
                  Data..= currentApplicationVersionId
              ),
            Prelude.Just ("Input" Data..= input)
          ]
      )

instance Data.ToPath AddApplicationInput where
  toPath = Prelude.const "/"

instance Data.ToQuery AddApplicationInput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddApplicationInputResponse' smart constructor.
data AddApplicationInputResponse = AddApplicationInputResponse'
  { -- | Describes the application input configuration.
    inputDescriptions :: Prelude.Maybe [InputDescription],
    -- | The Amazon Resource Name (ARN) of the application.
    applicationARN :: Prelude.Maybe Prelude.Text,
    -- | Provides the current application version.
    applicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationInputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDescriptions', 'addApplicationInputResponse_inputDescriptions' - Describes the application input configuration.
--
-- 'applicationARN', 'addApplicationInputResponse_applicationARN' - The Amazon Resource Name (ARN) of the application.
--
-- 'applicationVersionId', 'addApplicationInputResponse_applicationVersionId' - Provides the current application version.
--
-- 'httpStatus', 'addApplicationInputResponse_httpStatus' - The response's http status code.
newAddApplicationInputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddApplicationInputResponse
newAddApplicationInputResponse pHttpStatus_ =
  AddApplicationInputResponse'
    { inputDescriptions =
        Prelude.Nothing,
      applicationARN = Prelude.Nothing,
      applicationVersionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the application input configuration.
addApplicationInputResponse_inputDescriptions :: Lens.Lens' AddApplicationInputResponse (Prelude.Maybe [InputDescription])
addApplicationInputResponse_inputDescriptions = Lens.lens (\AddApplicationInputResponse' {inputDescriptions} -> inputDescriptions) (\s@AddApplicationInputResponse' {} a -> s {inputDescriptions = a} :: AddApplicationInputResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the application.
addApplicationInputResponse_applicationARN :: Lens.Lens' AddApplicationInputResponse (Prelude.Maybe Prelude.Text)
addApplicationInputResponse_applicationARN = Lens.lens (\AddApplicationInputResponse' {applicationARN} -> applicationARN) (\s@AddApplicationInputResponse' {} a -> s {applicationARN = a} :: AddApplicationInputResponse)

-- | Provides the current application version.
addApplicationInputResponse_applicationVersionId :: Lens.Lens' AddApplicationInputResponse (Prelude.Maybe Prelude.Natural)
addApplicationInputResponse_applicationVersionId = Lens.lens (\AddApplicationInputResponse' {applicationVersionId} -> applicationVersionId) (\s@AddApplicationInputResponse' {} a -> s {applicationVersionId = a} :: AddApplicationInputResponse)

-- | The response's http status code.
addApplicationInputResponse_httpStatus :: Lens.Lens' AddApplicationInputResponse Prelude.Int
addApplicationInputResponse_httpStatus = Lens.lens (\AddApplicationInputResponse' {httpStatus} -> httpStatus) (\s@AddApplicationInputResponse' {} a -> s {httpStatus = a} :: AddApplicationInputResponse)

instance Prelude.NFData AddApplicationInputResponse where
  rnf AddApplicationInputResponse' {..} =
    Prelude.rnf inputDescriptions
      `Prelude.seq` Prelude.rnf applicationARN
      `Prelude.seq` Prelude.rnf applicationVersionId
      `Prelude.seq` Prelude.rnf httpStatus
