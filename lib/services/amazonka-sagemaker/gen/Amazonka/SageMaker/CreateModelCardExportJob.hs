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
-- Module      : Amazonka.SageMaker.CreateModelCardExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon SageMaker Model Card export job.
module Amazonka.SageMaker.CreateModelCardExportJob
  ( -- * Creating a Request
    CreateModelCardExportJob (..),
    newCreateModelCardExportJob,

    -- * Request Lenses
    createModelCardExportJob_modelCardVersion,
    createModelCardExportJob_modelCardName,
    createModelCardExportJob_modelCardExportJobName,
    createModelCardExportJob_outputConfig,

    -- * Destructuring the Response
    CreateModelCardExportJobResponse (..),
    newCreateModelCardExportJobResponse,

    -- * Response Lenses
    createModelCardExportJobResponse_httpStatus,
    createModelCardExportJobResponse_modelCardExportJobArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateModelCardExportJob' smart constructor.
data CreateModelCardExportJob = CreateModelCardExportJob'
  { -- | The version of the model card to export. If a version is not provided,
    -- then the latest version of the model card is exported.
    modelCardVersion :: Prelude.Maybe Prelude.Int,
    -- | The name of the model card to export.
    modelCardName :: Prelude.Text,
    -- | The name of the model card export job.
    modelCardExportJobName :: Prelude.Text,
    -- | The model card output configuration that specifies the Amazon S3 path
    -- for exporting.
    outputConfig :: ModelCardExportOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelCardExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelCardVersion', 'createModelCardExportJob_modelCardVersion' - The version of the model card to export. If a version is not provided,
-- then the latest version of the model card is exported.
--
-- 'modelCardName', 'createModelCardExportJob_modelCardName' - The name of the model card to export.
--
-- 'modelCardExportJobName', 'createModelCardExportJob_modelCardExportJobName' - The name of the model card export job.
--
-- 'outputConfig', 'createModelCardExportJob_outputConfig' - The model card output configuration that specifies the Amazon S3 path
-- for exporting.
newCreateModelCardExportJob ::
  -- | 'modelCardName'
  Prelude.Text ->
  -- | 'modelCardExportJobName'
  Prelude.Text ->
  -- | 'outputConfig'
  ModelCardExportOutputConfig ->
  CreateModelCardExportJob
newCreateModelCardExportJob
  pModelCardName_
  pModelCardExportJobName_
  pOutputConfig_ =
    CreateModelCardExportJob'
      { modelCardVersion =
          Prelude.Nothing,
        modelCardName = pModelCardName_,
        modelCardExportJobName = pModelCardExportJobName_,
        outputConfig = pOutputConfig_
      }

-- | The version of the model card to export. If a version is not provided,
-- then the latest version of the model card is exported.
createModelCardExportJob_modelCardVersion :: Lens.Lens' CreateModelCardExportJob (Prelude.Maybe Prelude.Int)
createModelCardExportJob_modelCardVersion = Lens.lens (\CreateModelCardExportJob' {modelCardVersion} -> modelCardVersion) (\s@CreateModelCardExportJob' {} a -> s {modelCardVersion = a} :: CreateModelCardExportJob)

-- | The name of the model card to export.
createModelCardExportJob_modelCardName :: Lens.Lens' CreateModelCardExportJob Prelude.Text
createModelCardExportJob_modelCardName = Lens.lens (\CreateModelCardExportJob' {modelCardName} -> modelCardName) (\s@CreateModelCardExportJob' {} a -> s {modelCardName = a} :: CreateModelCardExportJob)

-- | The name of the model card export job.
createModelCardExportJob_modelCardExportJobName :: Lens.Lens' CreateModelCardExportJob Prelude.Text
createModelCardExportJob_modelCardExportJobName = Lens.lens (\CreateModelCardExportJob' {modelCardExportJobName} -> modelCardExportJobName) (\s@CreateModelCardExportJob' {} a -> s {modelCardExportJobName = a} :: CreateModelCardExportJob)

-- | The model card output configuration that specifies the Amazon S3 path
-- for exporting.
createModelCardExportJob_outputConfig :: Lens.Lens' CreateModelCardExportJob ModelCardExportOutputConfig
createModelCardExportJob_outputConfig = Lens.lens (\CreateModelCardExportJob' {outputConfig} -> outputConfig) (\s@CreateModelCardExportJob' {} a -> s {outputConfig = a} :: CreateModelCardExportJob)

instance Core.AWSRequest CreateModelCardExportJob where
  type
    AWSResponse CreateModelCardExportJob =
      CreateModelCardExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelCardExportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ModelCardExportJobArn")
      )

instance Prelude.Hashable CreateModelCardExportJob where
  hashWithSalt _salt CreateModelCardExportJob' {..} =
    _salt
      `Prelude.hashWithSalt` modelCardVersion
      `Prelude.hashWithSalt` modelCardName
      `Prelude.hashWithSalt` modelCardExportJobName
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData CreateModelCardExportJob where
  rnf CreateModelCardExportJob' {..} =
    Prelude.rnf modelCardVersion
      `Prelude.seq` Prelude.rnf modelCardName
      `Prelude.seq` Prelude.rnf modelCardExportJobName
      `Prelude.seq` Prelude.rnf outputConfig

instance Data.ToHeaders CreateModelCardExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateModelCardExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateModelCardExportJob where
  toJSON CreateModelCardExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ModelCardVersion" Data..=)
              Prelude.<$> modelCardVersion,
            Prelude.Just ("ModelCardName" Data..= modelCardName),
            Prelude.Just
              ( "ModelCardExportJobName"
                  Data..= modelCardExportJobName
              ),
            Prelude.Just ("OutputConfig" Data..= outputConfig)
          ]
      )

instance Data.ToPath CreateModelCardExportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateModelCardExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelCardExportJobResponse' smart constructor.
data CreateModelCardExportJobResponse = CreateModelCardExportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the model card export job.
    modelCardExportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelCardExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createModelCardExportJobResponse_httpStatus' - The response's http status code.
--
-- 'modelCardExportJobArn', 'createModelCardExportJobResponse_modelCardExportJobArn' - The Amazon Resource Name (ARN) of the model card export job.
newCreateModelCardExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelCardExportJobArn'
  Prelude.Text ->
  CreateModelCardExportJobResponse
newCreateModelCardExportJobResponse
  pHttpStatus_
  pModelCardExportJobArn_ =
    CreateModelCardExportJobResponse'
      { httpStatus =
          pHttpStatus_,
        modelCardExportJobArn =
          pModelCardExportJobArn_
      }

-- | The response's http status code.
createModelCardExportJobResponse_httpStatus :: Lens.Lens' CreateModelCardExportJobResponse Prelude.Int
createModelCardExportJobResponse_httpStatus = Lens.lens (\CreateModelCardExportJobResponse' {httpStatus} -> httpStatus) (\s@CreateModelCardExportJobResponse' {} a -> s {httpStatus = a} :: CreateModelCardExportJobResponse)

-- | The Amazon Resource Name (ARN) of the model card export job.
createModelCardExportJobResponse_modelCardExportJobArn :: Lens.Lens' CreateModelCardExportJobResponse Prelude.Text
createModelCardExportJobResponse_modelCardExportJobArn = Lens.lens (\CreateModelCardExportJobResponse' {modelCardExportJobArn} -> modelCardExportJobArn) (\s@CreateModelCardExportJobResponse' {} a -> s {modelCardExportJobArn = a} :: CreateModelCardExportJobResponse)

instance
  Prelude.NFData
    CreateModelCardExportJobResponse
  where
  rnf CreateModelCardExportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelCardExportJobArn
