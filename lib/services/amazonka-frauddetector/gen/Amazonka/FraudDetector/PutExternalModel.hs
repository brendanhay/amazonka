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
-- Module      : Amazonka.FraudDetector.PutExternalModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an Amazon SageMaker model endpoint. You can also use
-- this action to update the configuration of the model endpoint, including
-- the IAM role and\/or the mapped variables.
module Amazonka.FraudDetector.PutExternalModel
  ( -- * Creating a Request
    PutExternalModel (..),
    newPutExternalModel,

    -- * Request Lenses
    putExternalModel_tags,
    putExternalModel_modelEndpoint,
    putExternalModel_modelSource,
    putExternalModel_invokeModelEndpointRoleArn,
    putExternalModel_inputConfiguration,
    putExternalModel_outputConfiguration,
    putExternalModel_modelEndpointStatus,

    -- * Destructuring the Response
    PutExternalModelResponse (..),
    newPutExternalModelResponse,

    -- * Response Lenses
    putExternalModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutExternalModel' smart constructor.
data PutExternalModel = PutExternalModel'
  { -- | A collection of key and value pairs.
    tags :: Prelude.Maybe [Tag],
    -- | The model endpoints name.
    modelEndpoint :: Prelude.Text,
    -- | The source of the model.
    modelSource :: ModelSource,
    -- | The IAM role used to invoke the model endpoint.
    invokeModelEndpointRoleArn :: Prelude.Text,
    -- | The model endpoint input configuration.
    inputConfiguration :: ModelInputConfiguration,
    -- | The model endpoint output configuration.
    outputConfiguration :: ModelOutputConfiguration,
    -- | The model endpoint’s status in Amazon Fraud Detector.
    modelEndpointStatus :: ModelEndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutExternalModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putExternalModel_tags' - A collection of key and value pairs.
--
-- 'modelEndpoint', 'putExternalModel_modelEndpoint' - The model endpoints name.
--
-- 'modelSource', 'putExternalModel_modelSource' - The source of the model.
--
-- 'invokeModelEndpointRoleArn', 'putExternalModel_invokeModelEndpointRoleArn' - The IAM role used to invoke the model endpoint.
--
-- 'inputConfiguration', 'putExternalModel_inputConfiguration' - The model endpoint input configuration.
--
-- 'outputConfiguration', 'putExternalModel_outputConfiguration' - The model endpoint output configuration.
--
-- 'modelEndpointStatus', 'putExternalModel_modelEndpointStatus' - The model endpoint’s status in Amazon Fraud Detector.
newPutExternalModel ::
  -- | 'modelEndpoint'
  Prelude.Text ->
  -- | 'modelSource'
  ModelSource ->
  -- | 'invokeModelEndpointRoleArn'
  Prelude.Text ->
  -- | 'inputConfiguration'
  ModelInputConfiguration ->
  -- | 'outputConfiguration'
  ModelOutputConfiguration ->
  -- | 'modelEndpointStatus'
  ModelEndpointStatus ->
  PutExternalModel
newPutExternalModel
  pModelEndpoint_
  pModelSource_
  pInvokeModelEndpointRoleArn_
  pInputConfiguration_
  pOutputConfiguration_
  pModelEndpointStatus_ =
    PutExternalModel'
      { tags = Prelude.Nothing,
        modelEndpoint = pModelEndpoint_,
        modelSource = pModelSource_,
        invokeModelEndpointRoleArn =
          pInvokeModelEndpointRoleArn_,
        inputConfiguration = pInputConfiguration_,
        outputConfiguration = pOutputConfiguration_,
        modelEndpointStatus = pModelEndpointStatus_
      }

-- | A collection of key and value pairs.
putExternalModel_tags :: Lens.Lens' PutExternalModel (Prelude.Maybe [Tag])
putExternalModel_tags = Lens.lens (\PutExternalModel' {tags} -> tags) (\s@PutExternalModel' {} a -> s {tags = a} :: PutExternalModel) Prelude.. Lens.mapping Lens.coerced

-- | The model endpoints name.
putExternalModel_modelEndpoint :: Lens.Lens' PutExternalModel Prelude.Text
putExternalModel_modelEndpoint = Lens.lens (\PutExternalModel' {modelEndpoint} -> modelEndpoint) (\s@PutExternalModel' {} a -> s {modelEndpoint = a} :: PutExternalModel)

-- | The source of the model.
putExternalModel_modelSource :: Lens.Lens' PutExternalModel ModelSource
putExternalModel_modelSource = Lens.lens (\PutExternalModel' {modelSource} -> modelSource) (\s@PutExternalModel' {} a -> s {modelSource = a} :: PutExternalModel)

-- | The IAM role used to invoke the model endpoint.
putExternalModel_invokeModelEndpointRoleArn :: Lens.Lens' PutExternalModel Prelude.Text
putExternalModel_invokeModelEndpointRoleArn = Lens.lens (\PutExternalModel' {invokeModelEndpointRoleArn} -> invokeModelEndpointRoleArn) (\s@PutExternalModel' {} a -> s {invokeModelEndpointRoleArn = a} :: PutExternalModel)

-- | The model endpoint input configuration.
putExternalModel_inputConfiguration :: Lens.Lens' PutExternalModel ModelInputConfiguration
putExternalModel_inputConfiguration = Lens.lens (\PutExternalModel' {inputConfiguration} -> inputConfiguration) (\s@PutExternalModel' {} a -> s {inputConfiguration = a} :: PutExternalModel)

-- | The model endpoint output configuration.
putExternalModel_outputConfiguration :: Lens.Lens' PutExternalModel ModelOutputConfiguration
putExternalModel_outputConfiguration = Lens.lens (\PutExternalModel' {outputConfiguration} -> outputConfiguration) (\s@PutExternalModel' {} a -> s {outputConfiguration = a} :: PutExternalModel)

-- | The model endpoint’s status in Amazon Fraud Detector.
putExternalModel_modelEndpointStatus :: Lens.Lens' PutExternalModel ModelEndpointStatus
putExternalModel_modelEndpointStatus = Lens.lens (\PutExternalModel' {modelEndpointStatus} -> modelEndpointStatus) (\s@PutExternalModel' {} a -> s {modelEndpointStatus = a} :: PutExternalModel)

instance Core.AWSRequest PutExternalModel where
  type
    AWSResponse PutExternalModel =
      PutExternalModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutExternalModelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutExternalModel where
  hashWithSalt _salt PutExternalModel' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` modelEndpoint
      `Prelude.hashWithSalt` modelSource
      `Prelude.hashWithSalt` invokeModelEndpointRoleArn
      `Prelude.hashWithSalt` inputConfiguration
      `Prelude.hashWithSalt` outputConfiguration
      `Prelude.hashWithSalt` modelEndpointStatus

instance Prelude.NFData PutExternalModel where
  rnf PutExternalModel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf modelEndpoint
      `Prelude.seq` Prelude.rnf modelSource
      `Prelude.seq` Prelude.rnf invokeModelEndpointRoleArn
      `Prelude.seq` Prelude.rnf inputConfiguration
      `Prelude.seq` Prelude.rnf outputConfiguration
      `Prelude.seq` Prelude.rnf modelEndpointStatus

instance Data.ToHeaders PutExternalModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.PutExternalModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutExternalModel where
  toJSON PutExternalModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("modelEndpoint" Data..= modelEndpoint),
            Prelude.Just ("modelSource" Data..= modelSource),
            Prelude.Just
              ( "invokeModelEndpointRoleArn"
                  Data..= invokeModelEndpointRoleArn
              ),
            Prelude.Just
              ("inputConfiguration" Data..= inputConfiguration),
            Prelude.Just
              ("outputConfiguration" Data..= outputConfiguration),
            Prelude.Just
              ("modelEndpointStatus" Data..= modelEndpointStatus)
          ]
      )

instance Data.ToPath PutExternalModel where
  toPath = Prelude.const "/"

instance Data.ToQuery PutExternalModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutExternalModelResponse' smart constructor.
data PutExternalModelResponse = PutExternalModelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutExternalModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putExternalModelResponse_httpStatus' - The response's http status code.
newPutExternalModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutExternalModelResponse
newPutExternalModelResponse pHttpStatus_ =
  PutExternalModelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putExternalModelResponse_httpStatus :: Lens.Lens' PutExternalModelResponse Prelude.Int
putExternalModelResponse_httpStatus = Lens.lens (\PutExternalModelResponse' {httpStatus} -> httpStatus) (\s@PutExternalModelResponse' {} a -> s {httpStatus = a} :: PutExternalModelResponse)

instance Prelude.NFData PutExternalModelResponse where
  rnf PutExternalModelResponse' {..} =
    Prelude.rnf httpStatus
