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
-- Module      : Amazonka.Rekognition.StartProjectVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the running of the version of a model. Starting a model takes a
-- while to complete. To check the current state of the model, use
-- DescribeProjectVersions.
--
-- Once the model is running, you can detect custom labels in new images by
-- calling DetectCustomLabels.
--
-- You are charged for the amount of time that the model is running. To
-- stop a running model, call StopProjectVersion.
--
-- For more information, see /Running a trained Amazon Rekognition Custom
-- Labels model/ in the Amazon Rekognition Custom Labels Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:StartProjectVersion@ action.
module Amazonka.Rekognition.StartProjectVersion
  ( -- * Creating a Request
    StartProjectVersion (..),
    newStartProjectVersion,

    -- * Request Lenses
    startProjectVersion_maxInferenceUnits,
    startProjectVersion_projectVersionArn,
    startProjectVersion_minInferenceUnits,

    -- * Destructuring the Response
    StartProjectVersionResponse (..),
    newStartProjectVersionResponse,

    -- * Response Lenses
    startProjectVersionResponse_status,
    startProjectVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartProjectVersion' smart constructor.
data StartProjectVersion = StartProjectVersion'
  { -- | The maximum number of inference units to use for auto-scaling the model.
    -- If you don\'t specify a value, Amazon Rekognition Custom Labels doesn\'t
    -- auto-scale the model.
    maxInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name(ARN) of the model version that you want to
    -- start.
    projectVersionArn :: Prelude.Text,
    -- | The minimum number of inference units to use. A single inference unit
    -- represents 1 hour of processing.
    --
    -- For information about the number of transactions per second (TPS) that
    -- an inference unit can support, see /Running a trained Amazon Rekognition
    -- Custom Labels model/ in the Amazon Rekognition Custom Labels Guide.
    --
    -- Use a higher number to increase the TPS throughput of your model. You
    -- are charged for the number of inference units that you use.
    minInferenceUnits :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartProjectVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxInferenceUnits', 'startProjectVersion_maxInferenceUnits' - The maximum number of inference units to use for auto-scaling the model.
-- If you don\'t specify a value, Amazon Rekognition Custom Labels doesn\'t
-- auto-scale the model.
--
-- 'projectVersionArn', 'startProjectVersion_projectVersionArn' - The Amazon Resource Name(ARN) of the model version that you want to
-- start.
--
-- 'minInferenceUnits', 'startProjectVersion_minInferenceUnits' - The minimum number of inference units to use. A single inference unit
-- represents 1 hour of processing.
--
-- For information about the number of transactions per second (TPS) that
-- an inference unit can support, see /Running a trained Amazon Rekognition
-- Custom Labels model/ in the Amazon Rekognition Custom Labels Guide.
--
-- Use a higher number to increase the TPS throughput of your model. You
-- are charged for the number of inference units that you use.
newStartProjectVersion ::
  -- | 'projectVersionArn'
  Prelude.Text ->
  -- | 'minInferenceUnits'
  Prelude.Natural ->
  StartProjectVersion
newStartProjectVersion
  pProjectVersionArn_
  pMinInferenceUnits_ =
    StartProjectVersion'
      { maxInferenceUnits =
          Prelude.Nothing,
        projectVersionArn = pProjectVersionArn_,
        minInferenceUnits = pMinInferenceUnits_
      }

-- | The maximum number of inference units to use for auto-scaling the model.
-- If you don\'t specify a value, Amazon Rekognition Custom Labels doesn\'t
-- auto-scale the model.
startProjectVersion_maxInferenceUnits :: Lens.Lens' StartProjectVersion (Prelude.Maybe Prelude.Natural)
startProjectVersion_maxInferenceUnits = Lens.lens (\StartProjectVersion' {maxInferenceUnits} -> maxInferenceUnits) (\s@StartProjectVersion' {} a -> s {maxInferenceUnits = a} :: StartProjectVersion)

-- | The Amazon Resource Name(ARN) of the model version that you want to
-- start.
startProjectVersion_projectVersionArn :: Lens.Lens' StartProjectVersion Prelude.Text
startProjectVersion_projectVersionArn = Lens.lens (\StartProjectVersion' {projectVersionArn} -> projectVersionArn) (\s@StartProjectVersion' {} a -> s {projectVersionArn = a} :: StartProjectVersion)

-- | The minimum number of inference units to use. A single inference unit
-- represents 1 hour of processing.
--
-- For information about the number of transactions per second (TPS) that
-- an inference unit can support, see /Running a trained Amazon Rekognition
-- Custom Labels model/ in the Amazon Rekognition Custom Labels Guide.
--
-- Use a higher number to increase the TPS throughput of your model. You
-- are charged for the number of inference units that you use.
startProjectVersion_minInferenceUnits :: Lens.Lens' StartProjectVersion Prelude.Natural
startProjectVersion_minInferenceUnits = Lens.lens (\StartProjectVersion' {minInferenceUnits} -> minInferenceUnits) (\s@StartProjectVersion' {} a -> s {minInferenceUnits = a} :: StartProjectVersion)

instance Core.AWSRequest StartProjectVersion where
  type
    AWSResponse StartProjectVersion =
      StartProjectVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartProjectVersionResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartProjectVersion where
  hashWithSalt _salt StartProjectVersion' {..} =
    _salt `Prelude.hashWithSalt` maxInferenceUnits
      `Prelude.hashWithSalt` projectVersionArn
      `Prelude.hashWithSalt` minInferenceUnits

instance Prelude.NFData StartProjectVersion where
  rnf StartProjectVersion' {..} =
    Prelude.rnf maxInferenceUnits
      `Prelude.seq` Prelude.rnf projectVersionArn
      `Prelude.seq` Prelude.rnf minInferenceUnits

instance Data.ToHeaders StartProjectVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.StartProjectVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartProjectVersion where
  toJSON StartProjectVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxInferenceUnits" Data..=)
              Prelude.<$> maxInferenceUnits,
            Prelude.Just
              ("ProjectVersionArn" Data..= projectVersionArn),
            Prelude.Just
              ("MinInferenceUnits" Data..= minInferenceUnits)
          ]
      )

instance Data.ToPath StartProjectVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery StartProjectVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartProjectVersionResponse' smart constructor.
data StartProjectVersionResponse = StartProjectVersionResponse'
  { -- | The current running status of the model.
    status :: Prelude.Maybe ProjectVersionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartProjectVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'startProjectVersionResponse_status' - The current running status of the model.
--
-- 'httpStatus', 'startProjectVersionResponse_httpStatus' - The response's http status code.
newStartProjectVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartProjectVersionResponse
newStartProjectVersionResponse pHttpStatus_ =
  StartProjectVersionResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current running status of the model.
startProjectVersionResponse_status :: Lens.Lens' StartProjectVersionResponse (Prelude.Maybe ProjectVersionStatus)
startProjectVersionResponse_status = Lens.lens (\StartProjectVersionResponse' {status} -> status) (\s@StartProjectVersionResponse' {} a -> s {status = a} :: StartProjectVersionResponse)

-- | The response's http status code.
startProjectVersionResponse_httpStatus :: Lens.Lens' StartProjectVersionResponse Prelude.Int
startProjectVersionResponse_httpStatus = Lens.lens (\StartProjectVersionResponse' {httpStatus} -> httpStatus) (\s@StartProjectVersionResponse' {} a -> s {httpStatus = a} :: StartProjectVersionResponse)

instance Prelude.NFData StartProjectVersionResponse where
  rnf StartProjectVersionResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
