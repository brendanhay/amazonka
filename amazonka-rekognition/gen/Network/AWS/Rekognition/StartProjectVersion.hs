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
-- Module      : Network.AWS.Rekognition.StartProjectVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- This operation requires permissions to perform the
-- @rekognition:StartProjectVersion@ action.
module Network.AWS.Rekognition.StartProjectVersion
  ( -- * Creating a Request
    StartProjectVersion (..),
    newStartProjectVersion,

    -- * Request Lenses
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartProjectVersion' smart constructor.
data StartProjectVersion = StartProjectVersion'
  { -- | The Amazon Resource Name(ARN) of the model version that you want to
    -- start.
    projectVersionArn :: Prelude.Text,
    -- | The minimum number of inference units to use. A single inference unit
    -- represents 1 hour of processing and can support up to 5 Transaction Pers
    -- Second (TPS). Use a higher number to increase the TPS throughput of your
    -- model. You are charged for the number of inference units that you use.
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
-- 'projectVersionArn', 'startProjectVersion_projectVersionArn' - The Amazon Resource Name(ARN) of the model version that you want to
-- start.
--
-- 'minInferenceUnits', 'startProjectVersion_minInferenceUnits' - The minimum number of inference units to use. A single inference unit
-- represents 1 hour of processing and can support up to 5 Transaction Pers
-- Second (TPS). Use a higher number to increase the TPS throughput of your
-- model. You are charged for the number of inference units that you use.
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
      { projectVersionArn =
          pProjectVersionArn_,
        minInferenceUnits = pMinInferenceUnits_
      }

-- | The Amazon Resource Name(ARN) of the model version that you want to
-- start.
startProjectVersion_projectVersionArn :: Lens.Lens' StartProjectVersion Prelude.Text
startProjectVersion_projectVersionArn = Lens.lens (\StartProjectVersion' {projectVersionArn} -> projectVersionArn) (\s@StartProjectVersion' {} a -> s {projectVersionArn = a} :: StartProjectVersion)

-- | The minimum number of inference units to use. A single inference unit
-- represents 1 hour of processing and can support up to 5 Transaction Pers
-- Second (TPS). Use a higher number to increase the TPS throughput of your
-- model. You are charged for the number of inference units that you use.
startProjectVersion_minInferenceUnits :: Lens.Lens' StartProjectVersion Prelude.Natural
startProjectVersion_minInferenceUnits = Lens.lens (\StartProjectVersion' {minInferenceUnits} -> minInferenceUnits) (\s@StartProjectVersion' {} a -> s {minInferenceUnits = a} :: StartProjectVersion)

instance Core.AWSRequest StartProjectVersion where
  type
    AWSResponse StartProjectVersion =
      StartProjectVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartProjectVersionResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartProjectVersion

instance Prelude.NFData StartProjectVersion

instance Core.ToHeaders StartProjectVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.StartProjectVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartProjectVersion where
  toJSON StartProjectVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProjectVersionArn" Core..= projectVersionArn),
            Prelude.Just
              ("MinInferenceUnits" Core..= minInferenceUnits)
          ]
      )

instance Core.ToPath StartProjectVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery StartProjectVersion where
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

instance Prelude.NFData StartProjectVersionResponse
