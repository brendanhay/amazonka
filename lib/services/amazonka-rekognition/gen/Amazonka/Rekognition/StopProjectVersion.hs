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
-- Module      : Amazonka.Rekognition.StopProjectVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running model. The operation might take a while to complete. To
-- check the current status, call DescribeProjectVersions.
module Amazonka.Rekognition.StopProjectVersion
  ( -- * Creating a Request
    StopProjectVersion (..),
    newStopProjectVersion,

    -- * Request Lenses
    stopProjectVersion_projectVersionArn,

    -- * Destructuring the Response
    StopProjectVersionResponse (..),
    newStopProjectVersionResponse,

    -- * Response Lenses
    stopProjectVersionResponse_status,
    stopProjectVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopProjectVersion' smart constructor.
data StopProjectVersion = StopProjectVersion'
  { -- | The Amazon Resource Name (ARN) of the model version that you want to
    -- delete.
    --
    -- This operation requires permissions to perform the
    -- @rekognition:StopProjectVersion@ action.
    projectVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopProjectVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectVersionArn', 'stopProjectVersion_projectVersionArn' - The Amazon Resource Name (ARN) of the model version that you want to
-- delete.
--
-- This operation requires permissions to perform the
-- @rekognition:StopProjectVersion@ action.
newStopProjectVersion ::
  -- | 'projectVersionArn'
  Prelude.Text ->
  StopProjectVersion
newStopProjectVersion pProjectVersionArn_ =
  StopProjectVersion'
    { projectVersionArn =
        pProjectVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the model version that you want to
-- delete.
--
-- This operation requires permissions to perform the
-- @rekognition:StopProjectVersion@ action.
stopProjectVersion_projectVersionArn :: Lens.Lens' StopProjectVersion Prelude.Text
stopProjectVersion_projectVersionArn = Lens.lens (\StopProjectVersion' {projectVersionArn} -> projectVersionArn) (\s@StopProjectVersion' {} a -> s {projectVersionArn = a} :: StopProjectVersion)

instance Core.AWSRequest StopProjectVersion where
  type
    AWSResponse StopProjectVersion =
      StopProjectVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopProjectVersionResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopProjectVersion where
  hashWithSalt _salt StopProjectVersion' {..} =
    _salt `Prelude.hashWithSalt` projectVersionArn

instance Prelude.NFData StopProjectVersion where
  rnf StopProjectVersion' {..} =
    Prelude.rnf projectVersionArn

instance Core.ToHeaders StopProjectVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.StopProjectVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopProjectVersion where
  toJSON StopProjectVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProjectVersionArn" Core..= projectVersionArn)
          ]
      )

instance Core.ToPath StopProjectVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery StopProjectVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopProjectVersionResponse' smart constructor.
data StopProjectVersionResponse = StopProjectVersionResponse'
  { -- | The current status of the stop operation.
    status :: Prelude.Maybe ProjectVersionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopProjectVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'stopProjectVersionResponse_status' - The current status of the stop operation.
--
-- 'httpStatus', 'stopProjectVersionResponse_httpStatus' - The response's http status code.
newStopProjectVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopProjectVersionResponse
newStopProjectVersionResponse pHttpStatus_ =
  StopProjectVersionResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the stop operation.
stopProjectVersionResponse_status :: Lens.Lens' StopProjectVersionResponse (Prelude.Maybe ProjectVersionStatus)
stopProjectVersionResponse_status = Lens.lens (\StopProjectVersionResponse' {status} -> status) (\s@StopProjectVersionResponse' {} a -> s {status = a} :: StopProjectVersionResponse)

-- | The response's http status code.
stopProjectVersionResponse_httpStatus :: Lens.Lens' StopProjectVersionResponse Prelude.Int
stopProjectVersionResponse_httpStatus = Lens.lens (\StopProjectVersionResponse' {httpStatus} -> httpStatus) (\s@StopProjectVersionResponse' {} a -> s {httpStatus = a} :: StopProjectVersionResponse)

instance Prelude.NFData StopProjectVersionResponse where
  rnf StopProjectVersionResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
