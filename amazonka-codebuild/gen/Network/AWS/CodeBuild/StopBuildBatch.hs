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
-- Module      : Network.AWS.CodeBuild.StopBuildBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running batch build.
module Network.AWS.CodeBuild.StopBuildBatch
  ( -- * Creating a Request
    StopBuildBatch (..),
    newStopBuildBatch,

    -- * Request Lenses
    stopBuildBatch_id,

    -- * Destructuring the Response
    StopBuildBatchResponse (..),
    newStopBuildBatchResponse,

    -- * Response Lenses
    stopBuildBatchResponse_buildBatch,
    stopBuildBatchResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopBuildBatch' smart constructor.
data StopBuildBatch = StopBuildBatch'
  { -- | The identifier of the batch build to stop.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBuildBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'stopBuildBatch_id' - The identifier of the batch build to stop.
newStopBuildBatch ::
  -- | 'id'
  Prelude.Text ->
  StopBuildBatch
newStopBuildBatch pId_ = StopBuildBatch' {id = pId_}

-- | The identifier of the batch build to stop.
stopBuildBatch_id :: Lens.Lens' StopBuildBatch Prelude.Text
stopBuildBatch_id = Lens.lens (\StopBuildBatch' {id} -> id) (\s@StopBuildBatch' {} a -> s {id = a} :: StopBuildBatch)

instance Core.AWSRequest StopBuildBatch where
  type
    AWSResponse StopBuildBatch =
      StopBuildBatchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopBuildBatchResponse'
            Prelude.<$> (x Core..?> "buildBatch")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopBuildBatch

instance Prelude.NFData StopBuildBatch

instance Core.ToHeaders StopBuildBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.StopBuildBatch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopBuildBatch where
  toJSON StopBuildBatch' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath StopBuildBatch where
  toPath = Prelude.const "/"

instance Core.ToQuery StopBuildBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopBuildBatchResponse' smart constructor.
data StopBuildBatchResponse = StopBuildBatchResponse'
  { buildBatch :: Prelude.Maybe BuildBatch,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBuildBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildBatch', 'stopBuildBatchResponse_buildBatch' - Undocumented member.
--
-- 'httpStatus', 'stopBuildBatchResponse_httpStatus' - The response's http status code.
newStopBuildBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopBuildBatchResponse
newStopBuildBatchResponse pHttpStatus_ =
  StopBuildBatchResponse'
    { buildBatch =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
stopBuildBatchResponse_buildBatch :: Lens.Lens' StopBuildBatchResponse (Prelude.Maybe BuildBatch)
stopBuildBatchResponse_buildBatch = Lens.lens (\StopBuildBatchResponse' {buildBatch} -> buildBatch) (\s@StopBuildBatchResponse' {} a -> s {buildBatch = a} :: StopBuildBatchResponse)

-- | The response's http status code.
stopBuildBatchResponse_httpStatus :: Lens.Lens' StopBuildBatchResponse Prelude.Int
stopBuildBatchResponse_httpStatus = Lens.lens (\StopBuildBatchResponse' {httpStatus} -> httpStatus) (\s@StopBuildBatchResponse' {} a -> s {httpStatus = a} :: StopBuildBatchResponse)

instance Prelude.NFData StopBuildBatchResponse
