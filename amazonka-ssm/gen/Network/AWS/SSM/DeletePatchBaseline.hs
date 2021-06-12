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
-- Module      : Network.AWS.SSM.DeletePatchBaseline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a patch baseline.
module Network.AWS.SSM.DeletePatchBaseline
  ( -- * Creating a Request
    DeletePatchBaseline (..),
    newDeletePatchBaseline,

    -- * Request Lenses
    deletePatchBaseline_baselineId,

    -- * Destructuring the Response
    DeletePatchBaselineResponse (..),
    newDeletePatchBaselineResponse,

    -- * Response Lenses
    deletePatchBaselineResponse_baselineId,
    deletePatchBaselineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeletePatchBaseline' smart constructor.
data DeletePatchBaseline = DeletePatchBaseline'
  { -- | The ID of the patch baseline to delete.
    baselineId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'deletePatchBaseline_baselineId' - The ID of the patch baseline to delete.
newDeletePatchBaseline ::
  -- | 'baselineId'
  Core.Text ->
  DeletePatchBaseline
newDeletePatchBaseline pBaselineId_ =
  DeletePatchBaseline' {baselineId = pBaselineId_}

-- | The ID of the patch baseline to delete.
deletePatchBaseline_baselineId :: Lens.Lens' DeletePatchBaseline Core.Text
deletePatchBaseline_baselineId = Lens.lens (\DeletePatchBaseline' {baselineId} -> baselineId) (\s@DeletePatchBaseline' {} a -> s {baselineId = a} :: DeletePatchBaseline)

instance Core.AWSRequest DeletePatchBaseline where
  type
    AWSResponse DeletePatchBaseline =
      DeletePatchBaselineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePatchBaselineResponse'
            Core.<$> (x Core..?> "BaselineId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeletePatchBaseline

instance Core.NFData DeletePatchBaseline

instance Core.ToHeaders DeletePatchBaseline where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DeletePatchBaseline" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeletePatchBaseline where
  toJSON DeletePatchBaseline' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("BaselineId" Core..= baselineId)]
      )

instance Core.ToPath DeletePatchBaseline where
  toPath = Core.const "/"

instance Core.ToQuery DeletePatchBaseline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePatchBaselineResponse' smart constructor.
data DeletePatchBaselineResponse = DeletePatchBaselineResponse'
  { -- | The ID of the deleted patch baseline.
    baselineId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePatchBaselineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'deletePatchBaselineResponse_baselineId' - The ID of the deleted patch baseline.
--
-- 'httpStatus', 'deletePatchBaselineResponse_httpStatus' - The response's http status code.
newDeletePatchBaselineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeletePatchBaselineResponse
newDeletePatchBaselineResponse pHttpStatus_ =
  DeletePatchBaselineResponse'
    { baselineId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deleted patch baseline.
deletePatchBaselineResponse_baselineId :: Lens.Lens' DeletePatchBaselineResponse (Core.Maybe Core.Text)
deletePatchBaselineResponse_baselineId = Lens.lens (\DeletePatchBaselineResponse' {baselineId} -> baselineId) (\s@DeletePatchBaselineResponse' {} a -> s {baselineId = a} :: DeletePatchBaselineResponse)

-- | The response's http status code.
deletePatchBaselineResponse_httpStatus :: Lens.Lens' DeletePatchBaselineResponse Core.Int
deletePatchBaselineResponse_httpStatus = Lens.lens (\DeletePatchBaselineResponse' {httpStatus} -> httpStatus) (\s@DeletePatchBaselineResponse' {} a -> s {httpStatus = a} :: DeletePatchBaselineResponse)

instance Core.NFData DeletePatchBaselineResponse
