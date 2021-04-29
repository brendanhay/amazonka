{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeletePatchBaseline' smart constructor.
data DeletePatchBaseline = DeletePatchBaseline'
  { -- | The ID of the patch baseline to delete.
    baselineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeletePatchBaseline
newDeletePatchBaseline pBaselineId_ =
  DeletePatchBaseline' {baselineId = pBaselineId_}

-- | The ID of the patch baseline to delete.
deletePatchBaseline_baselineId :: Lens.Lens' DeletePatchBaseline Prelude.Text
deletePatchBaseline_baselineId = Lens.lens (\DeletePatchBaseline' {baselineId} -> baselineId) (\s@DeletePatchBaseline' {} a -> s {baselineId = a} :: DeletePatchBaseline)

instance Prelude.AWSRequest DeletePatchBaseline where
  type
    Rs DeletePatchBaseline =
      DeletePatchBaselineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePatchBaselineResponse'
            Prelude.<$> (x Prelude..?> "BaselineId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePatchBaseline

instance Prelude.NFData DeletePatchBaseline

instance Prelude.ToHeaders DeletePatchBaseline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DeletePatchBaseline" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeletePatchBaseline where
  toJSON DeletePatchBaseline' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("BaselineId" Prelude..= baselineId)]
      )

instance Prelude.ToPath DeletePatchBaseline where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePatchBaseline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePatchBaselineResponse' smart constructor.
data DeletePatchBaselineResponse = DeletePatchBaselineResponse'
  { -- | The ID of the deleted patch baseline.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeletePatchBaselineResponse
newDeletePatchBaselineResponse pHttpStatus_ =
  DeletePatchBaselineResponse'
    { baselineId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deleted patch baseline.
deletePatchBaselineResponse_baselineId :: Lens.Lens' DeletePatchBaselineResponse (Prelude.Maybe Prelude.Text)
deletePatchBaselineResponse_baselineId = Lens.lens (\DeletePatchBaselineResponse' {baselineId} -> baselineId) (\s@DeletePatchBaselineResponse' {} a -> s {baselineId = a} :: DeletePatchBaselineResponse)

-- | The response's http status code.
deletePatchBaselineResponse_httpStatus :: Lens.Lens' DeletePatchBaselineResponse Prelude.Int
deletePatchBaselineResponse_httpStatus = Lens.lens (\DeletePatchBaselineResponse' {httpStatus} -> httpStatus) (\s@DeletePatchBaselineResponse' {} a -> s {httpStatus = a} :: DeletePatchBaselineResponse)

instance Prelude.NFData DeletePatchBaselineResponse
