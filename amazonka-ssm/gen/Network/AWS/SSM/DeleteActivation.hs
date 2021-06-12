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
-- Module      : Network.AWS.SSM.DeleteActivation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activation. You are not required to delete an activation. If
-- you delete an activation, you can no longer use it to register
-- additional managed instances. Deleting an activation does not
-- de-register managed instances. You must manually de-register managed
-- instances.
module Network.AWS.SSM.DeleteActivation
  ( -- * Creating a Request
    DeleteActivation (..),
    newDeleteActivation,

    -- * Request Lenses
    deleteActivation_activationId,

    -- * Destructuring the Response
    DeleteActivationResponse (..),
    newDeleteActivationResponse,

    -- * Response Lenses
    deleteActivationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeleteActivation' smart constructor.
data DeleteActivation = DeleteActivation'
  { -- | The ID of the activation that you want to delete.
    activationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteActivation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activationId', 'deleteActivation_activationId' - The ID of the activation that you want to delete.
newDeleteActivation ::
  -- | 'activationId'
  Core.Text ->
  DeleteActivation
newDeleteActivation pActivationId_ =
  DeleteActivation' {activationId = pActivationId_}

-- | The ID of the activation that you want to delete.
deleteActivation_activationId :: Lens.Lens' DeleteActivation Core.Text
deleteActivation_activationId = Lens.lens (\DeleteActivation' {activationId} -> activationId) (\s@DeleteActivation' {} a -> s {activationId = a} :: DeleteActivation)

instance Core.AWSRequest DeleteActivation where
  type
    AWSResponse DeleteActivation =
      DeleteActivationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteActivationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteActivation

instance Core.NFData DeleteActivation

instance Core.ToHeaders DeleteActivation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DeleteActivation" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteActivation where
  toJSON DeleteActivation' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ActivationId" Core..= activationId)]
      )

instance Core.ToPath DeleteActivation where
  toPath = Core.const "/"

instance Core.ToQuery DeleteActivation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteActivationResponse' smart constructor.
data DeleteActivationResponse = DeleteActivationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteActivationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteActivationResponse_httpStatus' - The response's http status code.
newDeleteActivationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteActivationResponse
newDeleteActivationResponse pHttpStatus_ =
  DeleteActivationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteActivationResponse_httpStatus :: Lens.Lens' DeleteActivationResponse Core.Int
deleteActivationResponse_httpStatus = Lens.lens (\DeleteActivationResponse' {httpStatus} -> httpStatus) (\s@DeleteActivationResponse' {} a -> s {httpStatus = a} :: DeleteActivationResponse)

instance Core.NFData DeleteActivationResponse
