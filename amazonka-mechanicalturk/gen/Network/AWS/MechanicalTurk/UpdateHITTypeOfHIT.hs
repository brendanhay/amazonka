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
-- Module      : Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITTypeOfHIT@ operation allows you to change the HITType
-- properties of a HIT. This operation disassociates the HIT from its old
-- HITType properties and associates it with the new HITType properties.
-- The HIT takes on the properties of the new HITType in place of the old
-- ones.
module Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
  ( -- * Creating a Request
    UpdateHITTypeOfHIT (..),
    newUpdateHITTypeOfHIT,

    -- * Request Lenses
    updateHITTypeOfHIT_hITId,
    updateHITTypeOfHIT_hITTypeId,

    -- * Destructuring the Response
    UpdateHITTypeOfHITResponse (..),
    newUpdateHITTypeOfHITResponse,

    -- * Response Lenses
    updateHITTypeOfHITResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateHITTypeOfHIT' smart constructor.
data UpdateHITTypeOfHIT = UpdateHITTypeOfHIT'
  { -- | The HIT to update.
    hITId :: Core.Text,
    -- | The ID of the new HIT type.
    hITTypeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateHITTypeOfHIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITId', 'updateHITTypeOfHIT_hITId' - The HIT to update.
--
-- 'hITTypeId', 'updateHITTypeOfHIT_hITTypeId' - The ID of the new HIT type.
newUpdateHITTypeOfHIT ::
  -- | 'hITId'
  Core.Text ->
  -- | 'hITTypeId'
  Core.Text ->
  UpdateHITTypeOfHIT
newUpdateHITTypeOfHIT pHITId_ pHITTypeId_ =
  UpdateHITTypeOfHIT'
    { hITId = pHITId_,
      hITTypeId = pHITTypeId_
    }

-- | The HIT to update.
updateHITTypeOfHIT_hITId :: Lens.Lens' UpdateHITTypeOfHIT Core.Text
updateHITTypeOfHIT_hITId = Lens.lens (\UpdateHITTypeOfHIT' {hITId} -> hITId) (\s@UpdateHITTypeOfHIT' {} a -> s {hITId = a} :: UpdateHITTypeOfHIT)

-- | The ID of the new HIT type.
updateHITTypeOfHIT_hITTypeId :: Lens.Lens' UpdateHITTypeOfHIT Core.Text
updateHITTypeOfHIT_hITTypeId = Lens.lens (\UpdateHITTypeOfHIT' {hITTypeId} -> hITTypeId) (\s@UpdateHITTypeOfHIT' {} a -> s {hITTypeId = a} :: UpdateHITTypeOfHIT)

instance Core.AWSRequest UpdateHITTypeOfHIT where
  type
    AWSResponse UpdateHITTypeOfHIT =
      UpdateHITTypeOfHITResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateHITTypeOfHITResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateHITTypeOfHIT

instance Core.NFData UpdateHITTypeOfHIT

instance Core.ToHeaders UpdateHITTypeOfHIT where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.UpdateHITTypeOfHIT" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateHITTypeOfHIT where
  toJSON UpdateHITTypeOfHIT' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("HITId" Core..= hITId),
            Core.Just ("HITTypeId" Core..= hITTypeId)
          ]
      )

instance Core.ToPath UpdateHITTypeOfHIT where
  toPath = Core.const "/"

instance Core.ToQuery UpdateHITTypeOfHIT where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateHITTypeOfHITResponse' smart constructor.
data UpdateHITTypeOfHITResponse = UpdateHITTypeOfHITResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateHITTypeOfHITResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateHITTypeOfHITResponse_httpStatus' - The response's http status code.
newUpdateHITTypeOfHITResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateHITTypeOfHITResponse
newUpdateHITTypeOfHITResponse pHttpStatus_ =
  UpdateHITTypeOfHITResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateHITTypeOfHITResponse_httpStatus :: Lens.Lens' UpdateHITTypeOfHITResponse Core.Int
updateHITTypeOfHITResponse_httpStatus = Lens.lens (\UpdateHITTypeOfHITResponse' {httpStatus} -> httpStatus) (\s@UpdateHITTypeOfHITResponse' {} a -> s {httpStatus = a} :: UpdateHITTypeOfHITResponse)

instance Core.NFData UpdateHITTypeOfHITResponse
